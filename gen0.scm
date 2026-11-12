;;--------------------------------------------------------------
;; gen0 : compiler front end  (see gen.scm)
;;--------------------------------------------------------------

(require "core.scm")
(require "parse.scm")
(require "gen.scm")


;; Find the index of last flag in FORMS, starting at N.  Return PREV-N is no
;; flags are found.
;;
(define (scan-flags forms n prev-n)
  (or (case (nth n forms)
        ((PSymbol pos name)
         (if (filter "&private &public &native" name)
             (scan-flags forms (1+ n) n))))
      prev-n))


;; Return flags from FORMS, where flags start at the second element.
;;
(define (get-flags args)
  &public
  (append-for (form (wordlist 2 (scan-flags args 2 1) args))
    (case form
      ((PSymbol n name) name))))


;; Return forms in FORMS following flags. Flags begin at the second element.
;;
(define (skip-flags args)
  &public
  (nth-rest (1+ (scan-flags args 2 1)) args))


;; Remove B from the initial part of A.
;;
;; len(Result) = len(A) - len(B), when len(A) <= len(B).
;;
(define `(depth-sub a b)
    (patsubst (.. b "%") "%" a))


;; Return A+B-C.  Assumes A+B >= C.
;;
(define `(depth-add-sub a b c)
    (patsubst (.. c "%") "%" (.. a b)))


;; Construct a string describing the number of parameters:
;;   "a b c"      ==>   "3"
;;   "a ?b ?c"    ==>   "1 or 2 or 3"
;;   "a ?b ?c ..." ==>  "1 or more"
;;
(define (get-arity args)
  (if (filter "...% ?%" (lastword args))
      (if (filter "...%" (lastword args))
          (.. (words (filter-out "...% ?%" args)) "+")
          (.. (get-arity (butlast args)) " " (words args)))
      (words args)))


;; Translate function argument references, automatic variable definitions
;; and references, and replace macro arguments.
;;
;; NODE = an IL record to translate
;; TOP = distance from NODE to the "top" level of the macro (the top is the
;;     level at which arguments are to be replaced, and the level at which
;;     auto variables need to be adjusted) in the form of the UPS value that
;;     would refer to the TOP level if NODE were an IArg
;; AD = the "new" auto-depth of NODE (number of enclosing IFor's *within*
;;     the nearest enclosing ILambda, where NODE will be instantiated)
;; OLD-AD = auto depth where the macro was defined
;; NEW-AD = auto depth where the macro is being instantiated
;; ARGS = arguments passed to macro
;; SHIFT = number of UPS to add to captures, plus 1
;;
(define (xlat node top ad old-ad new-ad args shift pos)
  (define `(recur r-node r-top r-ad)
    (xlat r-node r-top r-ad old-ad new-ad args shift pos))

  ;; Shift an auto var's name to adjust for change in auto nesting level at
  ;; top of macro
  (define `(auto-shift name)
    (depth-add-sub name new-ad old-ad))

  ;; Translate a vector of nodes
  (define `(x* nodes)
    (for (n nodes)
      (recur n top ad)))

  ;; IFor: track foreach nesting (at this lambda level)
  (define `(xfor name list body)
    (define `new-name
      (if (filter "." top)
          (auto-shift name)
          name))
    (IFor new-name (recur list top ad) (recur body top (.. ad ";"))))

  ;; IArg: translate lambda argument or auto variable
  (define `(xarg name ups node)
    (cond
     ;; Macro arg => replace with args[ndx]
     ((and (filter top ups) args (filter-out ";%" name))
      (define `arg-node
        (if (findstring "+" name)
            (il-vector (nth-rest (subst "+" nil name) args))
            (nth name args)))
      (xlat arg-node "." ad new-ad ad nil ups pos))

     ;; Top-level auto within the macro => rename auto
     ((and (filter top ups) (findstring (.. old-ad ";") name))
      (IArg (auto-shift name) ups))

     ;; Capture => shift
     ;;   UPS>=TOP and not MACRO-ARG and not RENAMED-AUTO
     ((findstring top ups)
      (IArg name (depth-add-sub ups shift ".")))

     (else
      node)))

  (case node
    ((IString s) node)
    ((IBuiltin name args) (IBuiltin name (x* args)))
    ((ICallX name args xv) (ICallX name (x* args) (x* xv)))
    ((IArg n ups) (xarg n ups node))
    ((IConcat nodes) (IConcat (x* nodes)))
    ((IBlock nodes) (IBlock (x* nodes)))
    ((IFor name list body) (xfor name list body))
    ((ILambda stem body) (ILambda stem (recur body (.. "." top) nil)))
    ((IWhere p) (if p (IWhere pos) node))
    (else node)))


;; A substring that identifies an IWhere anywhere within an IL record.
;;
(define IWhere-sig
  (word 1 (subst "!" nil (IWhere nil))))


;; OLD-DEPTH = depth at which NODE was compiled
;; NEW-DEPTH = depth to which NODE is being translated
;; ARGS = for symbol macros, nil.  For compound macros, a *non-nil* vector
;;     of arguments to the macro: IL nodes compiled at NEW-DEPTH.
;; POS = parsing token index of location of instantiation
;;
(define (translate node old-depth new-depth args pos)
  (define `old-ad (depth.a old-depth))
  (define `new-ad (depth.a new-depth))
  (define `shift (depth-add-sub (depth.l new-depth) "." (depth.l old-depth)))

  ;; if no-args and old-depth=new-depth and no-IWheres: return node
  (if (or args
          (not (eq? old-depth new-depth))
          (findstring IWhere-sig node))
      ;; translate
      (xlat node "." new-ad old-ad new-ad args shift pos)
      ;; no word required
      node))


;;--------------------------------
;; c0
;;--------------------------------


(declare (c0-L env pos sym args defn))
(declare (c0-S env sym name defn))
(declare (c0-D env n pairs))
(declare (c0-qq env form))


(define (c0-error form)
  (define `msg
    (case form
      ((PUnquote n sub) "unquote (,) outside of a quasiquoted (`) form")
      ((PSplice n sub) "splice (,@) outside of a quasiquoted (`) form")
      (else "bad AST node: %q")))
  (case form
    ((PError n code) form)
    (else (gen-error form msg form))))


;; Compile an expression.  Return IL.
;;
(define (c0 form env)
  &public
  (case form
    ((PList n [sel ...args]) (c0-L env n sel args (resolve sel env)))
    ((PString n value) (IString value))
    ((PSymbol n value) (c0-S env form value (resolve form env)))
    ((PDict n pairs) (c0-D env n pairs))
    ((PVec n forms) (il-vector (for (f forms) (c0 f env))))
    ((PQuote n subform) (IString subform))
    ((PQQuote n subform) (c0-qq env subform))
    (else (c0-error form))))


;;--------------------------------
;; Symbol: macro name
;;--------------------------------


;; Generate a lambda value from a macro definition.  This is the value
;; that is produced when the macro name is used in a context other than
;; the first form in compound form.
;;
(define (c0-macro depth node env sym)
  (translate (ILambda nil node) (depth-sub depth ".") (current-depth env)
             nil (form-index sym)))


;;--------------------------------
;; Symbol: builtin name
;;--------------------------------


(define (c0-builtin sym name arity)
  (if (or (filter "%+" arity)
          (eq? "if" name))
      (gen-error sym "builtin `%s` does not have a value" name)
      (ILambda
       nil
       (IBuiltin name (for (n (wordlist 1 arity "1 2 3"))
                        (IArg n "."))))))


(define (c0-S-error sym defn)
  (if defn
      (gen-error sym "internal: %s binds to %q" sym defn)
      (gen-error sym "undefined variable: `%s`" (symbol-name sym))))


;;--------------------------------
;; Symbol
;;--------------------------------

(declare (c0-ctor env sym encs))


;; DEFN = definition bound to symbol
;;
(define (c0-S env sym name defn)
  (case defn
    ((EIL _ depth il)
     (translate il depth (current-depth env) nil (form-index sym)))

    ((EVar _ gname)
     (IVar gname))

    ((EMacro _ depth _ il)
     (c0-macro depth il env sym))

    ((EFunc _ _ gname)
     (IBuiltin "value" [(IString gname)]))

    ((ERecord _ encs tag)
     (c0-ctor env sym encs))

    ((EBuiltin _ bname arity)
     (c0-builtin sym bname arity))

    (else (c0-S-error sym defn))))


;;--------------------------------
;; { KEY: VALUE, ...}
;;--------------------------------

;; Return IL for a dictionary key.
;;
(define (c0-dict-key form env)
  (case (il-demote (c0 form env))
    ((ICallX name args _)
     (if (eq? name "^d")
         (ICall "^k" args)
         (ICall name args)))
    (node
     (subst-in-il "%" "!8" node))))


(define (c0-D env n pairs)
  (define `il-pairs
    (foreach ({=key: value} pairs)
      (define `key-node
        (case key
          ;; {symbol: ...} is treated as {"symbol": ...}
          ;; Note: symbols cannot contain "%"
          ((PSymbol n name)
           (if (filter "=%" name)
               (c0-dict-key (PSymbol n (patsubst "=%" "%" name)) env)
               (IString (demote name))))
          (else
           (c0-dict-key key env))))
      (define `value-node
        (il-demote (c0 value env)))

      [(il-concat [ key-node (IString "!=") value-node ])]))

  (il-concat (intersperse (IString " ") il-pairs)))


;;--------------------------------
;; (CTOR ...ARGS)
;;--------------------------------


;; form = `(Ctor ARG...)
(define (c0-record env sym args encodings tag)
  (or
   (check-arity (words encodings) args sym)

   (IConcat
    (cons
     (IString tag)
     (foreach (n (indices encodings))
       (define `enc (word n encodings))
       (define `arg (nth n args))
       (define `value (c0 arg env))
       (define `field (if (filter "S" enc)
                          (il-demote value)
                          value))

       (append [(IString " ")] [field]))))))

;;--------------------------------
;; c0-block
;;--------------------------------

;; Compile a vector of forms, calling `k` with results.
;;
;; FORMS = vector of forms
;; K = fuction to call: (k new-env nodes)
;; ENV = current environment
;; RESULTS = results (not including previous compiled form)
;; O = result of compiling previous form EXCEPT the first time
;;     this function is called, when it is nil.
;;
(define (c0-block-cc env forms k ?results ?o)
  (define `new-results
    (._. results [o]))

  (case o
    ((IEnv bindings node)
     (c0-block-cc (append bindings env) forms k results node))

    (else
     (if forms
         (c0-block-cc env (rest forms) k new-results
                      (c0 (first forms) env))
         (k env (filter-out [nil] new-results))))))


;; Compile a vector of forms as a block (each expression may modiy
;; the environment for subsequent expressions). Return a single IL node.
;;
(define (c0-block forms env)
  &public
  (c0-block-cc env
               forms
               (lambda (env results)
                 (if (word 2 results)
                     (IBlock results)
                     (first results)))))


;;--------------------------------
;; ( ...FORMS )    [compound expressions]
;;--------------------------------


;; Return a special form function native name given the symbol-name.
;;
;; Special forms are implemented in functions that begin with "M.".
;;
;; (M.XXX ENV SYM ARGS) -> RESULT
;;
;; SYM = a (PSymbol ...) record
;; ARGS = forms following SYM in `(SYM ...)` invocation of the special form
;; RESULT = a single IL node
;;
(define `(special-form-func name)
  (declare (M.))
  (.. (native-name M.) name))


;; Compile a vector of expressions independently, as in an argument list.
;; Bindings established by one expression do not apply to subsequent ones.
;; Note: This may evaluate ENV more than once.
;;
(define `(c0-vec forms env)
  &public
  (for (f forms) (c0 f env)))


;; DEFN = (resolve (first subforms) env), which is either:
;;     * "-" if op is not a symbol
;;     * nil if op is an unbound symbol
;;     * an env record if op is a defined symbol
;;
(define (c0-L env pos sym args defn)
  (define `symname (symbol-name sym))

  (case defn
    ((EFunc _ arity realname)
     (or (check-arity arity args sym)
         (ICall realname (c0-vec args env))))

    ((EMacro _ depth arity il)
     (or (check-arity arity args sym)
         (translate il depth (current-depth env)
                    (or (c0-vec args env)
                        [(IString "")])  ;; ensure ARGS is non-nil
                    (form-index sym))))

    ((EBuiltin _ realname arity)
     (or (check-arity arity args sym)
         (IBuiltin realname (c0-vec args env))))

    ((EXMacro scope name)
     (if (eq? scope "x")
         (gen-error sym "cannot use xmacro in its own file")
         (c0 (native-call name args pos) env)))

    ((ERecord _ encodings tag)
     (c0-record env sym args encodings tag))

    (else
     (cond
      ;; Empty list
      ((not sym)
       (gen-error pos "missing function/macro name"))

      ;; Defined symbol OR non-symbol => evaluate and call
      (defn
        (IFuncall (c0 sym env) (c0-vec args env)))

      ;; Special form
      ((native-bound? (special-form-func symname))
       (native-call (special-form-func symname) env sym args))

      ;; none of the above
      (else
       (gen-error sym "undefined symbol: `%s`" symname))))))


;;--------------------------------
;; (lambda (...SYMS) ...EXPRS)
;;--------------------------------


;; Construct environment bindings for a function parameter list.  The first
;; parameter is bound to (IArg 1 "."), the second to (IArg 2 "."), and so
;; on.  Parameters may be symbols or more complex targets.
;;
;; Syntax errors encountered during parsing parameters are returned as
;; bindings with a variable name of EnvErrorKey.
;;
(define (bind-params depth forms ?not-at-end)
  ;; Handle `...NAME` at the final position, preceded by zero or more `?NAME`.
  (define `variadic-case
    (case (last forms)
      ((PSymbol _ sym-name)
       (foreach (name (filter (if not-at-end "?%" "...% ?%") sym-name))

         (define `var-name
           (or (patsubst (if (filter "?%" name) "?%" "...%") "%" name)
               name))

         (define `var-index
           (.. (words forms) (if (filter "...%" name) "+")))

         (append (bind-params depth (butlast forms) 1)
                 { =var-name: (EIL "p" depth (IArg var-index ".")) })))))

  (if forms
      (or variadic-case
          ;; Process all non-optional arguments
          (foreach (n (indices forms))
            (bind-target (nth n forms) "p" depth (IArg n "."))))))


(define (c0-lambda env params body ?stem)
  &public
  (foreach (depth (depth-down env))
    (let ((bindings (bind-params depth params))
          (depth depth)
          (env env)
          (body body))

      (define `body-env
        (append (depth-marker depth) bindings env))

      (or (dict-get EnvErrorKey bindings)
          (ILambda stem (c0-block body body-env))))))


;; special form: (lambda ARGS BODY)
(define (M.lambda env sym [args-form ...body])
  (case args-form
    ((PList pos params)
     (c0-lambda env params body))
    (else (err-expected "L" args-form sym
                        "(ARGNAME...)" "(lambda (ARGNAME...) BODY)"))))


;;--------------------------------
;; (declare ...)
;; (define ...)
;;--------------------------------


(define (c0-check-body where first-form def-mode)
  ;; validate BODY
  (if (filter "dec%" def-mode)
      (if first-form
          (gen-error first-form "too many arguments to (declare ...)"))
      (if (not first-form)
          (gen-error where "no BODY supplied to (define TARGET BODY)"))))


;; Return a vector of all PError nodes contained in node (including possibly
;; node itself).
;;
(define (il-errors node)
  (define `(r* nodes)
    (append-for (node nodes)
      (il-errors node)))
  (case node
    ((PError _ _) [node])
    ((IBuiltin _ args) (r* args))
    ((ICallX _ args xv) (append (r* args) (r* xv)))
    ((IFor _ list body) (append (il-errors list) (il-errors body)))
    ((IConcat nodes) (r* nodes))
    ((IBlock nodes) (r* nodes))
    ((ILambda _ node) (il-errors node))))


;; If `node` contains any PError nodes, return a node that contains only them.
;; Otherwise, return nil.
;;
(define (il-error-node node)
  (if (findstring (subst "!" nil (word 1 (PError nil nil)))
                  node)
      (let ((errors (il-errors node)))
        (if (word 2 errors)
            (IBlock errors)
            (first errors)))))


;;--------------------------------
;; (define `TARGET FLAGS... BODY)
;; (declare TARGET FLAGS...)
;; (define  TARGET FLAGS... BODY)
;;--------------------------------


;; Generate code to assign a global variable
;;
(define (assign-nx nx flags il)
  (case nx
    ((Bind name xtor)
     (ICall "^S" [ (IString (gen-native-name name flags)) (xtor il) ]))))


;; DEF-MODE = "define", "declare", or nil (macro)
;;
(define (c0-def-target-2 env nxmap flags value scope def-mode)
  ;; Bindings for global variables (names mentioned in target)
  (define `bindings
    (append-for (nx nxmap)
      (case nx
        ((Bind name xtor)
         {=name: (EVar scope (gen-native-name name flags))}))))

  (define `set-nodes
    (for (nx nxmap)
      (assign-nx nx flags (IArg 1 "."))))

  (define `assignment-code
    (if (word 2 nxmap)
        ;; Mutiple vars => (let ((v VALUE)) (^S NAME1 (XTOR1 v)) ...)
        (IFuncall (ILambda nil (IBlock set-nodes)) [value])
        ;; Single var => (^S NAME (XTOR v))))
        (assign-nx (first nxmap) flags value)))

  (or (first-perror nxmap)

      (il-error-node value)

      (if def-mode
          (IEnv bindings
                (if (filter "def%" def-mode)
                    assignment-code))
          (IEnv (bind-nxmap nxmap scope (current-depth env) value) nil))))


(define (c0-def-target env target flags body def-mode)
  (or (c0-check-body (form-index target) (first body) def-mode)
      (c0-def-target-2 env
                       (parse-target target)
                       flags
                       (c0-block body env)
                       (if (filter "&public" flags) "x" "p")
                       def-mode)))


;;--------------------------------
;; (define `(NAME PARAMS...) FLAGS BODY)
;; (declare (NAME PARAMS...) FLAGS)
;; (define  (NAME PARAMS...) FLAGS BODY)
;;--------------------------------

(define (c0-def-compound2 env n name args body gname def-mode scope arity)
  ;; Make function name known *within* the body, unless it is a macro.
  (define `body-env
    (if def-mode
        (append { =name: (EFunc scope arity gname) }
                { =EnvFuncKey: gname }
                env)
        env))

  (or (c0-check-body n (first body) def-mode)

      ;; compile function/macro body
      (let ((body-il (c0-lambda body-env args body))
            (body-depth (depth-down env))
            (def-mode def-mode)
            (arity arity)
            (scope scope)
            (name name)
            (gname gname))

        (define `defn
          (if def-mode
              (EFunc scope arity gname)
              (EMacro scope body-depth arity
                      (case body-il
                        ((ILambda _ node) node)))))

        (or (il-error-node body-il)
            (IEnv {=name: defn}
                  (and (filter "def%" def-mode)
                       (ICall "^SF" [(IString gname) body-il])))))))


(define `(c0-def-compound env n name args flags body def-mode)
  (define `gname
    (gen-native-name name flags))
  (define `scope
    (if (filter "&public" flags) "x" "p"))
  (define `arity
    (get-arity (for (a args) (symbol-name a))))

  (c0-def-compound2 env n name args body gname def-mode scope arity))


;; Dispatch to c0-def-symbol or c0-def-compound
;;
(define (c0-def2 env pos what flags body def-mode)
  (define `syntax
    (if def-mode
        (.. def-mode " ")
        "define `"))

  (or
   ;; check for macro
   (if (filter "def%" def-mode)
       (case what
         ((PQQuote n subform)
          (c0-def2 env n subform flags body nil))))

   ;; get NAME, PARAMS
   (case what
     ((PList list-n [name-form ...params])
      (case name-form
        ((PSymbol sym-n name)
         (c0-def-compound env list-n name params flags body def-mode))

        (name-form
         ;; "missing/invalid NAME in (define/declare (NAME) ..."
         (err-expected "S" name-form what "NAME" "(%s(NAME...))" syntax))))

     (else
      (if what
          (c0-def-target env what flags body def-mode)

          ;; "missing/invalid FORM in ..."
          (err-expected "L P" what pos "FORM" "(%sFORM ...)" syntax))))))


;; Return IL for a `define` or `declare` form.
;;
(define `(c0-def env sym args def-mode)
  (c0-def2 env (form-index sym) (first args)
           (get-flags args) (skip-flags args)
           def-mode))

(define (M.define env sym args)
  (c0-def env sym args "define"))

(define (M.declare env sym args)
  (c0-def env sym args "declare"))


;;--------------------------------
;; Symbol: record constructor name
;;--------------------------------


;; Return the value of a constructor name: an equivalent lambda.
;;
(define (c0-ctor env sym encs)
  (define `args
    (for (i (indices encs))
      (PSymbol 0 (.. "a" i))))
  (c0-lambda env args [ (PList 0 (cons sym args)) ]))


;;--------------------------------
;; (begin STRING [&private])
;;--------------------------------


(define (M.begin env sym args)
  (c0-block args env))


;;--------------------------------
;; `EXPR   [quasi-quoting]
;;--------------------------------


;; A quasi-quoted form evaluates (at run time) to a form value.
;; Quasi-quoted forms can contain un-quoted expressions, which are evaluated
;; at run-time (presumably to a valid form).
;;
;; Quasi-quoting code must make *some* assumptions about how data
;; constructors work.  In order to minimize those, we use the constructors
;; to encode a sentinel value, QQS, that represents where its child/children
;; will go.  We we can see whether the sentinel appears in the resulting
;; string in demoted or un-demoted form, so we can replace it appropriately
;; with the separately-quasiquoted child/children.

(declare (c0-qq))

(define `QQS
  "*!*")


;; TEMPLATE = form encoding QQS as a child
;; SUB = IL node describing child/children
;;
(define (c0-qq-form template sub)
  ;; Replace from-str with to-il, and convert the remainder of the string to
  ;; IString nodes.
  (define `(replace from-str to-il)
    (il-concat (intersperse to-il
                            (for (a (split from-str template))
                              (IString a)))))

  (if (findstring QQS template)
      (replace QQS sub)
      (if (findstring [QQS] template)
          (replace [QQS] (il-demote sub))
          (PError 0 (.. "c0-qq-form: template='" template "'")))))


;; Expand a single form within a quasiquoted expression.
;;
(define (c0-qq env form ?nest)
  (case form
    ((PUnquote n child)
     (if nest
         (c0-qq-form (PUnquote n QQS) (c0-qq env child (rest nest)))
         (c0 child env)))

    ((PQQuote n child)
     (c0-qq-form (PQQuote n QQS) (c0-qq env child (cons 1 nest))))

    ((PList n children)
     (define `il-children
       (for (c children)
         (case c
           ((PSplice n expr) (c0 expr env))
           (else (il-demote (c0-qq env c nest))))))

     (c0-qq-form (PList n QQS)
                 (il-concat (intersperse (IString " ") il-children))))

    ((PError n desc)
     form)

    (else
     (IString form))))


;; Compile a list of forms, returning [ENV-OUT ...NODES].
;;
(define (gen0 forms env)
  &public
  (c0-block-cc env forms (lambda (env-out nodes)
                           (cons env-out nodes))))
