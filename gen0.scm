;;--------------------------------------------------------------
;; gen0 : compiler front end  (see gen.scm)
;;--------------------------------------------------------------

(require "core.scm")
(require "parse.scm")
(require "escape.scm")
(require "gen.scm")


(define *warn-upvals*
  (findstring "U" (value "SCAM_DEBUG")))


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
  (append-for form (wordlist 2 (scan-flags args 2 1) args)
              (case form
                ((PSymbol n name) name))))


;; Return forms in FORMS following flags, where flags begin at the second element.
;;
(define (skip-flags args)
  &public
  (nth-rest (1+ (scan-flags args 2 1)) args))


;; Return A-B dots.  Assumes A >= B.
;;
(define `(ups-sub a b)
    (patsubst (concat b "%") "%" a))


;; Return A+B-C dots.  Assumes A+B >= C.
;;
(define `(ups-add-sub a b c)
    (patsubst (concat c "%") "%" (concat a b)))


;; Construct a string describing the number of parameters:
;;   "a b c"      ==>   "3"
;;   "a ?b ?c"    ==>   "1 or 2 or 3"
;;   "a ?b ?c ..." ==>  "1 or more"
;;
(define (get-arity args)
  (if (filter "...% ?%" (lastword args))
      (if (filter "...%" (lastword args))
          (concat (words (filter-out "...% ?%" args)) "+")
          (concat (get-arity (butlast args)) " " (words args)))
      (words args)))


;; Generate an error if a non-optional parameter follows an optional one.
;;
(define (check-args args ?seen-optional)
  (define `a
    (first args))
  (if args
      (if (filter "...% ?%" (symbol-name a))
          (or (if (and (rest args)
                       (filter "...%" (symbol-name a)))
                  (gen-error a "'...' argument not in last position"))
              (check-args (rest args) 1))
          (if seen-optional
              (gen-error a "non-optional parameter after optional one")
              (check-args (rest args) nil)))))


;; Translate IL record NODE to a new nesting depth, and replace arguments.
;;
;; NODE = an IL record
;; OUT = minimum value for UPS to indicate a capture
;; DEEPER = amount to increment UPS (*after* removing one layer)
;; ARGS = If non-nil, replace macro arguments (where UPS+1 = OUT) with
;;    corresponding elements of ARGS.
;;
;; IArg nodes inside NODE can be one of the following:
;;  1. UPS >= OUT: A capture (to be translated)
;;  2. UPS < OUT:
;;      a) If ARGS and UPS == OUT-1, a macro arg (to be replaced).
;;      b) Otherwise, an internal arg (to be unchanged).
;;
;; Translation involves adjusting the UPS member of IArg nodes for an
;; increase or decrease in the lamda nesting level.  When we replace an
;; argument reference with an actual argument's IL, we likewise translate
;; *that* IL to the new (possibly greater) nesting depth.
;;
(define (xlat node out deeper args pos)
  (define `(x* nodes)
    (for n nodes (xlat n out deeper args pos)))

  (define `(xarg n ups node)
    (cond
     ;; translate capture?
     ((findstring out ups)
      (IArg n (ups-add-sub deeper ups ".")))

     ;; replace macro arg?
     ((and args (filter out (concat ups ".")))
      (xlat (if (findstring "+" n)
                (il-vector (nth-rest (subst "+" nil n) args))
                (nth n args))
            "." (patsubst ".%" "%" out) nil nil))

     ;; interior capture
     (else node)))

  (case node
    ((IString s) node)
    ((IBuiltin name args) (IBuiltin name (x* args)))
    ((ICall name args) (ICall name (x* args)))
    ((IArg n ups) (xarg n ups node))
    ((IFuncall nodes) (IFuncall (x* nodes)))
    ((IConcat nodes) (IConcat (x* nodes)))
    ((IBlock nodes) (IBlock (x* nodes)))
    ((ILambda body) (ILambda (xlat body (concat "." out) deeper args pos)))
    ((IWhere p) (if pos (IWhere pos) node))
    (else node)))


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
    ((PList n subforms) (c0-L env n (first subforms) (rest subforms)
                              (resolve (first subforms) env)))
    ((PString n value) (IString value))
    ((PSymbol n value) (c0-S env form value (resolve form env)))
    ((PDict n pairs) (c0-D env n pairs))
    ((PQuote n subform) (IString subform))
    ((PQQuote n subform) (c0-qq env subform))
    (else (c0-error form))))


;;--------------------------------
;; Symbol: local variable
;;--------------------------------


;; Return IL for a local variable reference.
;;
;; ARGN = argument index
;; DEPTH = argument depth ("." = top-most lambda)
;; AT-DEPTH = current depth (what ups="." refers to)
;; SYM = symbol form for the local variable reference
;;
(define (c0-local argn depth at-depth sym)
  (if (and *warn-upvals*
           (findstring (concat "." depth) at-depth))
      (info (describe-error
             (gen-error sym "reference to upvalue `%s`" (symbol-name sym))
             (pdec *compile-subject*)
             *compile-file*)))

  (IArg argn (concat "." (patsubst (concat depth "%") "%" at-depth))))


;;--------------------------------
;; Symbol: macro name
;;--------------------------------


;; Return "value" of a compound macro:  (lambda (a b c...) (MACRO a b c...))
;;
;; OUT = ".." for a compound macro, "." for a symbol macro
;; DEPTH = level at which the definition appeared; any captures are at
;;     this level or higher.
;;
;; For compoune macros, the (ILambda ...) wrapper is missing, so OUT="..".
;;
(define (c0-macro out at-depth depth il sym)
  (xlat il out (ups-add-sub at-depth "." depth) nil (form-index sym)))


;;--------------------------------
;; Symbol: builtin name
;;--------------------------------


(define (c0-builtin env name arity)
  (define `max-argc
    (word 1 (filter "3 2 1" arity)))

  (ILambda
   (if max-argc
       (IBuiltin name (for n (wordlist 1 max-argc "1 2 3")
                          (IArg n ".")))
       (ICall "^na" [ (IString name) (IVar "^av") ]))))


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
    ((ELocal argn depth)
     (c0-local argn depth (current-depth env) sym))

    ((EVar gname _)
     (IVar gname))

    ((EMacro depth scope _ il)
     (ILambda (c0-macro ".." (current-depth env) depth il sym)))

    ((EFunc gname _ _)
     (IBuiltin "value" [(IString gname)]))

    ((EIL depth _ il)
     (c0-macro "." (current-depth env) depth il sym))

    ((ERecord encs _ tag)
     (c0-ctor env sym encs))

    ((EBuiltin name _ arity)
     (c0-builtin env name arity))

    (else (c0-S-error sym defn))))


;;--------------------------------
;; { KEY: VALUE, ...}
;;--------------------------------

;; Return IL for a dictionary key.
;;
(define (c0-dict-key form env)
  (let ((node (il-demote (c0 form env))))
    (or (case node
          ((ICall name args)
           (if (eq? name "^d")
               (ICall "^k" args))))
        (il-subst "%" "!8" node))))


(define (c0-D env n pairs)
  (define `il-pairs
    (foreach
     pair pairs
     (define `key (dict-key pair))
     (define `value (dict-value pair))
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
     (foreach n (indices encodings)
              (begin
                (define `enc (word n encodings))
                (define `arg (nth n args))
                (define `value (c0 arg env))
                (define `field (if (filter "S" enc)
                                   (il-demote value)
                                   value))

                (append [(IString " ")] [field])))))))


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
    (concat results " " [o]))

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
;; Special forms are implemented in functions that begin with "ml.special-".
;;
;; (ml.special-XXX ENV SYM ARGS) -> RESULT
;;
;; SYM = a (PSymbol ...) record
;; ARGS = forms following SYM in `(SYM ...)` invocation of the special form
;; RESULT = a single IL node
;;
(define `(special-form-func name)
  (declare (ml.special-))
  (concat (native-name ml.special-) name))


;; Compile a vector of expressions independently, as in an argument list.
;; Bindings established by one expression do not apply to subsequent ones.
;; Note: This may evaluate ENV more than once.
;;
(define `(c0-vec forms env)
  &public
  (for f forms (c0 f env)))


;; NODE = compiled lambda body
;; FROM-DEPTH = depth at which macro was defined.  The body of the macro
;;              is actually nested one level deeper.
;; TO-DEPTH = lambda nesting level (unary) where macro is being expanded.
;;
(define (expand-macro node depth to-depth args sym)
  (xlat node
        ".."
        (ups-sub to-depth depth)
        (or args [(IString "")])  ;; ensure ARGS is non-nil
        (form-index sym)))


;; DEFN = (resolve (first subforms) env), which is either:
;;     * "-" if op is not a symbol
;;     * nil if op is an unbound symbol
;;     * an env record if op is a defined symbol
;;
(define (c0-L env pos sym args defn)
  (define `symname (symbol-name sym))

  (case defn
    ((EFunc realname _ arity)
     (or (check-arity arity args sym)
         (ICall realname (c0-vec args env))))

    ((EMacro depth _ arity il)
     (or (check-arity arity args sym)
         (expand-macro il depth (current-depth env) (c0-vec args env) sym)))

    ((EBuiltin realname _ arity)
     (or (check-arity arity args sym)
         (IBuiltin realname (c0-vec args env))))

    ((EXMacro name scope)
     (if (eq? scope "x")
         (gen-error sym "cannot use xmacro in its own file")
         (c0 (call name args) env)))

    ((ERecord encodings _ tag)
     (c0-record env sym args encodings tag))

    (else
     (cond
      ;; Empty list
      ((not sym)
       (gen-error pos "missing function/macro name"))

      ;; Defined symbol OR non-symbol => evaluate and call
      (defn
        (IFuncall (c0-vec (cons sym args) env)))

      ;; Macro/special form.
      ((bound? (special-form-func symname))
       (call (special-form-func symname) env sym args))

      ;; none of the above
      (else
       (gen-error sym "undefined symbol: `%s`" symname))))))


;;--------------------------------
;; (lambda (...SYMS) ...EXPRS)
;;--------------------------------


(define `(arg-defn name n depth)
  (if (filter "...%" name)
      ;; "...FOO" or "..."
      { (or (patsubst "...%" "%" name) name): (ELocal (concat n "+") depth) }
      ;; "?FOO" or "FOO"
      { (patsubst "?%" "%" name): (ELocal n depth) }))


;; Construct bindings for macro/function parameters
;;
(define (arg-locals syms n depth)
  (if syms
      (append (foreach name (symbol-name (first syms))
                       (arg-defn name n depth))
              (arg-locals (rest syms) (1+ n) depth))))


(define (c0-lambda env args body)
  (define `(arg-env env syms)
    (foreach depth (concat "." (current-depth env))
             (append (lambda-marker depth)
                     (arg-locals syms 1 depth)
                     env)))

  (or (check-args args)
      (ILambda (c0-block body (arg-env env args)))))


(define (lambda-error type form parent desc)
  (err-expected type form parent desc "(lambda (ARGNAME...) BODY)"))


;; special form: (lambda ARGS BODY)
(define (ml.special-lambda env sym args)
  (define `arglist (first args))
  (define `body (rest args))

  (case arglist
    ((PList pos lambda-args)
     (or (vec-or (for a lambda-args
                      (case a
                        ((PSymbol n name) nil)
                        (else (lambda-error "S" a sym "ARGNAME")))))
         (c0-lambda env lambda-args body)))
    (else (lambda-error "L" arglist sym "(ARGNAME...)"))))


;;--------------------------------
;; (declare ...)
;; (define ...)
;;--------------------------------


(define (c0-check-body where first-form is-define)
  ;; validate BODY
  (if is-define
      (if (not first-form)
          (gen-error where "no BODY supplied to (define FORM BODY)"))
      (if first-form
          (gen-error first-form "too many arguments to (declare ...)"))))


;; Return a vector of all PError nodes contained in node (including possibly
;; node itself).
;;
(define (il-errors node)
  (define `(r* nodes)
    (append-for node nodes (il-errors node)))
  (case node
    ((PError _ _) [node])
    ((IBuiltin _ args) (r* args))
    ((ICall _ args) (r* args))
    ((IFuncall nodes) (r* nodes))
    ((IConcat nodes) (r* nodes))
    ((IBlock nodes) (r* nodes))
    ((ILambda node) (il-errors node))))


;; If `node` contains any PError nodes, return a node that contains only them.
;; Otherwise, return nil.
;;
(define (il-error-node node)
  (let ((errors (il-errors node)))
    (if errors
        (if (word 2 errors)
            (IBlock errors)
            (first errors)))))


;;--------------------------------
;; (define `NAME FLAGS... BODY)
;; (declare NAME FLAGS...)
;; (define  NAME FLAGS... BODY)
;;--------------------------------


(define (c0-def-symbol env n name flags body is-define is-macro)
  (or (c0-check-body n (first body) is-define)

      (let ((value (c0-block body env))
            (scope (if (filter "&public" flags) "x" "p"))
            (gname (gen-native-name name flags))
            (depth (current-depth env))
            (is-define is-define)
            (is-macro is-macro))

        (define `env-out
          { =name: (if is-macro
                       (EIL depth scope value)
                       (EVar gname scope)) })

        (or (il-error-node value)
            (IEnv env-out
                  (and is-define
                       (not is-macro)
                       (ICall "^set" [ (IString gname) value ])))))))


;;--------------------------------
;; (define `(NAME ARGS...) FLAGS BODY)
;; (declare (NAME ARGS...) FLAGS)
;; (define  (NAME ARGS...) FLAGS BODY)
;;--------------------------------


(define (c0-def-compound env n name args flags body is-define is-macro)
  (define `arity (get-arity (for a args (symbol-name a))))
  (define `gname (gen-native-name name flags))
  (define `scope (if (filter "&public" flags) "x" "p"))

  ;; Make function name known *within* the body, unless it is a macro.
  (define `body-env
    (if is-macro
        env
        (append { =name: (EFunc gname scope arity) } env)))

  (or (c0-check-body n (first body) is-define)

      ;; compile function/macro body
      (let ((body-il (c0-lambda body-env args body))
            (depth (current-depth env))
            (is-define is-define)
            (is-macro is-macro)
            (arity arity)
            (scope scope)
            (name name)
            (gname gname))

        (define `defn
          (if is-macro
              (EMacro depth scope arity (case body-il ((ILambda node) node)))
              (EFunc gname scope arity)))

        (or (il-error-node body-il)
            (IEnv {=name: defn}
                  (and is-define
                       (not is-macro)
                       (ICall "^fset" [(IString gname) body-il])))))))


;; Dispatch to c0-def-symbol or c0-def-compound
;;
(define (c0-def2 env pos what flags body is-define is-macro)
  (define `def-or-decl
    (if is-define "define" "declare"))
  (define `macro-tick
    (if is-macro "`"))

  (or
   ;; check IS-MACRO
   (if (not is-macro)
       (case what
         ((PQQuote n subform)
          (c0-def2 env n subform flags body is-define 1))))

   ;; get NAME, ARGS
   (case what
     ((PSymbol n name)
      (c0-def-symbol env n name flags body is-define is-macro))

     ((PList list-n forms)
      (case (first forms)
        ((PSymbol sym-n name)
         (c0-def-compound env list-n name (rest forms) flags body
                          is-define is-macro))

        (name-form
         ;; "missing/invalid NAME in (define/declare (NAME) ..."
         (err-expected "S" name-form what "NAME" "(%s %s(NAME...))"
                       def-or-decl macro-tick))))

     (else
      ;; "missing/invalid FORM in ..."
      (err-expected "L S" what pos "FORM" "(%s %sFORM ...)"
                    def-or-decl macro-tick)))))


;; Break args into WHAT, FLAGS, and BODY.  Handle INBLOCK.
;;
(define `(c0-def env sym args is-define)
  (c0-def2 env (form-index sym) (first args)
           (get-flags args) (skip-flags args)
           is-define nil))

(define (ml.special-define env sym args)
  (c0-def env sym args 1))

(define (ml.special-declare env sym args)
  (c0-def env sym args nil))


;; This is supplied by modules that are not strict dependencies, in order to
;; avoid sprawling/circular dependencies.
(declare (get-module name base private) &public)

(data Mod
  &public
  ;; ID = string to be passed to ^R
  ;; ENV = exported environment entries
  (ModSuccess &word id &list env)
  (ModError message))


;;--------------------------------
;; Symbol: record constructor name
;;--------------------------------


;; Return the value of a constructor name: an equivalent lambda.
;;
(define (c0-ctor env sym encs)
  (define `args
    (for i (indices encs)
         (PSymbol 0 (concat "a" i))))
  (c0-lambda env args [ (PList 0 (cons sym args)) ]))


;;--------------------------------
;; (require STRING [&private])
;;--------------------------------


;; Generate IL nodes including a call to ^R and a "require" crumb to track
;; the module dependency, and wrap it in an IEnv that exports the new
;; symbols.
;;
;; Result = IL node that calls REQUIRE + includes a "require crumb"
;;
(define (ml.special-require env sym args)
  (define `module (first args))
  (define `flags (get-flags args))
  (define `body (skip-flags args))
  (define `mod-name (string-value module))
  (define `read-priv (filter "&private" flags))

  (or
   (if body
       (gen-error (first body) "too many arguments to require"))

   (case module
     ((PString _ name)
      (let ((o (get-module name *compile-file* read-priv))
            (module module))
        (case o
          ((ModError message)
           (gen-error module "require: %s" message))
          ((ModSuccess id exports)
           (define `arg (IConcat [(IString id) (ICrumb "require" id)]))
           (IEnv exports (ICall "^R" [arg]))))))
     (else
      (err-expected "Q" module sym "STRING" "(require STRING)")))))


;;--------------------------------
;; (begin STRING [&private])
;;--------------------------------


(define (ml.special-begin env sym args)
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
                            (for a (split from-str template)
                                 (IString a)))))

  (if (findstring QQS template)
      (replace QQS sub)
      (if (findstring [QQS] template)
          (replace [QQS] (il-demote sub))
          (PError 0 (concat "c0-qq-form: template='" template "'")))))


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
       (for c children
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
