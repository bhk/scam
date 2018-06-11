;;--------------------------------------------------------------
;; gen0 : compiler front end  (see gen.scm)
;;--------------------------------------------------------------

(require "core")
(require "parse")
(require "escape")
(require "gen")
(require "num")

(define *warn-upvals*
  (findstring "U" (value "SCAM_DEBUG")))

;; Return `FILE:LINE:COL` as determined by POS and current file & subject.
;;
(define (get-where pos)
  &public
  (define `lnum
    (get-subject-line pos *compile-subject*))
  (concat *compile-file* ":" lnum))


;; Env-modifying functions can return an environment when `inblock` is true.
;; Use this macro to generate the return value.
;;
(define `(block-result inblock env node)
  &public
  (if inblock
      (IEnv env node)
      node))


;; Extract sub-node from (IEnv ...) if INBLOCK is nil.
;;
(define (env-strip inblock node)
  &public
  (if inblock
      node
      (case node
        ((IEnv e subnode) subnode)
        (else node))))


;; scan-flags: find index of last flag (or 0 if no flags), after
;; skipping the first SKIP entries.

(declare (scan-flags-x args n prev-n))

(define `(scan-flags args skip)
  (scan-flags-x args (1+ skip) skip))

(define (scan-flags-x args n prev-n)
  (or (case (nth n args)
        ((PSymbol pos name)
         (if (filter "&private &public &inline &global" name)
             (scan-flags args n))))
      prev-n))


;; skip SKIP entries before looking for flags
(define (get-flags args skip)
  &public
  (append-for form (wordlist (1+ skip) (scan-flags args skip) args)
              (case form
                ((PSymbol n name) name))))

(define (skip-flags args skip)
  &public
  (nth-rest (1+ (scan-flags args skip)) args))


(define `(level-count level)
  (words (subst "." ". " level)))


;; Translate macsym body to a different (lower) context
;;
;; Captures (local variable references where ups >= TOP) will be adjusted.
;; Other local variable references will be left unchanged.
;;
(define (xlat-node node top deeper)
  (define `(x* nodes)
    (for n nodes (xlat-node n top deeper)))

  (case node
    ((IString s) node)
    ((IBuiltin name args) (IBuiltin name (x* args)))
    ((ICall name args) (ICall name (x* args)))
    ((ILocal ndx ups)
     (if (>= ups top)
         (ILocal ndx (+ ups deeper))
         node))
    ((IConcat nodes) (IConcat (x* nodes)))
    ((ILambda node)
     (ILambda (xlat-node node (1+ top) deeper)))
    ((IFuncall nodes) (IFuncall (x* nodes)))
    ((IBlock nodes) (IBlock (x* nodes)))
    (else node)))


;; Translate IWhere nodes to the source location where the macro is
;; expanded.
;;
(define (xlat-where node pos)
  (define `(x* nodes)
    (for n nodes (xlat-where n pos)))

  (case node
    ((IWhere s) (IWhere (get-where pos)))
    ((IBuiltin name args) (IBuiltin name (x* args)))
    ((ICall name args) (ICall name (x* args)))
    ((IConcat nodes) (IConcat (x* nodes)))
    ((ILambda node) (ILambda (xlat-where node pos)))
    ((IFuncall nodes) (IFuncall (x* nodes)))
    ((IBlock nodes) (IBlock (x* nodes)))
    (else node)))


;; FROM-LEVEL = level (unary) where macro was defined.
;; TO-LEVEL = level (unary) where macro is being expanded.
;;
(define (xlat-arg node from-level to-level)
  (define `(dots- a b)
    (subst (concat ":" b) "" (concat ":" a)))
  ;; If from-level == to-level, captures do not need to be translated.
  ;; If from-level==nil, there are no captures to translate.
  (if (filter-out to-level from-level)
      (xlat-node node 0 (level-count (dots- to-level from-level)))
      node))

;; This limits to 128 the levels of lambda nesting around an argument within
;; a macro.
(define (dots-from-num num)
  (wordlist 1 num (subst "." ". . . . . . . . "
                         "................")))


;; Translate a compound macro body to a new context.
;; Locals with ups==TOP are replaced with values from arg-values.
;; Locals with ups>TOP are adjusted by DEEPER.
;;
(define (xlat-macro node top deeper arg-values)
  (define `(x* nodes)
    (for n nodes (xlat-macro n top deeper arg-values)))

  (case node
    ((IString s) node)
    ((IBuiltin name args) (IBuiltin name (x* args)))
    ((ICall name args) (ICall name (x* args)))
    ((ILocal ndx ups) (cond
                       ;; macro arg
                       ((eq? ups top)
                        (xlat-arg (or (nth ndx arg-values) NoOp)
                                  "."
                                  (concat "." (dots-from-num top))))
                       ;; capture
                       ((> ups top) (ILocal ndx (+ ups deeper)))
                       ;; interior local
                       (else node)))
    ((IConcat nodes) (IConcat (x* nodes)))
    ((ILambda node)
     (ILambda (xlat-macro node (1+ top) deeper arg-values)))
    ((IFuncall nodes) (IFuncall (x* nodes)))
    ((IBlock nodes) (IBlock (x* nodes)))
    (else node)))


;; NODE = compiled lambda body
;; FROM-DEPTH = lambda nesting level (unary) where macro was defined.
;;    This refers to the nesting level outside the ILambda (which has
;;    been removed).  The body is actually nested one level deeper.
;; TO-DEPTH = lambda nesting level (unary) where macro is being expanded.
;;
(define (expand-macro node from-depth to-depth arg-values pos)
  (define `(dots- a b)
    (subst (concat ":" b) "" (concat ":" a)))
  ;; subtract 1 from the offset to account for extraction of lambda body
  (define `deeper
    (- (level-count (dots- to-depth from-depth)) 1))

  (xlat-macro (xlat-where node pos) 0 deeper arg-values))


;; Generate an error if a non-optional parameter follows an optional one.
;;
(define (check-optional-args args ?seen-optional)
  (define `a
    (first args))
  (if args
      (if (filter "...% ?%" (symbol-name a))
          (check-optional-args (rest args) 1)
          (if seen-optional
              (gen-error a "non-optional parameter after optional one")
              (check-optional-args (rest args) nil)))))


;;================================
;; c0 compilation
;;================================

(declare (c0 form env ?inblock))
(declare (c0-block forms env))
(declare (c0-lambda env args body))


;; c0-local: Construct a local variable reference.
;;
;; A unary counting system identifies levels of nesting of functions:
;;
;;    "." = outermost function
;;    ".." = first nesting level down
;;    "..." = third down
;;
;; ARG is a decimal index preceded by the level where it is bound (e.g. "..3").
;; MARKER is an EMarker describing the current nesting level.
;; FORM is used in the up-value warning form of the function.
;;
(define (c0-local arg depth sym)
  (define `ndx
    (subst "." "" arg))

  (if (and *warn-upvals*
           (not (findstring depth arg)))
          (compile-warn sym "reference to upvalue %q" (symbol-name sym)))
  (ILocal ndx
         (level-count (subst arg "" (concat depth ndx)))))


;; Return the "value" of a record constructor: an equivalent anonymous
;; function.
;;
(define (c0-ctor env sym encs)
  (define `args
    (for i (indices encs)
         (PSymbol 0 (concat "a" i))))
  (c0-lambda env args [ (PList 0 (cons sym args)) ]))


;; MACRO --> (lambda (a b c...) (MACRO a b c...)
;;
(define (c0-macro env inln)
  (define `depth-from (first inln))
  (define `macro-il (rest inln))

  ;; Adjust captures; do not replace macro args.
  (xlat-arg (ILambda macro-il) depth-from (current-depth env)))


(define (c0-builtin env name argc)
  (define `max-argc
    (firstword (filter "3 2 1" argc)))

  (ILambda
   (if max-argc
       (IBuiltin name (for n (wordlist 1 max-argc "1 2 3")
                          (ILocal n 0)))
       (ICall "^apply" [ (IString name) (IVar "^av") ]))))


(define (c0-S-error sym defn)
  (if defn
      (gen-error sym "internal: %q binds to %q" sym defn)
      (gen-error sym "undefined variable %q" (symbol-name sym))))


;; Symbol value
;;    DEFN = definition bound to symbol
(define (c0-S env sym name defn)
  (case defn
    ((EArg arg)
     (c0-local arg (current-depth env) sym))

    ((EVar gname _)
     (IVar gname))

    ((EFunc gname _ _ inln)
     (if (filter NoGlobalName gname)
         (c0-macro env inln)
         (IBuiltin "value" [(IString gname)])))

    ((EIL depth _ il)
     (xlat-arg (xlat-where il (form-index sym))
               depth (current-depth env)))

    ((ERecord encs _ tag)
     (c0-ctor env sym encs))

    ((EBuiltin name _ argc)
     (c0-builtin env name argc))

    (else (c0-S-error sym defn))))


;; Compile a vector of expressions independently, as in an argument
;; list.  Declarations and definitions on one expression do not apply to
;; subsequent ones (inlike c0-block-cc).
;;
(define (c0-vec forms env)
  &public
  (for f forms (c0 f env)))


;; Function call by name.
;;
;;   - Ordinary function:  inln is nil
;;   - Compound macro:     inln non-nil, REALNAME = NoGlobalName
;;   - Inline function:    inln non-nil, REALNAME = other
;;
(define (c0-call env sym args realname argc inln)
  (define `depth-from (first inln))
  (define `macro-il (rest inln))

  (or (check-argc argc args sym)
      (if inln
          ;; macro/inline
          (expand-macro macro-il
                        depth-from
                        (current-depth env)
                        (for a args (c0 a env nil))
                        (form-index sym))

          ;; non-inline
          (ICall realname (c0-vec args env)))))


;; Special forms are implemented in functions that begin with "ml.special-".
;;
(define `(special-form-func name)
  (declare (ml.special-))
  (concat (global-name ml.special-) name))


;; form = `(Ctor ARG...)
(define (c0-record env sym args encodings tag)
  (or
   (check-argc (words encodings) args sym)

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


;; L: compound forms
;;    defn = Result of (resolve op), which is either:
;;              a) "-" if op is not a symbol
;;              b) nil if op is an unbound symbol)
;;              b) an env record if op is a defined symbol
;;    inblock = if true, return: (IEnv new-env <node>)
;;              if nil, return:  <node>
;;
;(define (c0-L form env defn inblock)
(define (c0-L env pos sym args defn inblock)
  (define `symname (symbol-name sym))

  (case defn
    ((EFunc realname _ argc inln)
     (c0-call env sym args realname argc inln))

    ((EBuiltin realname _ argc)
     (or (check-argc argc args sym)
         (IBuiltin realname (c0-vec args env))))

    ((EXMacro name scope)
     (if (eq? scope "x")
         (gen-error sym "cannot use xmacro in its own file")
         (c0 (call name args) env inblock)))

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
       (call (special-form-func symname) env sym args inblock))

      ;; none of the above
      (else
       (gen-error sym "undefined symbol: %q" symname))))))


;;================================================================
;; lambda
;;================================================================

;; Construct a binding for an argument to a function
;;
(define `(lambda-arg sym single-value rest-value)
  (foreach name (symbol-name sym)
           (if (filter "...%" name)
               ;; "...X" => bind "X";  "..." => bind "..."
               { (or (patsubst "...%" "%" name) name): rest-value }
               { (patsubst "?%" "%" name): single-value })))


;; Add local variables to environment.
;;
;; level = string of "$" for a lambda marker
;;
(define (lambda-env-arg9 xargs level)
  (define `(macsym il)
    (EIL level "-" il))

  (define `(nth-value n)
    (macsym (IBuiltin "call" [(IString "^n") (IString n) (ILocal 9 0)])))

  (define `(nth-rest-value n)
    (if (eq? n 1)
        (EArg (concat level 9))
        (macsym
         (IBuiltin "wordlist" [(IString n) (IString 999999) (ILocal 9 0)]))))

  (foreach n (indices xargs) (lambda-arg (nth n xargs)
                                         (nth-value n)
                                         (nth-rest-value n))))


(define (lambda-env-args args level)
  (define `(nth-value n)
    (EArg (concat level n)))
  (define `(nth-rest-value n)
    (EIL "" "-" (IBuiltin "foreach" [(IString "N") (IString n) (IVar "^v")])))

  (append { =LambdaMarkerKey: (EMarker level) }
          ;; first 8 args = $1 ... $8
          (foreach n (indices (wordlist 1 8 args))
                   (lambda-arg (nth n args)
                               (nth-value n)
                               (nth-rest-value n)))
          ;; args 9 and up = (nth 1 $9), (nth 2 $9), ...
          (if (word 9 args)
              (lambda-env-arg9 (nth-rest 9 args) level))))


(define `(lambda-env args env)
  (append (lambda-env-args
           args
           (concat "." (current-depth env)))
          env))


(define (c0-lambda env args body)
  (or (check-optional-args args)
      (ILambda (c0-block body (lambda-env args env)))))


(define (lambda-error type form parent desc)
  (err-expected type form parent desc "(lambda (ARGNAME...) BODY)"))


;; special form: (lambda ARGS BODY)
(define (ml.special-lambda env sym args inblock)  ;;form env)
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

;;================================================================
;; declare/define
;;================================================================

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


;; Generate error is variable or function name conflicts with Make built-in
;; function of automatic variable.
;;
(define (check-name name n)
  (if (filter name builtin-names)
      (gen-error n "cannot redefine built-in function %q" name)
      ;; Use ":" (illegal name char) to represent "%"
      (if (filter "@ : < ? ^ + | *" (subst "%" ":" name) )
          (gen-error n "cannot redefine automatic variable '$%s'" name))))


;; (define `NAME FLAGS... BODY)
;; (declare NAME FLAGS...)
;; (define  NAME FLAGS... BODY)

(define (c0-def-symbol env n name flags body is-define is-macro)
  (or (if (filter "&inline" flags)
          (gen-error n "'&inline' does not apply to symbol definitions"))

      (c0-check-body n (first body) is-define)

      (if (not is-macro)
          (check-name name n))

      (let ((value (c0-block body env))
            (scope (if (filter "&public" flags) "x" "p"))
            (gname (gen-global-name name flags))
            (env env))

        (define `env-out
          (append { =name: (if is-macro
                               (EIL (current-depth env) scope value)
                               (EVar gname scope)) }
                  env))

        (or (il-error-node value)
            (IEnv env-out
                  (and is-define
                       (not is-macro)
                       (ICall "^set" [ (IString gname) value ])))))))


;; Construct a string describing the number of parameters:
;;  "a b c"      ==>   "3"
;;  "a ?b ?c"    ==>   "1 or 2 or 3"
;;  "a ?b ?c ..." ==>  "1 or more"
;;
(define (get-argc args)
  (if (filter "...% ?%" (lastword args))
      (if (filter "...%" (lastword args))
          (concat (words (filter-out "...% ?%" args)) " or more")
          (concat (get-argc (butlast args)) " or " (words args)))
      (words args)))


;; (define `(NAME ARGS...) FLAGS BODY)
;; (declare (NAME ARGS...) FLAGS)
;; (define  (NAME ARGS...) FLAGS BODY)

(define (c0-def-compound env n name args flags body is-define is-macro)
  (define `has-inline (filter "&inline" flags))
  (define `argc (get-argc (for a args (symbol-name a))))
  (define `gname (gen-global-name name flags))
  (define `scope (if (filter "&public" flags) "x" "p"))

  ;; Make function name known *within* the body, unless it is a macro.
  (define `env-in
    (if is-macro
        env
        (append { =name: (EFunc gname scope argc nil) } env)))

  (or (c0-check-body n (first body) is-define)

      (if (or is-macro has-inline)
          (vec-or
           (for a args
                (if (filter "...%" (symbol-name a))
                    (gen-error a "%s cannot have rest (...) parameters"
                               (if is-macro "macros" "inline functions"))))))

      (if (not is-macro)
          (check-name name n))

      ;; compile function/macro body
      (let ((macro-il (c0-lambda env-in args body))
            (env env))

        (define `defn
          (EFunc (if is-macro NoGlobalName gname)
                 scope
                 argc
                 (if (or is-macro has-inline)
                     (cons (current-depth env)
                           (case macro-il
                             ((ILambda body) body))))))

        (or (il-error-node macro-il)
            (IEnv (append {=name: defn} env)
                  (and is-define
                       (not is-macro)
                       (ICall "^fset" [(IString gname) macro-il])))))))

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
(define `(c0-def env sym args inblock is-define)
  (env-strip
   inblock
   (c0-def2 env (form-index sym) (first args)
            (get-flags args 1) (skip-flags args 1)
            is-define nil)))

(define (ml.special-define env sym args inblock)
  (c0-def env sym args inblock 1))

(define (ml.special-declare env sym args inblock)
  (c0-def env sym args inblock nil))


;; (require STRING [&private])
;;
;; Emit code to call REQUIRE, and add the module's exported symbols to the
;; current environment.
;;
(define (ml.special-require env sym args inblock)
  ;; These are supplied later by other modules.  We avoid circular build
  ;; dependencies this way.
  (declare (compile-module infile outfile flags))
  (declare *is-quiet*)

  (define `module (first args))
  (define `flags (get-flags args 1))
  (define `body (skip-flags args 1))
  (define `mod-name (string-value module))
  (define `read-priv (filter "&private" flags))

  (or
   (if body
       (gen-error body "too many arguments to require"))

   (case module
     ((PString _ name)
      (let ((origin (locate-module *compile-file* name))
            (module module) (env env) (name name) (inblock inblock) (read-priv read-priv))
        (define `id (module-id origin))
        (or (if (not origin)
                (gen-error module "require: Cannot find %q" name))
            (if (not (module-has-binary? origin))
                (begin
                  (or *is-quiet*
                      (print "... compiling " origin))
                  (if (compile-module origin (modid-file id) nil)
                      (gen-error module "require: compilation of %q failed" origin)
                      ;; success => nil => proceed to ordinary result
                      (set *file-mods* (append *file-mods* id)))))
            ;; already compiled or bundled
            (block-result inblock
                          (append (env-import origin read-priv) env)
                          (IBlock [ (ICall "^require" [ (IString id) ])
                                    (ICrumb "require" origin) ])))))
     (else
      (err-expected "S" module sym "STRING" "(require STRING)")))))


;; c0-block-cc: Compile a vector of forms, calling `k` with results.
;;
;;   FORMS = vector of forms
;;   K = fuction to call: (k new-env nodes)
;;   ENV = current environment
;;   RESULTS = results (not including previous compiled form)
;;   O = result of compiling previous form EXCEPT the first time
;;       this function is called, when it is nil.
;;       This may be a regular IL node *or* (IEnv env node)
;;
(define (c0-block-cc env forms k ?results ?o)
  &public
  (define `new-results
    (append results (if o [o])))

  (case o
    ((IEnv new-env node)
     (c0-block-cc new-env forms k results node))

    (else
     (if (not forms)
         (k env (filter-out NoOp new-results))
         (c0-block-cc env (rest forms) k new-results
                      (c0 (first forms) env 1))))))


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
                     (or (first results)
                         NoOp)))))


(define (ml.special-begin env sym args inblock)
  (c0-block args env))


;;--------------------------------
;; dictionaries
;;--------------------------------

;; Compile a form and encode its IL value as a dictionary key.
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
;; quasi-quoting
;;--------------------------------

;; A quasi-quoted form evaluates (at run time) to a form value.
;; Quasi-quoted forms can contain un-quoted expressions, which are
;; evaluated at run-time (presumably to a valid form).
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


(define (c0-error form)
  (define `msg
    (case form
      ((PUnquote n sub)
       "unquote (,) outside of a quasiquoted (`) form")
      ((PSplice n sub)
       "splice (,@) outside of a quasiquoted (`) form")
      (else
       "bad AST node: %q")))
  (gen-error form msg form))


;; c0: compile an inline expression.  Return IL.  (see c0-block)
(define (c0 form env ?inblock)
  &public
  (case form
    ((PSymbol n value) (c0-S env form value (resolve form env)))
    ((PString n value) (IString value))
    ((PList n subforms) (c0-L env n (first subforms) (rest subforms)
                              (resolve (first subforms) env) inblock))
    ((PDict n pairs) (c0-D env n pairs))
    ((PQuote n subform) (IString subform))
    ((PQQuote n subform) (c0-qq env subform))
    ((PError n code) form)
    (else (c0-error form))))
