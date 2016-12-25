;;--------------------------------------------------------------
;; gen0 : compiler front end  (see gen.scm)
;;--------------------------------------------------------------

(require "core")
(require "parse")
(require "escape")
(require "gen")


;; Env-modifying functions can return an environment when `inblock` is true.
;; Use this macro to generate the return value.
;;
(define (block-result inblock env node)
  (if inblock
      (IEnv env node)
      (or node NoOp)))


;; Extract sub-node from (IEnv ...) if INBLOCK is nil.
;;
(define (env-strip inblock node)
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
  (append-for form (wordlist (1+ skip) (scan-flags args skip) args)
              (case form
                ((PSymbol n name) name))))

(define (skip-flags args skip)
  (nth-rest (1+ (scan-flags args skip)) args))


;;================================
;; c0 compilation
;;================================

(declare (c0 form env))
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
(define (c0-local arg marker sym)
  (define `ndx
    (subst "." "" arg))
  (define `level
    (case marker
      ((EMarker level) level)))

  (if (and (findstring "U" SCAM_DEBUG)
           (not (findstring level arg)))
          (compile-warn sym "reference to upvalue %q" (symbol-name sym)))
  (ILocal ndx
         (words (subst "." ". " (subst arg "" (concat level ndx))))))


;; Return the "value" of a record constructor: an equivalent anonymous
;; function.
;;
(define (c0-ctor env sym encs)
  &private
  (define `args
    (for i (indices encs)
         (PSymbol 0 (concat "a" i))))
  (c0-lambda env args [ (PList 0 (cons sym args)) ]))


;; MACRO --> (lambda (a b c...) (MACRO a b c...)
;;
(define (c0-macro env sym inln)
  (define `arg-syms
    (for a (first inln)
         (PSymbol 0 a)))
  (c0-lambda env arg-syms [ (PList 0 (cons sym arg-syms)) ]))


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


;; Symbol
;;    defn = definition for symbol (from environment)
;(define (c0-S form env defn)
(define (c0-S env sym name defn)
  &private
  (case defn
    ((EArg arg)
     (c0-local arg (hash-get LambdaMarkerKey env) sym))

    ((EVar gname p)
     (IVar gname))

    ((EFunc gname p inln)
     (if (filter NoGlobalName gname)
         (c0-macro env sym inln)
         (IBuiltin "value" [(IString gname)])))

    ((ESMacro value-form p)
     (c0 value-form (env-rewind env name)))

    ((EIL node priv)
     node)

    ((ERecord encs p tag)
     (c0-ctor env sym encs))

    ((EBuiltin name priv argc)
     (c0-builtin env name argc))

    (else (c0-S-error sym defn))))


;; Compile a vector of expressions independently, as in an argument
;; list.  Declarations and definitions on one expression do not apply to
;; subsequent ones (inlike c0-block-cc).
;;
(define (c0-vec forms env)
  (for f forms (c0 f env)))


;; Expand inlined function
;;
;; 1. Compile each argument expression to IL.
;; 2. Expand function definition in environment that matches its original
;;    environment PLUS bindings for the formal arguments (bound to an "I"
;;    value).
;;
(define (c0-call-inline env sym args realname inln)
  (define `name (symbol-name sym))
  (define `formal-args (first inln))
  (define `body (rest inln))

  (define `(zip vec1 vec2)
    (join (addsuffix "!=" vec1) vec2))

  (define `body-env
    (append (zip formal-args
                 (for a args
                      (EIL (c0 a env) "p")))
            ;; Track the source position where the macro was invoked
            (hash-bind MacroMarkerKey
                       (EMarker (form-index sym)))
            ;; re-bind symbol to non-inline function bindingto avoid
            ;; endless cycle of expansion
            (if (not (filter NoGlobalName realname))
                (hash-bind name (EFunc realname "." nil)))
            ;; rewind env to allow proper resolution of symbols in body
            (env-rewind env name)))

  (or (check-argc (words formal-args) args sym)
      (c0-block body body-env)))


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
                (define `value (c0 arg env nil))
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
  &private
  (define `symname (symbol-name sym))

  (case defn
    ((EFunc realname p inln)
     (if inln
         (c0-call-inline env sym args realname inln)
         (ICall realname (c0-vec args env))))

    ((EBuiltin realname p argc)
     (or (check-argc argc args sym)
         (IBuiltin realname (c0-vec args env))))

    ((EXMacro name priv)
     (if priv
         (c0 (call name args) env inblock)
         (gen-error sym "cannot use xmacro in its own file")))

    ((ERecord encodings priv tag)
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
               (hash-bind (or (patsubst "...%" "%" name) name) rest-value)
               (hash-bind name single-value))))


;; Add local variables to environment.
;;
;; level = string of "$" for a lambda marker
;;
(define (lambda-env-arg9 xargs level)
  &private

  (define `(nth-value n)
    (EIL (IBuiltin "call" [(IString "^n") (IString n) (IVar 9)]) "."))
  (define `(nth-rest-value n)
    (if (eq? n 1)
        (EIL (IVar 9) ".")
        (EIL (IBuiltin "wordlist" [(IString n) (IString 999999) (IVar 9)]) ".")))

  (foreach n (indices xargs) (lambda-arg (nth n xargs)
                                         (nth-value n)
                                         (nth-rest-value n))))


(define (lambda-env-args args level)
  &private

  (define `(nth-value n)
    (EArg (concat level n)))
  (define `(nth-rest-value n)
    (EIL (IBuiltin "foreach" [ (IString "N") (IString n) (IVar "^v") ]) "."))

  (append (hash-bind LambdaMarkerKey (EMarker level))
          ;; first 8 args = $1 ... $8
          (foreach n (indices (wordlist 1 8 args))
                   (lambda-arg (nth n args)
                               (nth-value n)
                               (nth-rest-value n)))
          ;; args 9 and up = (nth 1 $9), (nth 2 $9), ...
          (if (word 9 args)
              (lambda-env-arg9 (nth-rest 9 args) level))))


(define (emarker-level marker)
  (case marker
    ((EMarker level) level)))


(define `(lambda-env args env)
  (append (lambda-env-args
           args
           (concat "." (emarker-level (hash-get LambdaMarkerKey env))))
          env))


(define (c0-lambda env args body)
  (ILambda (c0-block body (lambda-env args env))))


(define (lambda-error type form parent desc)
  &private
  (err-expected type form parent desc "(lambda (ARGNAME...) BODY)"))


;; special form: (lambda ARGS BODY)
(define (ml.special-lambda env sym args inblock)  ;;form env)
  &private
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

;; (define `NAME FLAGS... BODY)
;; (declare NAME FLAGS...)
;; (define  NAME FLAGS... BODY)

(define (c0-def-symbol env n name flags body is-define is-macro)
  (define `priv (if (filter "&private" flags) "p" "."))
  (define `is-global (filter "&global" flags))
  (define `gname (gen-global-name name flags))

  (define `env-out
    (hash-bind name (if is-macro
                        (ESMacro (begin-block body) priv)
                        (EVar gname priv))
               env))

  (define `set-il
    (ICall "^set" [ (IString gname) (c0-block body env) ]))

  (or (if (filter "&inline" flags)
          (gen-error n "'&inline' does not apply to symbol definitions"))
      (c0-check-body n (first body) is-define)
      (IEnv env-out
             (if (and is-define (not is-macro))
                 set-il))))


;; (define `(NAME ARGS...) FLAGS BODY)
;; (declare (NAME ARGS...) FLAGS)
;; (define  (NAME ARGS...) FLAGS BODY)

(define (c0-def-compound env n name args flags body is-define is-macro)
  (define `priv (if (filter "&private" flags) "p" "."))
  (define `is-inline (and (not is-macro) (filter "&inline" flags)))
  (define `is-global (filter "&global" flags))
  (define `gname (gen-global-name name flags))

  ;; inline function definition (to be embedded in env entry)
  (define `fdefn (cons (for sym args (symbol-name sym))
                       body))

  ;; make function name known *within* the function body (without inline)
  (define `env-in
    (if is-macro
        env
        (hash-bind name (EFunc gname priv nil) env)))

  ;; make function known *after* the definition (including inline expansion)
  (define `env-out
    (hash-bind name (EFunc (if is-macro NoGlobalName gname)
                           priv
                           (if (or is-inline is-macro)
                               fdefn))
               env))

  (define `set-il
    (ICall "^fset" [ (IString gname) (c0-lambda env-in args body) ]))

  (or (c0-check-body n (first body) is-define)
      (if is-macro
          (vec-or
           (for a args
                (if (filter "...%" (symbol-name a))
                    (gen-error a "macros do not support varargs"))))
          (if (filter name builtin-names)
              (gen-error n "cannot redefine built-in function %q" name)))
      (IEnv env-out
             (if (and is-define (not is-macro))
                 set-il))))


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
  (define `module (first args))
  (define `flags (get-flags args 1))
  (define `body (skip-flags args 1))
  (define `mod-name (string-value module))
  (define `priv (filter "&private" flags))

  (or
   (if body
       (gen-error body "too many arguments to require"))

   (case module
     ((PString pos name)
      (let ((imports (require-module name priv))
            (env env)
            (name name)
            (inblock inblock))
        (if imports
            (block-result inblock
                          (append imports env)
                          (ICall "^require" [ (IString (notdir name)) ]))
            (gen-error module "require: Cannot find module %q" name))))

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
(define (c0-block-cc env forms k results o)
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
  &private
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
(define (c0-qq env form nest)
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
(define (c0 form env inblock)
  ;;(if (findstring "c0" SCAM_DEBUG)
  ;;    (printf "form: %q" form))
  (case form
    ((PSymbol n value) (c0-S env form value (resolve form env)))
    ((PString n value) (IString value))
    ((PList n subforms) (c0-L env n (first subforms) (rest subforms)
                              (resolve (first subforms) env) inblock))
    ((PQuote n subform) (IString subform))
    ((PQQuote n subform) (c0-qq env subform))
    ((PError n code) form)
    (else (c0-error form))))
