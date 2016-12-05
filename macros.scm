;;--------------------------------------------------------------
;; macros.scm : standard macro definitions
;;--------------------------------------------------------------

;; Functions named "ml.special-NAME" implement special forms as well as
;; macros.  They are executed at compile-time when a list form starting with
;; the correponding NAME is encountered.  These functions are passed a list
;; of arguments (each an AST) and an environment.  They return an IL tree.
;;
;; The term "macro" would properly be applied to a function that transforms
;; an AST to another AST. "Special forms" implement custom code generation.
;; Both are lumped into the same mechanism here -- AST in, IL out.
;;
;; Some specials here are described as "inlined".  This means that actual
;; functions by the same name are available, although when called directlly
;; by name the code will be expanded inline for performance reasons.

(require "core")
(require "parse")
(require "gen")
(require "gen0" &private)


;; If exprs has exactly one item, return first item.  Otherwise place
;; them in a list form with 'sym' as the first element.
;; e.g. (group-with 'begin exprs)
(define (begin-block exprs)
  (if (filter 1 (words exprs))
      (first exprs)
      `(begin ,@exprs)))


;; (current-env)

(define (ml.special-current-env _ env)
  ["Q" env])


;; (current-file-line)

(define (ml.special-current-file-line form env)
  ;; '#pos' marks the position of a macro invocation
  (define `pos (or (word 2 (hash-get "#pos" env))
                   (form-index form)))
  (define `lnum (describe-lnum pos *compile-subject*))

  ["Q" (concat *compile-file* ":" lnum)])


;; (concat FORM...)

(define (ml.special-concat form env)
  (concat "C " (c0-vec (rrest form) env)))


;; (subst FROM TO {FROM TO}... STR)

(define (subst-x strs value)
  &private
  (if strs
      (subst-x (rrest strs) ["F" "subst" (nth 1 strs) (nth 2 strs) value])
      value))

(define (ml.special-subst form env)
  (let ((args (rrest form))
        (keyword (nth 2 form))
        (env env))

    (or (if (filter "%2 %4 %6 %8 %0 1" (words args))
            (gen-error keyword
                       "wrong number of arguments to (subst {FROM TO}+ STR); must be multiple of 2 + 1"))
        (subst-x (c0-vec (butlast args) env) (c0 (last args) env)))))


;; (vector FORM...)

(define (ml.special-vector form env)
  (define `args (rrest form))
  (concat "C " (subst " " " Q!0!10 " (map-call (global-name il-demote) (c0-vec args env)))))


;; (set SYM VALUE [RETVAL])

(define (ml.special-set form env)
  (define `symbol (nth 3 form))

  (or (check-argc "2 or 3" form)

      (check-type "S" symbol form "NAME" "(set NAME VALUE [RETVAL])")

      (let ((binding (resolve symbol env))
            (other-args (nth-rest 4 form))
            (env env)
            (symbol symbol))

        (if (not (type? "V F" binding))
            (gen-error symbol "%q is not a global variable" (symbol-name symbol))
            (append [ "f"
                      (if (type? "V" binding) "^set" "^fset")
                      ["Q" (nth 2 binding)] ]
                    (c0-vec other-args env))))))


;; (let-global ((VAR VALUE)...) BODY)
;;   ==>  (^set 'VAR (^set 'VAR VALUE VAR) (begin BODY))

(define (letg-error type form parent desc)
  &private
  (check-type type form parent desc "(let-global ((VAR VALUE)...) BODY)"))


(define (letg-expand blist body form)
  &private
  (define `b (first blist))
  (define `sym (nth 2 b))                ;; symbol for var being set
  (define `val (nth 3 b))                ;; value being assigned to var

  (if (not blist)
      ;; no more vars to bind
      (begin-block body)
      (or (letg-error "L" b form "(VAR VALUE)")
          (letg-error "S" sym b "VAR")
          (letg-error "" val  b "VALUE")

          `(set ,sym
                (set ,sym ,val ,sym)
                ,(letg-expand (rest blist) body form)))))


(define (ml.macro-let-global form)
  (define `bform (nth 3 form))   ;; form that contains bindings
  (define `body  (nth-rest 4 form))

  (or (letg-error "L" bform form "((VAR VALUE)...)")
      (letg-expand (rest bform) body form)))


(define (ml.special-let-global form env)
  (c0 (ml.macro-let-global form) env))


;; (? <fn> ...args...)

(define (ml.special-? form env)
  (define `func (nth 3 form))
  (define `func-args (nth-rest 4 form))
  (define `defn (resolve func env))

  (define `traceable?
    ;; (B)uiltin or (F)unction, but not a macro
    (filter-out "F#" (filter "B% F%" (subst " " "" (wordlist 1 2 defn)))))

  (or (check-type "S" func form "FUNC" "(? FUNC ...)")
      (if (not traceable?)
          (gen-error func "FUNC in (? FUNC ...) is not a function variable"))
      (append [ "f" "^t" ["Q" (nth 2 defn)] ]
              (c0-vec func-args env))))


;; (let& ((VAR VAL)...) BODY)

;; Similar to 'symbol-macrolet' in Common Lisp.
;;
;; `let&` associates variables with expressions that will be evaluated where
;; the variables occur.  The syntax is like `let*`, but the expressions
;; evaluated at each occurrence of the variable (zero or more times).
;;
;; `let&` generates an "M" record in the environment:
;;
;;     ["M" <form>]
;;
;; Each *use* of the macro will be compiled in the scope of the definition.
;; We know that scope because we know where the definition itself lies in the
;; of bindings in the `env` structure.

(define (let&-env nvs env)
  (append (reverse
           (append-for b nvs
                       (hash-bind (nth 2 (nth 2 b))  ;; symbol name
                                  ["M" (nth 3 b)]))) ;; expr
          env))

(define (let&-check type form parent a)
  (check-type type form parent a "(let ((VAR VALUE)...) BODY)"))

(define (let&-check-bindings nvs form)
  (vec-or (for b nvs
               (or (let&-check "L" b form "(NAME EXPR)"))
                   (let&-check "S" (nth 2 b) form "NAME"))))


(define (ml.special-let& form env)
  (define `nv-list (nth 3 form))
  (define `nvs (rest nv-list))
  (define `body (nth-rest 4 form))
  (or (let&-check "L" nv-list form "((NAME EXPR)...)")
      (let&-check-bindings nvs form)
      (c0-block body (let&-env nvs env))))


;; ( let ((VAR VAL)...) BODY )
;;   ==>  ( (lambda (VAR ...) BODY ) (VAL ...) )

(define (let-error type form parent a)
  (check-type type form parent a "(let ((VAR VALUE)...) BODY)"))


(define (ml.macro-let form)
  (define `bform (nth 3 form))   ;; form that contains bindings
  (define `blist (rest bform))   ;; list of bindings: (VAR VALUE) ...
  (define `body  (nth-rest 4 form))
  (define `vars  (append "L" (for b blist (nth 2 b))))
  (define `vals  (for b blist (nth 3 b)))

  (or (let-error "L" bform form "((VAR VALUE)...)")
      (vec-or (for b blist (or (let-error "L" b form "(VAR VALUE)")
                               (let-error "S" (nth 2 b) b "VAR")
                               (let-error "" (nth 3 b) b "VALUE"))))
      (append ["L" (append ["L" "S lambda" vars] body)] vals)))


(define (ml.special-let form env)
  (c0 (ml.macro-let form) env))


;; (foreach VAR WORDS EXP)
(define (ml.special-foreach form env)
  (define `var (nth 3 form))
  (define `word-list (nth 4 form))
  (define `exp (nth 5 form))

  (or (check-argc 3 form)
      (check-type "S" var form "VAR" "(foreach VAR LIST EXPR)")
      (let ((var var)
            (word-list word-list)
            (exp exp)
            (env env))
        (c0 `(.foreach ,(symbol-to-string var) ,word-list ,exp)
            (hash-bind (nth 2 var) (concat "V " (word 2 var)) env)))))


;; (for VAR VEC BODY)
;; ==> (foreach "&f" VEC (^d (let& ((VAR (^u &f))) BODY)))
;;
;; Unlike Make builtin `foreach`, `for` operates on lists, promoting input
;; values and demoting results.  Also, VAR is a symbol, not a quoted string.

(define (ml.special-for form env)
  (define `var (nth 3 form))
  (define `vec (nth 4 form))
  (define `body (nth-rest 5 form))

  (c0 `(foreach ,(gensym var) ,vec
                (call ,["Q" "^d"] (let& ((,var (call ,["Q" "^u"] ,(gensym var))))
                          ,@body)))
      env))


;; (append-for VAR VEC BODY)
;; ==> (filter "%" (foreach "&f" VEC (let& ((VAR (^u &f))) BODY)))
;;
;; Similar to `(for ...)` but evaluates to the result of appending all BODY
;; values (vectors).

(define (ml.special-append-for form env)
  (define `var (nth 3 form))
  (define `vec (nth 4 form))
  (define `body (nth-rest 5 form))

  (c0 `(filter "%"
               (foreach ,(gensym var) ,vec
                        (let& ((,var (call ,["Q" "^u"] ,(gensym var))))
                              ,@body)))
      env))


;; (concat-for VAR VEC DELIM BODY)
;;
;; Similar to `(for ...)` but evaluates to the string concatenation of all
;; BODY values.

(define (ml.special-concat-for form env)
  (define `var (nth 3 form))
  (define `vec (nth 4 form))
  (define `delim (nth 5 form))
  (define `body (nth-rest 6 form))

  (define `expansion
    (if (and (type? "Q%" delim)
             (eq (nth 2 delim) " "))
        ;; Simple case (spaces are the default separator)
        `(foreach ,(gensym var) ,vec
                  (let& ((,var (call ,["Q" "^u"] ,(gensym var))))
                        ,@body))

        ;; Replace space separators with DELIM.
        `(subst "|. " (subst "|" "|1" ,delim)
                "|." ""
                "|1" "|"
                (foreach ,(gensym var) ,vec
                         (let& ((,var (call ,["Q" "^u"] ,(gensym var))))
                               (concat (subst "|" "|1" ,@body)
                                       "|."))))))
  (c0 expansion env))



;; (print args...)

(define (ml.special-print form env)
  (define `args (rrest form))
  (c0 `(info (concat ,@args)) env))


;; (cond (TEST BODY)...)
;;   ==>  (if TEST1 (begin BODY1...)
;;            (if TEST2 (begin BODY2...)
;;                ... ) )

(define cond-cxt "(cond (TEST BODY)...)")

(define (cond-expand clause more form)
  (define `test (nth 2 clause))
  (define `body (rrest clause))

  (if clause
      (or (check-type "L" clause form "(TEST BODY)" cond-cxt)
          (check-type "" test clause "TEST" cond-cxt)
          (check-type "" body clause "BODY" cond-cxt)
          (if (and (symbol? test)
                   (eq "else" (word 2 test)))
              (begin-block body)
              `(if ,test
                   ,(begin-block body)
                   ,@(if more
                         [(cond-expand (first more) (rest more) form)]))))))


(define (ml.macro-cond form)
  (or (cond-expand (nth 3 form) (nth-rest 4 form) form)
      "Q !."))

(define (ml.special-cond form env)
  (c0 (ml.macro-cond form) env))


;; (local-to-global EXPR)
;;
(define (ml.special-local-to-global form env)
  (define `var (nth 3 form))

  (or (check-argc "1" form)
      ["C" ["Q" (gen-global-name "")]
           (c0 var env)]))


;; (global-name SYM)
;;
(define (ml.special-global-name form env)
  (define `var (nth 3 form))
  (define `binding (resolve var env))

  (or (check-argc "1" form)
      (check-type "S" var form "NAME" "(get-global NAME)")
      (let& ((binding (resolve var env)))
        (if (type? "V F" binding)
            ["Q" (nth 2 binding)]
            (gen-error var "%q is not a global variable" (symbol-name var))))))


(define (ml.special-defmacro form env inblock)
  (define `what (nth 3 form))
  (define `sym (nth 2 what))
  (define `name (symbol-name sym))

  (or (check-type "L" what form "(NAME ARG...)" "(defmacro (NAME ARG...) BODY...)")
      (check-type "S" sym what "NAME" "(defmacro (NAME ARG...) ...)")

      ; `(define WHAT BODY)
      (let ((o (c0 (append ["L" ["S" "define"]] (nth-rest 3 form))
                   env
                   inblock)))
        (define `env (nth 2 o))
        (define `node (nth-rest 3 o))
        (append ["env" (hash-bind name ["X" (gen-global-name name)] env)]
                node))))


;; (use STRING)
;;
(define (ml.special-use form env inblock)
  (define `module (nth 3 form))
  (define `mod-name (string-value module))

  (or (check-argc "1" form)
      (check-type "Q" module form "NAME" "(use NAME)")
      (let ((imports (use-module mod-name))
            (env env))
        (if imports
            (block-result inblock (append imports env) nil)))
      (gen-error "use: Cannot find module %q" mod-name)))
