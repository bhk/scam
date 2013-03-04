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
(require "gen0")


;; If exprs has exactly one item, return first item.  Otherwise place
;; them in a list form with 'sym' as the first element.
;; e.g. (group-with 'begin exprs)
(define (begin-block exprs)
  (if (filter 1 (words exprs))
      (first exprs)
      `(begin ,@exprs)))


;; (current-env)

(define (ml.special-current-env args env)
  ["Q" env])


;; (current-file-line)

(define (ml.special-current-file-line args env form)
  ;; '#pos' marks the position of a macro invocation
  (define `pos (or (word 2 (get "#pos" env))
                   (form-index form)))
  (define `lnum (describe-lnum pos *compile-subject*))

  ["Q" (concat *compile-file* ":" lnum)])
               


;; (native STRING)

(define (ml.special-native args env form)
  (or (check-argc 1 args form)
      (check-type "Q" (first args) form "CODE" "(native CODE)")
      ["R" (string-value (first args)) ] ))


;; (concat FORMS...)

(define (ml.special-concat args env)
  (concat "C " (c0-vec args env)))


;; (subst FROM TO {FROM TO}... STR)

(define (subst-x strs value)
  &private
  (if strs
      (subst-x (rrest strs) ["F" "subst" (nth 1 strs) (nth 2 strs) value])
      value))
      
(define (ml.special-subst args env form)
  (or (if (filter "%2 %4 %6 %8 %0 1" (words args))
          (gen-error (or (first args) form)
                     "wrong number of arguments to (subst {FROM TO}+ STR); must be multiple of 2 + 1"))
      (subst-x (c0-vec (butlast args) env) (c0 (last args) env))))


;; (vector FORMS...)

;; Return IL node for demotion of IL node.  In general the resulting
;; code calls `demote` at run time, but for quoted strings we can 
;; demote at compile time.
(define (c1-demote node)
  (if (string? node)
      (concat "Q " (demote (word 2 node)))
      ["F" "call" "Q ^d" node]))

(define (ml.special-vector args env)
  (concat "C " (subst " " " Q!0!10 " (map-call "c1-demote" (c0-vec args env)))))


;; (set SYM VALUE [RETVAL])

(define (ml.special-set args env form)
  (define `symbol (first args))
  (define `name (nth 2 symbol))
  (define `type (word 1 (resolve symbol env)))

  (or (check-argc "2 or 3" args form)
      (check-type "S" symbol form "NAME" "(set NAME VALUE [RETVAL])")
      (if (not (filter "V F" type))
          (gen-error symbol "%q is not a global variable" name))

      (append [ "f"
                (if (filter "V" type) "^set" "^fset")
                ["Q" name] ]
              (c0-vec (rest args) env))))


;; (let-global ((VAR VALUE)...) BODY...)
;;   ==>  (^set 'VAR (^set 'VAR VALUE VAR) (begin BODY...))

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

         
(define (ml.special-let-global args env form)
  (c0 (ml.macro-let-global form) env))


;; (? <fn> ...args...)

(define (ml.special-? args env form)
  (define `func (first args))
  (define `(traceable? defn)
    ;; (B)uiltin or (F)unction, but not a macro
    (filter-out "F#" (filter "B% F%" (subst " " "" (wordlist 1 2 defn)))))

  (or (check-type "S" func form "FUNC" "(? FUNC ...)")
      (if (not (traceable? (get (symbol-name func) env)))
          (gen-error func "FUNC in (? FUNC ...) is not a function variable"))
      (append [ "f" "^trace" (symbol-to-string func)]
              (c0-vec (rest args) env))))


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
           (foreach b nvs
                    (bind (nth 2 (nth 2 (promote b)))  ;; symbol name
                          ["M" (nth 3 (promote b))]))) ;; expr
          env))

(define (let&-check type form parent a)
  (check-type type form parent a "(let ((VAR VALUE)...) BODY)"))

(define (let&-check-bindings nvs form)
  (vec-or (for b nvs
               (or (let&-check "L" b form "(NAME EXPR)"))
                   (let&-check "S" (nth 2 b) form "NAME"))))


(define (ml.special-let& args env form)
  (define `nvs (rest (first args)))
  (define `body (rest args))
  (or (let&-check "L" (first args) form "((NAME EXPR)...)")
      (let&-check-bindings nvs form)
      (c0-block body (let&-env nvs env))))


;; ( let ((VAR VAL)...) BODY... )
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


(define (ml.special-let args env form)
  (c0 (ml.macro-let form) env))


;; (for VAR LIST BODY)

;; (foreach VAR WORDS EXP)
(define (ml.special-foreach args env form)
  (or (check-argc 3 args form)
      (check-type "S" (first args) form "VAR" "(foreach VAR LIST EXPR)")
      (let ((var (first args))
            (wrds (nth 2 args))
            (exp (nth 3 args))
            (env env))
        (c0 `(.foreach ,(symbol-to-string var) ,wrds ,exp)
            (bind (nth 2 var) (concat "V " (word 2 var)) env)))))


;; ==> (foreach "&f" LIST (^d (let& ((VAR (^u &f))) BODY)))
;;
;; Unlike Make builtin `foreach`, `for` operates on lists, promoting input
;; values and demoting results.  Also, VAR is a symbol, not a quoted string.

(define (ml.special-for args env)
  (define `var (first args))
  (define `vec (nth 2 args))
  (define `body (rrest args))

  (c0 `(foreach ,(gensym var) ,vec
                (call "^d" (let& ((,var (call "^u" ,(gensym var))))
                          ,@body)))
      
      env))

;; (print args...)

(define (ml.special-print args env)
  (c0 `(info (concat ,@args)) env))


;; (cond (TEST BODY...)...)
;;   ==>  (if TEST1 (begin BODY1...)
;;            (if TEST2 (begin BODY2...)
;;                ... ) )

(define cond-cxt "(cond (TEST BODY...)...)")

(define (cond-expand clause more form)
  (define `test (nth 2 clause))
  (define `body (rrest clause))

  (if clause
      (or (check-type "L" clause form "(TEST BODY...)" cond-cxt)
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

(define (ml.special-cond args env form)
  (c0 (ml.macro-cond form) env))
