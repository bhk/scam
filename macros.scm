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

(define (ml.special-current-env _ env)
  (String env))


;; (current-file-line)

(define (ml.special-current-file-line form env)
  ;; '#pos' marks the position of a macro invocation
  (define `pos (or (word 2 (hash-get "#pos" env))
                   (form-index form)))
  (define `lnum (describe-lnum pos *compile-subject*))

  (String (concat *compile-file* ":" lnum)))


;; (concat FORM...)

(define (ml.special-concat form env)
  (Concat (c0-vec (rrest form) env)))


;; (subst FROM TO {FROM TO}... STR)

(define (subst-x strs value)
  &private
  (if strs
      (subst-x (rrest strs)
               (Builtin "subst" (conj (wordlist 1 2 strs) value)))
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
  (Concat (subst " " " Q!0!10 " (map-call (global-name il-demote) (c0-vec args env)))))


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
            (Call (if (type? "V" binding) "^set" "^fset")
                  (cons (String (nth 2 binding))
                        (c0-vec other-args env)))))))


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
      (Call "^t" (cons (String (nth 2 defn))
                       (c0-vec func-args env)))))


;; (let& ((VAR VAL)...) BODY)

;; Similar to 'symbol-macrolet' in Common Lisp.
;;
;; `let&` associates variables with expressions that will be evaluated where
;; the variables occur.  The syntax is like `let*`, but the expressions
;; evaluated at each occurrence of the variable (zero or more times).
;;
;; `let&` generates an ESMacro environment record.
;;
;; Each *use* of the macro will be compiled in the scope of the definition.
;; We know that scope because we know where the definition itself lies in the
;; of bindings in the `env` structure.

(define (let&-env nvs env)
  (append (reverse
           (append-for b nvs
                       (hash-bind (nth 2 (nth 2 b))  ;; symbol name
                                  (ESMacro (nth 3 b) nil)))) ;; expr
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
      (String "")))

(define (ml.special-cond form env)
  (c0 (ml.macro-cond form) env))


;; (local-to-global EXPR)
;;
(define (ml.special-local-to-global form env)
  (define `var (nth 3 form))

  (or (check-argc 1 form)
      (Concat [ (String (gen-global-name ""))
                (c0 var env) ])))


;; (global-name SYM)
;;
(define (ml.special-global-name form env)
  (define `var (nth 3 form))

  (or (check-argc 1 form)
      (check-type "S" var form "NAME" "(get-global NAME)")
      (let& ((binding (resolve var env)))
            (if (type? "V F" binding)
                (String (nth 2 binding))
                (gen-error var "%q is not a global variable" (symbol-name var))))))


(define (ml.special-defmacro form env inblock)
  (define `what (nth 3 form))
  (define `sym (nth 2 what))
  (define `name (symbol-name sym))

  (or (check-type "L" what form "(NAME ARG...)" "(defmacro (NAME ARG...) BODY...)")
      (check-type "S" sym what "NAME" "(defmacro (NAME ARG...) ...)")

      ; `(define WHAT BODY)
      (let ((o (c0 (append ["L" ["S" "define"]]
                           (nth-rest 3 form))
                   env
                   inblock)))
        (define `env (nth 2 o))
        (define `node (nth-rest 3 o))
        (append ["env" (hash-bind name
                                  (EXMacro (gen-global-name name) nil)
                                  env)]
                node))))


;; (use STRING)
;;
(define (ml.special-use form env inblock)
  (define `module (nth 3 form))
  (define `mod-name (string-value module))

  (or (check-argc 1 form)
      (check-type "Q" module form "NAME" "(use NAME)")
      (let ((imports (use-module mod-name))
            (env env))
        (if imports
            (block-result inblock (append imports env) nil)))
      (gen-error "use: Cannot find module %q" mod-name)))


;; (data NAME CTOR...)
;;    CTOR = (NAME ARG...)
;;    ARG  = FLAG? NAME
;;    FLAG  = "&word"  => value contains no whitespace and is not nil
;;          | "&list"  => value has no leading or trailing whitespace

(data Data
      (DataType &word tag
                &word name
                encodings
                &list argnames))

;; Generate a Ctor record, or an Error record (a la parse tree) on failure.
;;
(define (get-type-r args form tag pattern names flag)
  &private
  (define `arg (first args))      ; a form
  (define `arg-name (nth 2 arg))  ; a string

  ;; Note: We can use list *encoding* only when a member is of list *type*
  ;; AND is the last member.
  (define `(arg-enc flag has-more-args)
    (cond ((eq "&word" flag) "W")
          ((and (eq "&list" flag) (not has-more-args)) "L")
          (else "S")))

  (or
   (if (not args)
       (if flag
           (gen-error form "no argument following last flag: %s" flag)
           ;; Done.
           (DataType tag (first pattern) (rest pattern) names)))

   (check-type "S" arg form "ARG" "(CTOR ARG...)")

   (if (not (filter "&%" arg-name))
       ;; argument name
       (get-type-r (rest args) form tag
                   (concat pattern " " (arg-enc flag (word 2 args)))
                   (conj names arg-name)
                   "" (filter "&list" flag)))

   (if flag
       (gen-error arg "two type flags supplied for one argument"))

   (if (filter "&list &word" arg-name)
       (get-type-r (rest args) form tag pattern names arg-name))

   (gen-error arg "unknown flag [supported: &list, &word]")))


(define (get-type form tag data-form)
  &private
  (define `sym (nth 2 form))        ;; symbol namin the ctor
  (define `name (symbol-name sym))
  (define `tag-str (nth 3 form))    ;; optional explicit tag

  (or (check-type "L" form data-form "(CTOR ...)" "(data NAME (CTOR ...)...)")
      (check-type "S" sym data-form "CTOR" "(data NAME (CTOR ...)...)")
      (if (type? "Q%" tag-str)
          ;; explicit tag
          (get-type-r (nth-rest 4 form) form (string-value tag-str) name [] nil)
          ;; no explicit tag
          (get-type-r (nth-rest 3 form) form tag name [] nil))))


;; Return vector of ctor descriptions (see get-type).
;;
(define (get-types data-form tag-base ctor-forms counter)
  &private
  (define `index (words counter))
  (define `tag (concat tag-base index))

  (if ctor-forms
      (cons (get-type (first ctor-forms) tag data-form)
            (get-types data-form tag-base (rest ctor-forms) (append counter 1)))))


(define (ml.special-data form env inblock)
  (define `name (nth 3 form))
  (define `ctor-forms (nth-rest 4 form))
  (define `tag-base (concat "!:" (nth 2 name)))

  (or
   (check-type "S" name form "NAME" "(data NAME ...)")

   (let ((types (get-types form tag-base ctor-forms nil)))
     (or
      ;; return all errors (if there were any)
      (if (filter "E%" types)
          (Block (filter "E%" types)))

      (begin
        ;; list of tag definitions:  tagname!=CtorName!01W!0L ...
        (define `tag-defs
          (append-for ty types
                      (case ty
                        ((DataType tag name encodings argnames)
                         (hash-bind tag (append name encodings))))))

        ;; Add record descriptions to the environment
        (define `bindings
          (append-for ty types
                      (case ty
                        ((DataType tag name encodings argnames)
                         (hash-bind name (ERecord encodings "." tag))))))

        ;; Add tag/pattern bindings to ^tags
        (define `node
          (Call (global-name ^add-tags) [(String tag-defs)]))

        (block-result inblock (append bindings env) node))))))


;; (case VALUE (PATTERN BODY)... )
;;   PATTERN = (NAME VAR...) | NAME
;;
;; Match a value against a series of patterns, evaluating the BODY paired
;; with the first match.

;; Return bindings for arguments in a (CTOR ARG...) pattern.
(define (arg-bindings args encs value-node)
  (foreach
   n (indices encs)
   (let& ((enc (word n encs))
          (arg (nth n args))
          (ndx-node (String (1+ n)))
          (arg-node
           (cond
            ((filter "S" enc) (Call "^n" [ndx-node value-node]))
            ((filter "W" enc) (Builtin "word" [ndx-node value-node]))
            (else (Builtin "wordlist" [ ndx-node
                                        (String 99999999)
                                        value-node])))))
         (hash-bind (symbol-name arg)
                    (EIL arg-node ".")))))


;; Compile a vector of (PATTERN BODY) cases
;;
(define (c0-matches cases value-node env)
  (for case cases
       ;; case = `(PATTERN BODY)
       (begin
         (define `pattern (nth 2 case))
         (define `body (nth-rest 3 case))
         (define `ctor-form (nth 2 pattern))
         (define `ctor-name (symbol-name ctor-form))
         (define `ctor-args (nth-rest 3 pattern))
         (define `defn (hash-get ctor-name env))
         (define `tag (nth 4 defn))
         (define `encs (nth 2 defn))
         (define `test-node
           (Builtin "filter" [ (String tag)
                               (Builtin "firstword" [value-node]) ]))
         (define `bindings (arg-bindings ctor-args encs value-node))
         (define `then-node (c0-block body (append bindings env)))

         (or (check-type "L" case nil "(PATTERN BODY)"
                         "(case VALUE (PATTERN BODY)...)")

             (check-type "L S" pattern case "(CTOR ARG...)"
                         "(case VALUE ((CTOR ARG..) BODY)...)")

             (if (symbol? pattern)
                 (c0-block body (hash-bind (symbol-name pattern)
                                           ["I" value-node]
                                           env))
                 ;; (CTOR NAME)
                 (or
                  (check-type "S" ctor-form pattern "CTOR" "(CTOR ARG...)")
                  (check-argc (words encs) pattern)
                  (Builtin "if" [ test-node then-node ])))))))


(define (nest-nodes nodes)
  &private
  (concat (first nodes)
          (if (word 2 nodes)
              (concat " " [(nest-nodes (rest nodes))]))))


(define (ml.special-case form env)
  (define `value-node (c0 (nth 3 form) env))
  (if (not (word 3 form))
      (gen-error form "missing VALUE in (case VALUE ...)")
      (nest-nodes (c0-matches (nth-rest 4 form) value-node env))))
