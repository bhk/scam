;;--------------------------------------------------------------
;; macros.scm : standard macro definitions
;;--------------------------------------------------------------

(require "core")
(require "parse")
(require "gen")
(require "gen0")

;; Functions named "ml.special-NAME" implement special forms.  They are
;; executed at compile-time when a list form starting with the corresponding
;; NAME is encountered.  These functions are passed a list of arguments
;; (each an AST) and an environment.  They return an IL tree.
;;
;; (ml.special-SPECIALNAME env sym args)
;;    ENV = environment
;;    SYM = the symbol naming the special form (e.g. `print)
;;    ARGS = forms passed to the special form


;;--------------------------------
;; (print args...)
;;--------------------------------

(define (ml.special-print env sym args)
  (IBuiltin "info" [ (IConcat (c0-vec args env)) ]))


;;--------------------------------
;; (current-env)
;;--------------------------------

(define (ml.special-current-env env sym args)
  (IString env))


;;--------------------------------
;; (current-file-line)
;;--------------------------------

;; "FILE:LINE:COL" at which the macro containing this line is invoked.
;;
(define (ml.special-current-file-line env sym args)
  (or (check-argc 0 args sym)
      (IWhere (get-where (form-index sym)))))


;;--------------------------------
;; (concat FORM...)
;;--------------------------------

(define (ml.special-concat env sym args)
  (il-concat (c0-vec args env)))


;;--------------------------------
;; (vector FORM...)
;;--------------------------------

(define (ml.special-vector env sym args)
  (il-concat
   (intersperse (IString " ")
                (for f args
                     (il-demote (c0 f env))))))


;;--------------------------------
;; (subst FROM TO {FROM TO}... STR)
;;--------------------------------

(define (subst-x strs value)
  (if strs
      (subst-x (rrest strs) (IBuiltin "subst" (conj (wordlist 1 2 strs) value)))
      value))

(define (ml.special-subst env sym args)
  (if (filter "%2 %4 %6 %8 %0 1" (words args))
      (gen-error sym
                 "(subst {FROM TO}+ STR) accepts 2n+1 arguments, not %s"
                 (words args))
      (subst-x (c0-vec (butlast args) env) (c0 (last args) env))))


;;--------------------------------
;; (set SYM VALUE [RETVAL])
;;--------------------------------

(define (c0-set env sym value-node retval-node what where)
  (let ((binding (resolve sym env))
        (sym sym)
        (value-node value-node)
        (retval-node retval-node)
        (env env))

    (define `(il-set setter varname)
      (ICall setter (append [ (IString varname) value-node ]
                           (if retval-node
                               [retval-node]))))
    (case sym
      ((PSymbol pos var-name)
       (case binding
         ((EVar name _) (il-set "^set" name))
         ((EFunc name _ _ _) (il-set "^fset" name))
         (else
          (gen-error sym "%q is not a global variable" (symbol-name sym)))))
      (else
       (err-expected "S" sym nil what where)))))


(define (ml.special-set env sym args)
  (define `var-sym (first args))
  (define `value-form (nth 2 args))
  (define `retval (nth 3 args))

  (or (check-argc "2 or 3" args sym)
      (c0-set env
              var-sym
              (c0 value-form env)
              (if retval (c0 retval env))
              "NAME"
              "(set NAME VALUE [RETVAL])")))


;;--------------------------------
;; (? <fn> ...args...)
;;--------------------------------

(define (ml.special-? env sym args)
  (define `func (first args))
  (define `func-args (rest args))
  (define `defn (resolve func env))

  (let ((defn (resolve func env))
        (env env)
        (sym sym)
        (args args))
    (or
     (if (eq? "-" defn)
         (err-expected "S" func sym "FUNC" "(? FUNC ARGS...)")
         (begin
           (define `(trace ctor name)
             (ctor "^t" (cons (IString name) (c0-vec func-args env))))
           (case defn
             ((EFunc name _ _ _) (if (not (eq? name NoGlobalName))
                                     (trace ICall name)))
             ((EBuiltin name _ _) (trace IBuiltin name)))))

     (if defn
         (gen-error func "FUNC in (? FUNC ...) is not traceable")
         (gen-error func "undefined variable: %q" (symbol-name sym))))))


;;--------------------------------
;; (let ((VAR VAL)...) BODY)
;;--------------------------------

(define (read-pairs-r forms where out)
  (define `form (first forms))
  (if (not forms)
      out
      (case form
        ((PList n pair)
         (define `var (first pair))
         (define `value (nth 2 pair))
         (define `extra (nth 3 pair))
         (case var
           ((PSymbol n name)
            (if value
                (if (not extra)
                    (read-pairs-r (rest forms) where (conj out pair))
                    (gen-error extra "extra form after value in %q" where))
                (err-expected "" value form "VALUE" where)))
           (else (err-expected "S" var form "VAR" where))))
        (else (err-expected "L" form nil "(VAR VALUE)" where)))))

;; Parse and validate a form describing (SYM VALUE) pairs as in `(let ...)
;; expressions.  Return a vector of [SYM VALUE] pairs, or a PError on
;; failure.
;;
;;  LIST = `( (NAME VALUE)... )
;;
(define (read-pairs list sym where)
  (case list
    ((PList n forms)
     (read-pairs-r forms where nil))
    (else
     (err-expected "L" list sym "((VAR VALUE)...)" where))))


(define let-where
  "(let ((VAR VALUE)...) BODY)")

;; (let ((VAR VAL)...) BODY)
;;   ==>  ( (lambda (VAR ...) BODY ) (VAL ...) )
(define (ml.special-let env sym args)
  (define `form
    (let ((body (rest args))
          (pairs (read-pairs (first args) sym let-where)))
      (define `vars (for p pairs (nth 1 p)))
      (define `values (for p pairs (nth 2 p)))
      (or (case pairs ((PError _ _) pairs))
          (PList 0 (cons (PList 0 (append [(PSymbol 0 "lambda")]
                                          [(PList 0 vars)]
                                          body))
                         values)))))
  (c0 form env))


;;--------------------------------
;; (let-global ((VAR VALUE)...) BODY)
;;--------------------------------

(define letg-where
  "(let-global ((VAR VALUE)...) BODY)")

;; (let-global ((VAR VALUE) OTHERS...) BODY)
;;    -->  (^set 'VAR (^set 'VAR VALUE VAR) (let-global (OTHERS...) BODY))
;;
;;   BINDING = (PList n [ SYM VALUE ])
;;   OTHERS = [BINDING...]
;;
(define (letg-expand env sym body pairs)
  (or
   ;; read-pairs may have returned an error...
   (case pairs
     ((PError _ _) pairs))

   (if (not pairs)
      ;; no more bindings
      (c0-block body env))

   (begin
     (define `p (first pairs))
     (define `sym (first p))
     (define `value (nth 2 p))
     (define `others (letg-expand env sym body (rest pairs)))
     ;; don't need WHAT and WHERE; the other call to c0-set will get those
     (define `inner (c0-set env sym (c0 value env) (c0 sym env) nil nil))

     (c0-set env sym inner others nil nil))))


(define (ml.special-let-global env sym args)
  (letg-expand env sym (rest args)
               (read-pairs (first args) sym letg-where)))


;;--------------------------------
;; (let& ((VAR VAL)...) BODY)
;;--------------------------------

;; Similar to 'symbol-macrolet' in Common Lisp, `let&` associates variables
;; with expressions that will be evaluated where the variables occur.
;; Scoping works like `let*`, but the expressions evaluated at each
;; occurrence of the variable (zero or more times).
;;
;; `let&` generates an EIL environment record.
;;
;; Each *use* of the macro will be compiled in the scope of the definition.
;; We know that scope because we know where the definition itself lies in
;; the environment.

(define let&-where
  "(let& ((VAR VALUE)...) BODY)")

(define (let&-env pairs env depth)
  (define `p (first pairs))
  (define `p-binding
    (case (first p)
      ((PSymbol _ name)
       { =name: (EIL depth "-" (c0 (nth 2 p) env)) })))

  (if pairs
      (let&-env (rest pairs) (append p-binding env) depth)
      env))

(define (ml.special-let& env sym args)
  (let ((body (rest args))
        (pairs (read-pairs (first args) sym let&-where))
        (env env))
    (case pairs
      ((PError _ _) pairs)
      (else (c0-block body (let&-env pairs env (current-depth env)))))))


;;--------------------------------
;; (foreach VAR LIST BODY)
;;--------------------------------

;; c0-for
;;   ARGS = VAR LIST BODY   in   `(for VAR LIST BODY)
;;   WHERE = syntax synopsis of the containing form, including "VAR"
;;           "BODY" (and optionally "DELIM")
;;   VAR-XFORM = transformation to apply to VAR's IL node
;;               where it is mentioned in BODY
;;   BODY-XFORM = transformation to apply to BODY after it is evaluated
;;
(define (c0-for env sym args where var-xform body-xform)
  (define `var (first args))
  (define `list (nth 2 args))
  (define `body-index (words (rest where)))  ;; 3- or 4-arg form
  (define `body (nth-rest body-index args))

  (case var
    ((PSymbol _ name)
     (define `var-defn
       (EIL "" "-" (var-xform (IVar name))))
     (define `body-node
       (body-xform (c0-block body (append { =name: var-defn } env))))
     (if body
         ;; list, delim, and body ok
         (IBuiltin "foreach" [ (IString name) (c0 list env) body-node ])
         ;; body (and maybe list and delim) missing
         (err-expected "" nil sym
                       (word (words (concat ". . " args)) (subst ")" "" where))
                       where)))
    (else
     (err-expected "S" var sym "VAR" where))))

;; (foreach VAR LIST BODY) : Unlike in the Make builtin "foreach", VAR is a
;; symbol, not a quoted string.
;;
(define (ml.special-foreach env sym args)
  (c0-for env sym args "(foreach VAR LIST BODY)" identity identity))


;;--------------------------------
;; (for VAR VEC BODY)
;;--------------------------------

;; --> (foreach "&f" VEC (^d (let& ((VAR (^u &f))) BODY)))
;;
(define (ml.special-for env sym args)
  (c0-for env sym args "(for VAR VEC BODY)" il-promote il-demote))


;;--------------------------------
;; (append-for VAR VEC BODY)
;;--------------------------------

;; Append all body values (a.k.a. "concat-map").
;;
;; --> (filter "%" (foreach "&f" VEC (let& ((VAR (^u &f))) BODY)))
;;
(define (ml.special-append-for env sym args)
  (define `for-value
    (c0-for env sym args "(append-for VAR VEC BODY)" il-promote identity))

  (IBuiltin "filter" [ (IString "%") for-value ]))


;;--------------------------------
;; (concat-for VAR VEC DELIM BODY)
;;--------------------------------

;; Similar to `(for ...)` but evaluates to the string concatenation of all
;; BODY values.

(define concat-for-where
  "(concat-for VAR VEC DELIM BODY)")

(define (il-spc-encode node)
  (il-subst " " "|0" (il-subst "|" "|1" node)))

(define (il-spc-decode node)
  (il-subst "|1" "|" (il-subst "|0" " " node)))

(define (ml.special-concat-for env sym args)
  (define `delim (nth 3 args))
  (define `for-value
    (c0-for env sym args concat-for-where il-promote identity))

  (define `(for-result value-xform)
    (c0-for env sym args concat-for-where
            il-promote value-xform))

  (or (case delim
        ((PString n value)
         (if (eq? value " ")
             ;; Simple case: single space is what `foreach` adds
             (for-result identity))))

      ;; General case
      (il-spc-decode
       (IBuiltin "subst" [ (IString " ")
                          (il-subst "|" "|1" (c0 delim env))
                          (for-result il-spc-encode) ]))))


;;--------------------------------
;; (cond (TEST BODY)...)
;;--------------------------------

;; -->  (if TEST1 (begin BODY1...)
;;        (if TEST2 (begin BODY2...)
;;          ... ) )

(define cond-where
  "(cond (TEST BODY)...)")

;; Combine a cond clause with its "else" value.
;;   CLAUSE = `(TEST BODY)
;;   ELSE-FORM = value of remaining clauses
(define (cond-wrap clause else-form)
  (case clause
    ((PList _ forms)
     (define `test (first forms))
     (define `body (rest forms))
     (define `is-else (case test ((PSymbol _ name) (eq? name "else"))))
     (if body
         (if is-else
             ;; evaluate to BODY
             (if (not else-form)
                 (begin-block body)
                 (gen-error test "(else ...) is followed by additional clauses"))
             ;; evaluate to (if TEST BODY NODE)
             (PList 0 (append [ (PSymbol 0 "if") test (begin-block body) ]
                              (if else-form [else-form]))))
         ;; no BODY
         (if test
             (err-expected "" nil clause "BODY" cond-where)
             (err-expected "" nil clause "TEST" cond-where))))
    (else
     (err-expected "L" clause nil "(TEST BODY)" cond-where))))


(define (ml.special-cond env sym args)
  (c0 (foldr cond-wrap nil args) env))


;;--------------------------------
;; (global-name SYM)
;;--------------------------------

(define (defn-global-name defn)
  (case defn
    ((EFunc name _ _ _) name)
    ((EVar name _) name)))

(define (ml.special-global-name env sym args)
  (define `var (first args))
  (or (check-argc 1 args sym)
      (case var
        ((PSymbol _ name)
         (let ((global-name (defn-global-name (resolve var env)))
               (name name))
           (if global-name
               (IString global-name)
               (gen-error var "%q is not a global variable" name))))
        (else (err-expected "S" var sym "NAME" "(global-name NAME)")))))


;;--------------------------------
;; (defmacro (NAME ARG...) BODY)
;;--------------------------------

(define defmacro-where
  "(defmacro (NAME ARG...) BODY)")

(define (ml.special-defmacro env sym args)
  (define `what (first args))
  (define `body (rest args))
  (case what
    ((PList _ decl-forms)
     (define `m-name (first decl-forms))
     (define `m-args (rest decl-forms))

     (case m-name
       ((PSymbol _ name)
        ;; compile as a function
        (let ((node (c0 (PList 0 (cons (PSymbol 0 "define") args))
                        env))
              (new-env { =name: (EXMacro (gen-global-name name env) "x") }))
          (IEnv new-env
                ;; discard the env entries returned by `define`
                (case node
                  ((IEnv _ subnode) subnode)
                  (else node)))))
       (else (err-expected "S" m-name sym "NAME" defmacro-where))))
    (else (err-expected "L" what sym "(NAME ARG...)" defmacro-where))))


;;--------------------------------
;; (use MODULE)
;;--------------------------------

(define (ml.special-use env sym args)
  (define `module (first args))

  (or (check-argc 1 args sym)
      (case module
        ((PString _ mod-name)
         (let ((origin (locate-module *compile-file* mod-name))
               (imports (use-module-env mod-name))
               (mod-name mod-name)
               (env env))
           (case (dict-get ErrorMarkerKey imports)
             ;; error loading module
             ((EMarker desc)
              (gen-error "use: module %q %s" mod-name desc))
             ;; success
             (else
              (IEnv imports (ICrumb "use" origin))))))
        (else (err-expected "Q" module sym "MODULE" "(use MODULE)")))))


;;--------------------------------
;; (data NAME CTOR...)
;;--------------------------------

(data Data
      (DataType &word tag
                &word name
                encodings
                &list argnames))

(define data-where
  "(data NAME (CTOR ARG...)...)")

;; Generate a Ctor record, or an Error record (a la parse tree) on failure.
;;
;;   ARGS = [ `(CTOR ARG...) ... ]
;;   FORM = parent form (for error messages)
;;   ARG  = FLAG? NAME
;;   FLAG = "&word"  => value contains no whitespace and is not nil
;;        | "&list"  => value has no leading or trailing whitespace
;;
(define (read-type-r args form tag pattern names flag)
  (define `arg (first args))      ; a form

  ;; Note: We can use list *encoding* only when a member is of list *type*
  ;; AND is the last member.
  (define `(arg-enc flag has-more-args)
    (cond ((eq? "&word" flag) "W")
          ((and (eq? "&list" flag) (not has-more-args)) "L")
          (else "S")))

  (or
   (if (not args)
       (if flag
           (gen-error form "no argument following last flag: %s" flag)
           ;; Done.
           (DataType tag (first pattern) (rest pattern) names)))

   (case arg
     ((PSymbol _ arg-name)
      (or
       (if (not (filter "&%" arg-name))
           ;; argument name
           (read-type-r (rest args) form tag
                        (concat pattern " " (arg-enc flag (word 2 args)))
                        (conj names arg-name)
                        nil))

       (if flag
           (gen-error arg "two type flags supplied for one argument"))

       (if (filter "&list &word" arg-name)
           (read-type-r (rest args) form tag pattern names arg-name))

       (gen-error arg "unknown flag [supported: &list, &word]")))

     (else
      ;; not a symbol
      (err-expected "S" arg form "ARG" data-where)))))


;; Return a `Data` record for a constructor.
;;
;;  FORM = `(CtorName ARG...)
;;
(define (read-type form tag parent)
  (case form
    ((PList _ args)
     (define `sym (first args))        ;; symbol naming the ctor
     (define `tag-form (nth 2 args))   ;; optional explicit tag

     (case sym
       ((PSymbol _ name)
        (case tag-form
          ;; explicit tag provided:
          ((PString _ value)
           (read-type-r (rrest args) form value name [] nil))
          (else
           (read-type-r (rest args) form tag name [] nil))))
       (else (err-expected "S" sym parent "CTOR" data-where))))
    (else (err-expected "L" form parent "(CTOR ...)" data-where))))


;; Return vector of ctor descriptions (see read-type), or a PError record.
;;
(define (read-types parent tag-base ctor-forms ?counter ?prev-ctor ?others)
  (define `index (words counter))
  (define `tag (concat tag-base index))
  (define `all-ctors (append others
                             (if prev-ctor
                                 [prev-ctor])))

  (case prev-ctor
    ((PError _ _) prev-ctor)
    (else
     (if (not ctor-forms)
         all-ctors
         (read-types parent tag-base (rest ctor-forms) (append counter 1)
                    (read-type (first ctor-forms) tag parent)
                     all-ctors)))))


(define (ml.special-data env sym args)
  (define `type (first args))
  (define `flags (get-flags args 1))
  (define `ctor-forms (skip-flags args 1))

  (let ((types
         (case type
           ((PSymbol _ name)
            (read-types sym (concat "!:" name) ctor-forms))
           (else
            (err-expected "S" type sym "NAME" data-where))))
        (scope (if (filter "&public" flags) "x" "p")))
    (begin
      ;; list of tag definitions:  tagname!=CtorName!01W!0L ...
      (define `tag-defs
        (append-for ty types
                    (case ty
                      ((DataType tag name encodings argnames)
                       { =tag: (append name encodings) }))))

      ;; Add record descriptions to the environment
      (define `bindings
        (append-for ty types
                    (case ty
                      ((DataType tag name encodings argnames)
                       { =name: (ERecord encodings scope tag) }))))

      ;; Add tag/pattern bindings to ^tags
      (define `node
        (ICall "^add-tags" [(IString tag-defs)]))

      (or (case types ((PError _ _) types))
          (IEnv bindings node)))))


;;--------------------------------
;; (case VALUE (PATTERN BODY)... )
;;--------------------------------

;; PATTERN = (NAME VAR...) | NAME
(define case-where
  "(case VALUE (PATTERN BODY)...)")

;; Return bindings for arguments in a (CTOR ARG...) pattern.
(define (arg-bindings args encs value-node depth)
  (foreach
   n (indices encs)
   (let& ((enc (word n encs))
          (arg (nth n args))
          (ndx-node (IString (1+ n)))
          (arg-node
           (cond
            ((eq? "S" enc) (ICall "^n" [ndx-node value-node]))
            ((eq? "W" enc) (IBuiltin "word" [ndx-node value-node]))
            (else (IBuiltin "wordlist" [ ndx-node
                                        (IString 99999999)
                                        value-node])))))
         { (symbol-name arg): (EIL depth "-" arg-node) })))


;; Compile a vector of (PATTERN BODY) cases
;;
(define (c0-clauses cases value-node env)
  (for
   c cases    ; c = `(PATTERN BODY)
   (case c
     ((PList pos forms)
      (begin
        (define `pattern (first forms))
        (define `body (rest forms))

        (case pattern
          ;; (SYM BODY)
          ((PSymbol n var-name)
           (c0-block body (append { =var-name:
                                    (EIL (current-depth env) "-" value-node) }
                                  env)))

          ;; ((CTOR ARG...) BODY)
          ((PList n syms)
           (define `ctor-form (first syms))
           (define `ctor-args (rest syms))
           (or
            (let ((defn (resolve ctor-form env))
                  (value-node value-node)
                  (ctor-args ctor-args)
                  (ctor-form ctor-form)
                  (body body))
              (case defn
                ((ERecord encs _ tag)

                 (define `test-node
                   (IBuiltin "filter" [ (IString tag)
                                       (IBuiltin "firstword" [value-node]) ]))
                 (define `bindings
                   (arg-bindings ctor-args encs value-node (current-depth env)))

                 (define `then-node
                   (c0-block body (append bindings env)))

                 (or (check-argc (words encs) ctor-args ctor-form)
                     ;; Success
                     (IBuiltin "if" [ test-node then-node ])))))

            (case ctor-form
              ((PSymbol _ name)
               (gen-error ctor-form "symbol %q does not identify a record type"
                          name))
              (else
               (err-expected "S" ctor-form pattern "CTOR" case-where)))))

          (else (err-expected "L S" pattern c "PATTERN" case-where)))))
     (else (err-expected "L" c nil "(PATTERN BODY)" case-where)))))

(define (case-append-arg node value)
  (case node
    ((IBuiltin name args) (IBuiltin name (conj args value)))
    ;; could be an error, or block containing errors
    (else node)))

(define (case-fold args)
  (if (word 2 args)
      (foldr case-append-arg (last args) (butlast args))
      (first args)))

(define (ml.special-case env sym args)
  (define `value-form (first args))
  (if value-form
      (let ((value (c0 value-form env))
            (args args)
            (env env))
        (case value
          ((PError _ _) value)
          (else (case-fold (c0-clauses (rest args) value env)))))
      ;; no value-form
      (err-expected "" value-form sym "VALUE" case-where)))
