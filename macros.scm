;;--------------------------------------------------------------
;; macros.scm : standard macro definitions
;;--------------------------------------------------------------

(require "core.scm")
(require "parse.scm")
(require "gen.scm")
(require "gen0.scm")


;; Functions named "M.<NAME>" implement special forms.  They are
;; executed at compile-time when a list form starting with the corresponding
;; NAME is encountered.  These functions are passed a list of arguments
;; (each an AST) and an environment.  They return an IL tree.
;;
;; (M.NAME env sym args)
;;    ENV = environment
;;    SYM = the symbol naming the special form (e.g. `print)
;;    ARGS = forms passed to the special form


;;--------------------------------
;; (when COND EXPR...)
;;--------------------------------

(define (M.when env sym args)
  (or (check-arity "2+" args sym)
      (IBuiltin "if" [ (c0 (first args) env) (c0-block (rest args) env) ])))


;;--------------------------------
;; (print args...)
;;--------------------------------

(define (M.print env sym args)
  (IBuiltin "info" [ (IConcat (c0-vec args env)) ]))


;;--------------------------------
;; (current-env)
;;--------------------------------

(define (M.current-env env sym args)
  (IString env))


;;--------------------------------
;; (current-file)
;;--------------------------------

;; Evaluates to literal string containing source file name.
;;
(define (M.current-file env sym args)
  (or (check-arity 0 args sym)
      (IWhere nil)))


;;--------------------------------
;; (current-file-line)
;;--------------------------------

;; "FILE:LINE:COL" at which the macro containing this line is invoked.
;;
(define (M.current-file-line env sym args)
  (or (check-arity 0 args sym)
      (IWhere (form-index sym))))


;;--------------------------------
;; (concat FORM...)
;; (.. FORM...)
;; (._. FORM...)
;;--------------------------------

(define (M.concat env sym args)
  (il-concat (c0-vec args env)))


(define (M... env sym args)
  (il-concat (c0-vec args env)))


(define (M.._. env sym args)
  (il-concat (c0-vec (intersperse (PString 0 " ") args) env)))


;;--------------------------------
;; (subst FROM TO {FROM TO}... STR)
;;--------------------------------

(define (subst-x strs value)
  (if strs
      (subst-x (rrest strs) (IBuiltin "subst" (conj (wordlist 1 2 strs) value)))
      value))

(define (M.subst env sym args)
  (if (filter "%2 %4 %6 %8 %0 1" (words args))
      (gen-error sym
                 "(subst {FROM TO}+ STR) accepts 2n+1 arguments, not %s"
                 (words args))
      (subst-x (c0-vec (butlast args) env) (c0 (last args) env))))


;;--------------------------------
;; (set SYM VALUE [RETVAL])
;;--------------------------------

(define (c0-set env sym value-node retval-node what where)
  (define `(il-set setter varname)
    (ICall setter (append [ (IString varname) value-node ]
                          (if retval-node
                              [retval-node]))))
  (case sym
    ((PSymbol pos var-name)
     (case (resolve sym env)
       ((EVar _ name) (il-set "^set" name))
       ((EFunc _ name _) (il-set "^fset" name))
       (else
        (gen-error sym "`%s` is not a global variable" (symbol-name sym)))))
    (else
     (err-expected "S" sym nil what where))))


(define (M.set env sym args)
  (define `var-sym (first args))
  (define `value-form (nth 2 args))
  (define `retval (nth 3 args))

  (or (check-arity "2 3" args sym)
      (c0-set env
              var-sym
              (c0 value-form env)
              (if retval (c0 retval env))
              "NAME"
              "(set NAME VALUE [RETVAL])")))


;;--------------------------------
;; (? <fn> ...args...)
;;--------------------------------

(define (M.? env sym args)
  (define `func (first args))
  (define `func-args (rest args))
  (define `(trace ctor name)
    (ctor "^t" (cons (IString name) (c0-vec func-args env))))

  (case (resolve func env)
    ((EFunc _ name _) (trace ICall name))
    ((EBuiltin _ name _) (trace IBuiltin name))
    (defn
      (if (eq? "-" defn)
          (err-expected "S" func sym "FUNC" "(? FUNC ARGS...)")
          (if defn
              (gen-error func "FUNC in (? FUNC ...) is not traceable")
              (gen-error func "undefined variable: `%s`" (symbol-name sym)))))))


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
                    (gen-error extra "extra form after value in %s" where))
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
(define (M.let env sym args)
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


(define (M.let-global env sym args)
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


;; ENV = environment for compiling macro values
;; DEPTH = nesting depth of ENV
;;
(define (let&-env pairs env)
  (define `p (first pairs))
  (define `p-binding
    (case (first p)
      ((PSymbol _ name)
       { =name: (EIL "p" (current-depth env) (c0 (nth 2 p) env)) })))

  (if pairs
      (let&-env (rest pairs) (append p-binding env))
      env))


(define (M.let& env sym args)
  (case (read-pairs (first args) sym let&-where)
    ((PError pos str)
     (PError pos str))
    (pairs
     (c0-block (rest args) (let&-env pairs env)))))


;;--------------------------------
;; (for PAT VEC BODY)
;; (append-for PAT VEC BODY)
;;--------------------------------


(define `(for-arg new-depth)
  (IArg (depth.a new-depth) "."))

(define `(for-node new-depth list body)
  (IFor (depth.a new-depth) list body))

(define `(for-env new-depth env)
  (append (depth-marker new-depth) env))


(define (c0-for2 env bindings depth sym tmpl body body-x where)
  (define `pattern (nth 1 tmpl))
  (define `list (nth 2 tmpl))
  (define `has-list (word 2 tmpl))
  (define `body-or-list (if has-list "BODY" (filter "VEC LIST" where)))

  (define `env-in
    (._. bindings
         (for-env depth env)))

  (if bindings
      (if body
          (for-node depth (c0 list env) (body-x (c0-block body env-in)))
          (err-expected "" nil sym body-or-list where))
      (err-expected "S" pattern sym "PAT" where)))


;; Compile to IFor.  Delimiter = " ".
;;
;; SYM = `for | `foreach | ...
;; TMPL = [PAT LIST ?DELIM]
;; BODY = [...EXPR]
;; WORD-X = function to apply to IL of each word prior to bindings
;; BODY-X = function to apply to IL of `foreach` result
;;
;; The result is summarized informally as:
;;
;;   (foreach *word* LIST (let ((PAT (WORD-X *word*))) (BODY-X BODY)))
;;
(define `(c0-for env sym tmpl body word-x body-x where)
  (define `pattern (nth 1 tmpl))

  (foreach
      depth (.. (current-depth env) ";")

      (define `bindings
        (pat-bindings pattern (word-x (for-arg depth)) depth))
      (c0-for2 env bindings depth sym tmpl body body-x where)))


;; For old-style syntax
(define `(c0-for-old env sym args word-x body-x where)
  (c0-for env sym (wordlist 1 2 args) (nth-rest 3 args) word-x body-x where))


(define (M.for env sym args)
  (case (first args)
    ;; New syntax
    ((PList _ tmpl)
     (c0-for env sym tmpl (rest args) il-promote il-demote
             "(for (PAT VEC) BODY)"))

    ;; Old syntax
    (_  (c0-for-old env sym args il-promote il-demote
                    "(for PAT VEC BODY)"))))


(define (M.append-for env sym args)
  (define `for-value
    (case (first args)
      ;; New syntax
      ((PList _ tmpl)
       (c0-for env sym tmpl (rest args) il-promote identity
               "(append-for (PAT VEC) BODY)"))

      ;; Old syntax
      (_ (c0-for-old env sym args il-promote identity
                     "(append-for PAT VEC BODY)"))))

  (IBuiltin "filter" [ (IString "%") for-value ]))


;;--------------------------------
;; (foreach (PAT LIST ?DELIM) BODY)
;; (concat-for (PAT LIST ?DELIM) BODY)
;;--------------------------------

;; Since IFor (Make's foreach) always inserts a space between the result of
;; each iteration, we have to replace that space with the user-supplied
;; delimiter after `foreach` completes.  The complex part is doing this
;; without corrupting the rest of the result or the delimiter itself,
;; and with minimal overhead.
;;
;; We do this by encoding the result of each iteration with (subst "~" "~~x"
;; ...), and then use "~x~x" as a marker for the end of each element.  (It
;; cannot appear within encoded text.)  After `foreach` completes, "~x~x "
;; identifies gaps between elements (to be replaced with delim), and then
;; all "~x" instances can be deleted, un-doing the encoding *and* removing
;; the "~x~x" marker from the last word.


(define (c0-cfor2 env sym tmpl body word-x where delim-value)
  (define `(xenc node)
    (subst-in-il "~" "~~x" node))

  (define `(xenc-each node)
    (IConcat [(xenc node) (IString "~x~x")]))

  (define `(xdec out delim)
    (subst-in-il "~x" nil (il-subst (IString "~x~x ") (xenc delim) out)))

  (define `(loop-result body-x)
    ;; [PAT LIST DELIM ...BODY]  ->  [PAT LIST ...BODY]
    (c0-for env sym tmpl body word-x body-x where))

  (or (case (delim-value)
        ((IString str)
         (if (eq? str " ")
             ;; Simple `foreach` is all we need
             (loop-result identity))))

      ;; General case: apply xenc to words and xdec to result
      (xdec (loop-result xenc-each) delim-value)))


;; Compile concat-for[each] expresions to to IFor.
;;
(define (c0-cfor env sym tmpl body word-x where)
  (define `delim-value
    (if (word 3 tmpl)
        (c0 (nth 3 tmpl) env)
        (IString " ")))
  (c0-cfor2 env sym tmpl body word-x where delim-value))


(define (M.concat-for env sym args)
  (case (first args)
    ;; New syntax
    ((PList _ tmpl)
     (c0-cfor env sym tmpl (rest args) il-promote
              "(concat-for (PAT VEC ?DELIM) BODY)"))

    ;; Old syntax
    (_ (begin
         (define `tmpl (wordlist 1 3 args))
         (define `body (nth-rest 4 args))
         (c0-cfor env sym tmpl body il-promote
                  "(concat-for PAT VEC DELIM BODY)")))))


;; (foreach PAT LIST BODY)
;; (foreach (PAT LIST ?DELIM) BODY)

(define (M.foreach env sym args)
  (case (first args)
    ;; New syntax
    ((PList _ tmpl)
     (c0-cfor env sym tmpl (rest args) identity
              "(foreach (PAT LIST ?DELIM) BODY)"))

    ;; Old syntax
    (_ (c0-for-old env sym args identity identity "(foreach PAT LIST BODY)"))))


;;--------------------------------
;; (cond (TEST BODY)...)
;;--------------------------------

;; -->  (if TEST1 (begin BODY1...)
;;        (if TEST2 (begin BODY2...)
;;          ... ) )

(define cond-where
  "(cond (TEST BODY)...)")


;; Make a valid expression from a vector of forms.
;;
(define (begin-block forms)
  (if (filter 1 (words forms))
      (first forms)
      (PList 0 (cons (PSymbol 0 "begin") forms))))


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


(define (M.cond env sym args)
  (c0 (foldr cond-wrap nil args) env))


;;--------------------------------
;; (native-name SYM)
;;--------------------------------

(define (defn-native-name defn)
  (case defn
    ((EFunc _ name _) name)
    ((EVar _ name) name)
    ((EBuiltin _ name _) name)))

(define (M.native-name env sym args)
  (define `var (first args))
  (or (check-arity 1 args sym)
      (case var
        ((PSymbol _ name)
         (let ((global-name (defn-native-name (resolve var env)))
               (name name))
           (if global-name
               (IString global-name)
               (gen-error var "`%s` is not a global variable" name))))
        (else (err-expected "S" var sym "NAME" "(native-name NAME)")))))


;;--------------------------------
;; (defmacro (NAME ARGS POS) BODY)
;;--------------------------------

(define defmacro-where
  "(defmacro (NAME ARGS [POS]) BODY)")

(define (M.defmacro env sym args)
  (define `what (first args))
  (define `body (rest args))
  (case what
    ((PList _ decl-forms)
     (define `m-name (first decl-forms))
     (define `m-args (rest decl-forms))

     (case m-name
       ((PSymbol _ name)
        ;; compile as a function
        (IEnv { =name: (EXMacro "x" (gen-native-name name env)) }
              ;; discard the env entries returned by `define`
              (case (c0 (PList 0 (cons (PSymbol 0 "define") args)) env)
                ((IEnv _ subnode) subnode)
                (other other))))
       (else (err-expected "S" m-name sym "NAME" defmacro-where))))
    (else (err-expected "L" what sym "(NAME ARG...)" defmacro-where))))


;;--------------------------------
;; (data NAME CTOR...)
;;--------------------------------

;; A TAG is a string stored in the first word of a constructed record,
;; differentiating it from other records, and from vectors.  Each tag begins
;; with "!:", which cannot appear in any of SCAM's standard subordinate data
;; types (vectors, dictionaries, numbers).  Record types are therefore
;; disjoint from each other and from subordinate types.  (Although vectors
;; and numbers are not disjoint from each other... 1 == [1] == [[1]].)
;;
;; Subsequent words in the record describe the members of the record.  An
;; ENCODING describes how arguments are encoded in words:
;;
;;      "W" => word   => extract using $(word N ...)
;;      "S" => string => extract using $(nth N ...)
;;      "L" => list   => extract using $(nth-rest N ...)
;;
;; A PATTERN contains the constructor name and the member encodings:
;;
;;    [CTORNAME ENCODING...]
;;
;; Global variable `^tags` holds a dictionary that maps TAGs to PATTERNs for
;; all constructors whose definitions have been executed.


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
                        (._. pattern (arg-enc flag (word 2 args)))
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
  (define `tag (.. tag-base index))
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


(define (M.data env sym args)
  (define `type (first args))
  (define `flags (get-flags args))
  (define `ctor-forms (skip-flags args))

  (let ((types
         (case type
           ((PSymbol _ name)
            (read-types sym (.. "!:" name) ctor-forms))
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
                       { =name: (ERecord scope encodings tag) }))))

      ;; Add tag/pattern bindings to ^tags
      (define `node
        (ICall "^at" [(IString tag-defs)]))

      (or (case types ((PError _ _) types))
          (IEnv bindings node)))))


;;--------------------------------
;; (case VALUE (PATTERN BODY)... )
;;--------------------------------

;; PATTERN = (NAME VAR...) | NAME
(define case-where
  "(case VALUE (PATTERN BODY)...)")


;; Extract a member from a record (in the value of an EIL binding)
;;
;; DEFN = an EIL binding for a record.
;; NDX = index (1-based) of the member.
;; ENCODING = "S", "W", or "L"
;; Result = an EIL binding for the member.
;;
(define `(extract-member defn ndx encoding)
  (define `il-ndx (IString (1+ ndx)))
  (case defn
    ((EIL scope depth node)
     (EIL scope depth
          (cond
           ((eq? "S" encoding) (ICall "^n" [il-ndx node]))
           ((eq? "W" encoding) (IBuiltin "word" [il-ndx node]))
           (else (IBuiltin "wordlist" [il-ndx (IString 99999999) node])))))))


(define (member-bindings args encodings value-defn)
  (foreach
      n (indices encodings)
      { (symbol-name (nth n args)):
         (extract-member value-defn n (word n encodings))}))


;; Compile a vector of (PATTERN BODY) cases
;;
;; FILTER-VALUE = IL record to use for the value in filter expressions
;; VALUE-DEFN = an environment entry that evaluates to the value
;; IS-WORD = true when FILTER-VALUE is an automatic variable containing
;;     (demote value); otherwise FILTER-VALUE is the actual value.
;;
(define (c0-clauses cases filter-value value-defn is-word env)
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
           (c0-block body (append {=var-name: value-defn} env)))

          ;; ((NAME ARGS...) BODY)
          ((PList n syms)
           (define `ctor-name (first syms))
           (define `ctor-args (rest syms))
           (or
            (case (resolve ctor-name env)
              ((ERecord _ encs tag)
               (define `test-node
                 (IBuiltin
                  "filter"
                  (if is-word
                      [(IString [(.. tag " %")])
                       (IConcat [filter-value (IString "!0")])]
                      [(IString tag)
                       (IBuiltin "word" [(IString 1) filter-value])])))

               (define `bindings
                 (member-bindings ctor-args encs value-defn))

               (define `then-node
                 (c0-block body (append bindings env)))

               (or (check-arity (words encs) ctor-args ctor-name)
                   ;; Success
                   (IBuiltin "if" [ test-node then-node ]))))

            (case ctor-name
              ((PSymbol _ name)
               (gen-error ctor-name "symbol `%s` is not a record constructor"
                          name))
              (else
               (err-expected "S" ctor-name pattern "CTOR" case-where)))))

          (else (err-expected "L S" pattern c "PATTERN" case-where)))))
     (else (err-expected "L" c nil "(PATTERN BODY)" case-where)))))


(define (case-append-arg node value)
  (case node
    ((IBuiltin name args) (IBuiltin name (conj args value)))
    ;; could be an error, or block containing errors
    (else node)))


;; Collapse sequence of IF expressions...
;;   [(IF C A) (IF C2 A2) ...] --> (IF C A (IF C2 A2 ...))
;;
(define (case-fold args)
  (if (word 2 args)
      (foldr case-append-arg (last args) (butlast args))
      (first args)))


;; If NODE matches `(if (filter "P" V) T)` then return [P V T], else nil.
;;
(define (clause-pvt node)
  (case node
    ((IBuiltin if? if-args)
     (and (filter "if" if?)
          (not (word 3 if-args))
          (case (first if-args)
            ((IBuiltin filter? f-args)
             (if (filter "filter" filter?)
                 (case (first f-args)
                   ((IString p)
                    [p (nth 2 f-args) (nth 2 if-args)])))))))))


;; CLAUSE1 = first clause
;; PVT1 = (clause-pvt CLAUSE)
;; CLAUSE2 = second clause
;; PVT2 = (clause-pvt CLAUSE2)
;; IN = rest of clauses
;;
(define (clauses-merge-loop clause1 pvt1 clause2 pvt2 in)
  (define `(recur clause pvt)
    (clauses-merge-loop clause pvt
                        (first in) (clause-pvt (first in))
                        (rest in)))

  (cond
   ;; merge
   ((and pvt1 (eq? (rest pvt1) (rest pvt2)))
    (define `p (._. (first pvt1) (first pvt2)))
    (define `v (nth 2 pvt1))
    (define `t (nth 3 pvt1))
    (recur (IBuiltin "if" [ (IBuiltin "filter" [(IString p) v]) t])
           (cons p (rest pvt1))))

   ;; shift
   (clause2
    (append [clause1] (recur clause2 pvt2)))

   ;; done
   (else
    [clause1])))


;; Merge adjacent clauses where the following applies:
;;      clause = (if (filter "A" V) T)
;; next clause = (if (filter "B" V) T)
;;     result -> (if (filter "A B" V) T)
;;
(define (clauses-merge clauses)
  (if (word 2 clauses)
      (clauses-merge-loop (first clauses) (clause-pvt (first clauses))
                          (nth 2 clauses) (clause-pvt (nth 2 clauses))
                          (nth-rest 3 clauses))
      clauses))



;; True if NODE should be evaluated only once when used as the value of a
;; case statement.  Any impure expression must be evaluated only once.
;; Additionally, we choose to eval complex expressions only once.
;;
(define `(eval-only-once? node)
  (define `(call-once? name nodes)
    (or (filter-out "^u ^n wordlist" name)
        (word 1 (foreach n nodes
                         (case (promote n)
                           ((IArg _ _) nil)
                           ((IVar _) nil)
                           ((IString _) nil)
                           (else 1))))))
  (case node
    ((IArg _ _) nil)
    ((IVar _) nil)
    ((IString _) nil)
    ((IBuiltin name nodes) (call-once? name nodes))
    ((ICall name nodes) (call-once? name nodes))
    (else 1)))


(define (c0-case cases depth value value-defn sym env)
  (cond
   ((case value ((PError _ _) 1))
    value)

   ((eval-only-once? value)
    ;; Prevent multiple evaluation of a complex expression: wrap in
    ;; "(foreach X (to-word EXPR) ...)"
    (define `f-depth (.. depth ";"))
    (define `f-filter-value (for-arg f-depth))
    (define `f-value-defn (EIL "p" f-depth (il-promote (for-arg f-depth))))
    (define `f-body
      (c0-case cases f-depth f-filter-value f-value-defn sym
               (for-env f-depth env)))

    (for-node f-depth (il-demote value) f-body))

   (else
    (define `defn (or value-defn (EIL "p" depth value)))
    (define `clauses (c0-clauses cases value defn (if value-defn 1) env))
    (case-fold (clauses-merge clauses)))))


(define (M.case env sym args)
  (if args
      (c0-case (rest args) (current-depth env) (c0 (first args) env)
               nil sym env)
      ;; no value-form
      (err-expected nil nil sym "VALUE" case-where)))
