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
  (define `[var-sym value-form retval] args)

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

(define (M.? env sym [func ...func-args])
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
;; (let ((TARGET VALUE)...) BODY)
;;--------------------------------

(define `let-where
  "(let ((TARGET VALUE)...) BODY)")


(define `let-EXTRA    1)
(define `let-NOVALUE  2)
(define `let-NOTARGET 3)
(define `let-BADPAIR  4)
(define `let-NOLIST   5)
(define `let-BADLIST  6)
(define `let-EMPTY    7)
(define `let-NOOPT    8)


(define (let-err form type where)
  (define `descs [
     "extra form after VALUE"       ;; EXTRA
     "missing VALUE"                ;; NOVALUE
     "missing TARGET"               ;; NOTARGET
     "expected (TARGET VALUE)"      ;; BADPAIR
     "missing ((TARGET VALUE)...)"  ;; NOLIST
     "expected ((TARGET VALUE)...)" ;; BADLIST
     "empty ((TARGET VALUE)...)"    ;; EMPTY
     "'?NAME' and '...NAME' cannot be used as TARGET" ;; NOOPT
     ])
  (gen-error form (.. (nth type descs) " in %s") where))


;; Parse/validate ((TARGET VALUE)...) pairs as in let-like expressions.
;; Return a vector, each entry being [TARGET VALUE] or (PError n desc).
;;
(define (parse-pairs pairs-form sym where)
  (case pairs-form
    ((PList n forms)
     (or (for (form forms)
           (case form
             ((PList n pair)
              (define `[target value extra] pair)
              (define `target-is-opt
                (case target
                  ((PSymbol _ name)
                   (filter "?% ...%" name))))
              (cond
               ;; too many/few forms in PList?
               ((filter-out 2 (words pair))
                (if extra
                    (let-err extra let-EXTRA where)
                    (let-err form (if pair let-NOVALUE let-NOTARGET) where)))
               ;; c0-lambda will allow '?NAME', so filter it out here
               (target-is-opt (let-err target let-NOOPT where))
               (else [target value])))
             (_ (let-err form let-BADPAIR where))))
         [(let-err pairs-form let-EMPTY where)]))
    (other
     [(let-err (or other sym)
               (if other let-BADLIST let-NOLIST)
               where)])))


(define (c0-let env body pmap)
  (define `params
    (for ([p v] pmap) p))
  (define `values
    (c0-vec (for ([p v] pmap) v) env))

  (or (first-perror pmap)
      (IFuncall (cons (c0-lambda env params body) values))))


(define (M.let env sym [pairs ...body])
  (c0-let env body (parse-pairs pairs sym let-where)))


;;--------------------------------
;; (let-global ((TARGET VALUE)...) BODY)
;;--------------------------------

(define letg-where
  "(let-global ((TARGET VALUE)...) BODY)")


;; Generate IL for:
;;
;;     (set "VAR" (set "VAR" NEW VALUE) BODY)
;;     7    1     5    2     3   4      6      <-- eval order
;;
;; During BODY (step 6) VAR has value VALUE.  At step 7 VAR is reset to
;; the value it had in step 4 (its previous value).
;;
(define (letting env var-sym value body)
  ;; what & where (last two args) cannot be relevant
  (define `set-new
    (c0-set env var-sym value (c0 var-sym env) nil nil))
  (c0-set env var-sym set-new body nil nil))


(declare (lg* env body tvmap nxmap value pos))


(define (lg*2 env body tvmap nxmap value pos)
  (or
   (first-perror nxmap)

   (if (word 2 nxmap)
       ;; use lambda
       (begin
         (define `inner-env
           (append (depth-marker (.. "." (current-depth env)))
                   env))
         (define `inner
           (lg* inner-env body tvmap nxmap (IArg 1 ".") pos))
         (IFuncall [(ILambda inner) value]))
       ;; single
       (lg* env body tvmap nxmap value pos))))


;; TVMAP = map of remaining [target value] pairs
;; Pertaining to the "current" target/value:
;;   NXMAP = map of [name xtor] pairs
;;   VALUE = IL node
;;   POS = form-index of target
;;
(define (lg* env body tvmap nxmap value pos)
  (cond
   (nxmap
    (define `[nx ...other-nxs] nxmap)
    (case nx
      ((Bind name xtor)
       (letting env (PSymbol pos name) (xtor value)
                (lg* env body tvmap other-nxs value pos)))
      (_ nx)))

   (tvmap
    (define `[[target value-form] ...more-tvs] tvmap)
    (or (first-perror tvmap)
        (lg*2 env body more-tvs
              (parse-target target)
              (c0 value-form env)
              (form-index target))))

   (else
    (c0-block body env))))


(define (M.let-global env sym [pairs ...body])
  (lg* env body (parse-pairs pairs sym letg-where) nil nil nil))


;;--------------------------------
;; (let& ((TARGET VAL)...) BODY)
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
  "(let& ((TARGET VALUE)...) BODY)")


(define (let&-bindings [tv ...more-tvs] depth env ?bindings)
  (if tv
      (case tv
        ((PError _ _)
         {=EnvErrorKey: tv})

        ([target value-form]
         (define `value-il (c0 value-form (._. bindings env)))
         (define `tb (bind-target target "p" depth value-il))
         (let&-bindings more-tvs depth env (append tb bindings))))
      bindings))


(define (M.let& env sym [pairs ...body])
  (let ((bindings (let&-bindings (parse-pairs pairs sym let&-where)
                                 (current-depth env)
                                 env))
        (body body)
        (env env))

    (or (dict-get EnvErrorKey bindings)
        (c0-block body (append bindings env)))))


;;--------------------------------
;; (for TARGET VEC BODY)
;; (append-for TARGET VEC BODY)
;;--------------------------------


(define `(for-arg new-depth)
  (IArg (depth.a new-depth) "."))

(define `(for-node new-depth list body)
  (IFor (depth.a new-depth) list body))

(define `(for-env new-depth env)
  (append (depth-marker new-depth) env))


(define (c0-for2 env bindings depth sym tmpl body out-x where)
  (define `[target list] tmpl)
  (define `has-list (word 2 tmpl))
  (define `body-or-list (if has-list "BODY" (filter "VEC LIST" where)))

  (define `env-in
    (._. bindings
         (for-env depth env)))

  (if bindings
      (if body
          (for-node depth (c0 list env) (out-x (c0-block body env-in)))
          (err-expected "" nil sym body-or-list where))
      (err-expected "S" target sym "TARGET" where)))


;; Compile to IFor.  Delimiter = " ".
;;
;; SYM = `for | `foreach | ...
;; TMPL = [TARGET LIST ?DELIM]
;; BODY = [...EXPR]
;; IN-X = function to apply to IL of the `foreach` variable (prior to the
;;        evaluation of the body)
;; OUT-X = function to apply to IL of the result of the body (for each item)
;;
;; The result is summarized informally as:
;;
;;   (foreach *word* LIST (let ((TARGET (IN-X *word*))) (OUT-X BODY)))
;;
(define `(c0-for env sym tmpl body in-x out-x where)
  (define `[target] tmpl)

  (if tmpl
      (foreach (depth (.. (current-depth env) ";"))
        (define `bindings
          (bind-target target "p" depth (in-x (for-arg depth))))
        (c0-for2 env bindings depth sym tmpl body out-x where))
      (err-expected "S" nil sym "TARGET" where)))


;; For old-style syntax
(define `(c0-for-old env sym args in-x out-x where)
  (c0-for env sym (wordlist 1 2 args) (nth-rest 3 args) in-x out-x where))


(define (M.for env sym args)
  (case (first args)
    ;; New syntax
    ((PList _ tmpl)
     (c0-for env sym tmpl (rest args) il-promote il-demote
             "(for (TARGET VEC) BODY)"))

    ;; Old syntax
    (_  (c0-for-old env sym args il-promote il-demote
                    "(for TARGET VEC BODY)"))))


(define (M.append-for env sym args)
  (define `for-value
    (case (first args)
      ;; New syntax
      ((PList _ tmpl)
       (c0-for env sym tmpl (rest args) il-promote identity
               "(append-for (TARGET VEC) BODY)"))

      ;; Old syntax
      (_ (c0-for-old env sym args il-promote identity
                     "(append-for TARGET VEC BODY)"))))

  (IBuiltin "filter" [ (IString "%") for-value ]))


;;--------------------------------
;; (foreach (TARGET LIST ?DELIM) BODY)
;; (concat-for (TARGET LIST ?DELIM) BODY)
;;--------------------------------

;; Since IFor (Make's foreach) always inserts a space between the result of
;; each iteration, we have to replace that space with the user-supplied
;; delimiter after `foreach` completes.  The complex part is doing this
;; without corrupting the rest of the result or the delimiter itself,
;; and with minimal overhead.
;;
;; In the result of each iteration, we encode "|~" with "||~~" and append
;; "|~" to the end of each element.  After `foreach` completes, "|~<SPC>" is
;; replaced with the (encoded) delimiter and then all "|~" instances are
;; deleted, un-doing the encoding *and* removing any trailing "|~".

(define (c0-cfor2 env sym tmpl body in-x where delim-value)
  (define `(enc str)
    (subst-in-il "|~" "||~~" str))

  (define `(out-x node)
    (IConcat [(enc node) (IString "|~")]))

  (define `(dec eresult)
    (subst-in-il "|~" nil
                 (il-subst (IString "|~ ") (enc delim-value)
                           eresult)))

  (define `(loop-result out-x)
    ;; [TARGET LIST DELIM ...BODY]  ->  [TARGET LIST ...BODY]
    (c0-for env sym tmpl body in-x out-x where))

  (if (eq? delim-value (IString " "))
      ;; Simple `foreach` is all we need
      (loop-result identity)
      ;; General case: apply xenc to words and xdec to result
      (dec (loop-result out-x))))


;; Compile concat-for[each] expresions to to IFor.
;;
(define (c0-cfor env sym tmpl body in-x where)
  (define `delim-value
    (if (word 3 tmpl)
        (c0 (nth 3 tmpl) env)
        (IString " ")))
  (c0-cfor2 env sym tmpl body in-x where delim-value))


(define (M.concat-for env sym args)
  (case (first args)
    ;; New syntax
    ((PList _ tmpl)
     (c0-cfor env sym tmpl (rest args) il-promote
              "(concat-for (TARGET VEC ?DELIM) BODY)"))

    ;; Old syntax
    (_ (begin
         (define `tmpl (wordlist 1 3 args))
         (define `body (nth-rest 4 args))
         (c0-cfor env sym tmpl body il-promote
                  "(concat-for TARGET VEC DELIM BODY)")))))


;; (foreach TARGET LIST BODY)
;; (foreach (TARGET LIST ?DELIM) BODY)

(define (M.foreach env sym args)
  (case (first args)
    ;; New syntax
    ((PList _ tmpl)
     (c0-cfor env sym tmpl (rest args) identity
              "(foreach (TARGET LIST ?DELIM) BODY)"))

    ;; Old syntax
    (_ (c0-for-old env sym args identity identity "(foreach TARGET LIST BODY)"))))


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
    ((PList _ [test ...body])
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
  (define `[what ...body] args)
  (case what
    ((PList _ [m-name ...m-args])
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
;;  FORM = `(CtorName ARGS...)  or `(CtorName "TAG" ARGS...)
;;
(define (read-type form tag parent)
  (case form
    ((PList _ args)
     (define `[sym tag-form ...tag-args] args)

     (case sym
       ((PSymbol _ name)
        (case tag-form
          ;; explicit tag provided:
          ((PString _ value)
           (read-type-r tag-args form value name [] nil))
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
    ;; list of tag definitions:  tagname!=CtorName!01W!0L ...
    (define `tag-defs
      (append-for (ty types)
        (case ty
          ((DataType tag name encodings argnames)
           { =tag: (append name encodings) }))))

    ;; Add record descriptions to the environment
    (define `bindings
      (append-for (ty types)
        (case ty
          ((DataType tag name encodings argnames)
           { =name: (ERecord scope encodings tag) }))))

    ;; Add tag/pattern bindings to ^tags
    (define `node
      (ICall "^at" [(IString tag-defs)]))

    (or (case types ((PError _ _) types))
        (IEnv bindings node))))


;;--------------------------------
;; (case VALUE (PATTERN BODY)... )
;;--------------------------------

;; PATTERN = (NAME VAR...) | NAME
(define case-where
  "(case VALUE (PATTERN BODY)...)")

(data Case
  (Clause tag nxmap &list body))


;;   (Any target body) | (Match ctor-sym ctor-args body)
;;   (Any target body) | (Match tag encs ctor-args body-form)  <- env
;;
;;    tag  nxmap     body
;;    tag  bindings  body    <-- (bind clauses env is-word)
;;    tag  code
;; <merge>
;;    tag  code
;; <fold>
;;    code
;;
;; (c0-block body (append {=var-name: value-defn} env)))

(define (clause-or-error tag nxmap body)
  (or (first-perror nxmap)
      (Clause tag nxmap body)))


(define (enc-xtor encoding index)
  (cond
   ((filter "S" encoding) (lambda (il) (il-nth index il)))
   ((filter "W" encoding) (lambda (il) (il-word index il)))
   (else (lambda (il) (il-nth-rest index il)))))


;; Construct NXMap for record fields
;;
(define (record-nxmap targets encs)
  (append-for (n (indices targets))
    (define `target (nth n targets))
    (define `enc (nth n encs))
    (parse-target target (enc-xtor enc (1+ n)))))


;; [...FORMS] -> (Clause tag nxmap body) | (PError ...)
;;
(define (case-parse cases env)
  (for (c cases)
    ;; Expect c = `(PATTERN BODY)
    (case c
      ((PList pos forms)
       (define `[pattern ...body] forms)

       (if (word 2 forms)
           (case pattern
             ;; pattern = (CTOR ARGS...)
             ((PList n [ctor-name ...ctor-args])
              ;; look up ctor definition
              (case (resolve ctor-name env)
                ((ERecord _ encs tag)
                 (or (check-arity (words encs) ctor-args ctor-name)
                     (clause-or-error tag (record-nxmap ctor-args encs) body)))
                (_ (gen-error ctor-name "expected a record constructor name"))))

             ;; pattern = TARGET
             (target
              (clause-or-error "" (parse-target target) body)))
           ;; no BODY (maybe no PATTERM)
           (err-expected nil nil c (if forms "BODY" "PATTERN") case-where)))

      (_ ;; bad clause
       (err-expected "L" c nil "(PATTERN BODY)" case-where)))))


;; VALUE = IL for the value being matched
;;
(define (case-cb clauses value env depth)
  (for (c clauses)
    (case c
      ((Clause tag nxmap body)
       (define `bindings (bind-nxmap nxmap "p" depth value))
       (define `code (c0-block body (append bindings env)))
       [tag code])

      (err
       ["" err]))))


;; [C] -> [ [pat code]... ]
(define `(case-bodies clauses value env)
  (case-cb clauses value env (current-depth env)))


;; Combine sequential cases that have identical body IL
;;
(define (case-merge clauses)
  (define `[[tag1 il1] [tag2 il2] ...more] clauses)

  ;; If t2 == nil, return nil.  Otherwise, join with a space delimiter.
  (define `(merge-tags t1 t2)
    (addprefix (.. t1 " ") t2))

  (if (word 2 clauses)
      (if (eq? il1 il2)
          (case-merge (cons [(merge-tags tag1 tag2) il1] more))
          (append (word 1 clauses) (case-merge (rest clauses))))
      clauses))


;; Construct code to test each tag, and fold clauses together:
;;
;;   [["T1" BODY1] ["T2" BODY2] ...]
;;     -->  (if Match[T1] BODY1 (if Match[T1] BODY2 ...))
;;
;; Match[TAG] => a filter expression.  (make-filter-args TAG) returns the
;; args as a vector of IL nodes.
;;
(define (case-fold clauses make-filter-args)
  (define `[[tag body] ...more] clauses)

  ;; If we have a nil tag (which matches match everything) *and* subsequent
  ;; clauses, we include the code for the subsequent clauses anyway, so that
  ;; any errors in that code will be reported.
  (if (or tag more)
      (IBuiltin "if" [ (if tag
                           (IBuiltin "filter" (make-filter-args tag))
                           (IString 1))
                       body
                       (case-fold more make-filter-args) ])
      (or body
          (IString ""))))


;; True if NODE should be evaluated only once when used as the value of a
;; case statement.  Any impure expression must be evaluated only once.
;; Additionally, we choose to eval complex expressions only once.
;;
(define `(eval-only-once? node)
  (define `(call-once? name nodes)
    (or (filter-out "^u ^n wordlist" name)
        (word 1 (foreach (n nodes)
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


;; (TEST-CTOR tag) => expression that tests value for TAG
;;
(define (c0-case env cases depth value ?filter-args-ovr)
  (cond
   ((and (not filter-args-ovr)
         (eval-only-once? value))
    ;; Prevent multiple evaluation of a complex expression: wrap in
    ;; "(foreach (X (to-word EXPR)) ...)"
    (define `f-depth (.. depth ";"))
    (define `f-arg (for-arg f-depth))
    (define `f-value (il-promote f-arg))
    (define `f-env (for-env f-depth env))
    (define `(f-filter-args tags)
      [ (IString (.. (subst "!0" "!0% " [tags]) "!0%"))
        (IConcat [f-arg (IString "!0")]) ])


    (for-node f-depth (il-demote value)
              (c0-case f-env cases f-depth f-value f-filter-args)))

   (else
    (define `clauses (case-parse cases env))
    (define `pairs (case-cb clauses value env depth))
    (define `merged (case-merge pairs))
    (define `(filter-args tags)
      [(IString tags) (IBuiltin "word" [(IString 1) value])])
    (case-fold merged (or filter-args-ovr filter-args)))))


(define (M.case env sym args)
  (define `[record ...clauses] args)
  (if args
      (c0-case env clauses (current-depth env) (c0 record env))
      (err-expected nil nil sym "VALUE" case-where)))
