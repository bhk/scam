;--------------------------------------------------------------
;; gen : Environment and IL types; code generation utilities
;;--------------------------------------------------------------

(require "core.scm")
(require "parse.scm")

;; Globals used in code generation
;; -------------------------------

;; *compile-subject* contains the penc-encoded SCAM source being compiled
(declare *compile-subject* &public)

;; *compile-file* is the name of the source file being compiled.
(declare *compile-file* &public)

;; When true, the compiler operates in boot mode: no builtin modules will be
;; used, and globals in generated code will be prefixed with "~".
(define *is-boot* &public nil)


;; IL Records
;; ----------
;;
;; "IL" is an intermediate language encoded as records of the IL data type,
;; defined below.  IArg, IFuncall, and ILamda records embody the untyped
;; lambda calculus, but with multiple positional arguments to each function.
;; Further terms deal with the "primordial" data types and functionality of
;; the target VM, GNU Make 3.81, wherein all values (including functions)
;; are strings.  A `nil` value in IL represents an empty string.
;;
;; IArg: References a local variable -- a lambda argument of automatic
;;     (foreach) variable.  UPS is a de Bruijn index in string form: "."
;;     refers to an argument of the current (nearest enclosing) function or
;;     an automatic variable bound in the current function; ".." refers to
;;     locals in the parent function, and so on.
;;
;;     If the local variable is a function argument, NAME is a positional
;;     argument index (decimal), which may be followed by "+" (in which case
;;     the IArg denotes a vector of all arguments beginning at the
;;     positional index).
;;
;;     If the local is a foreach iterator, NAME is one or more ";"
;;     characters.  The top-most foreach within a lambda is assigned the
;;     name ";", a nested foreach (within the same lambda) is assigned ";;",
;;     and so on.
;;
;; ILambda: An anonymous function whose body is NODE, an IL record.
;;
;; IFuncall: Calls an anonymous function with a vector of arguments.
;;
;; IFor: A GNU Make `foreach` sstatement, where NAME is the automatic
;;     variable name (";", ";;", ";;;", depending on nesting level within
;;     the current lambda).  LIST and BODY are IL records.
;;
;; IBlock: An IBlock is a sequences of expressions, as in a `begin`
;;     expression or a function body.  All code within a block is executed,
;;     and the return values are discarded for all but the last sub-node.
;;
;; IWhere: IWhere will be expanded to the source file name, and, if POS is
;;     non-nil, a ":LINE" suffix.  When IWhere occurs within a macro, POS
;;     will be rewritten to reflect where the macro was invoked.
;;
;; ICrumb: An ICrumb holds a name/value pair that will be passed through to
;;     the `compile-text` output without affecting the behavior of the
;;     generated code.
;;
;; IEnv: Some expressions in a block context create new bindings for the
;;     environment.  In those cases, the phase 0 compilation of those
;;     expressions return an IEnv record containing the code *and* the new
;;     bindings.  These are "unwrapped" by c0-block-cc and replaced with
;;     NODE.  If phase 1 sees an IEnv record, it ignores the env field.
;;
;; Errors
;;
;;    PError records may occur where IL records may appear.  See parse.scm.


(data IL
  &public
  (IArg     &word ndx  &word ups)       ; "$(ndx)"
  (ILambda  node)                       ; (lambda-quote (c1 NODE))
  (IFuncall &list nodes)                ; "$(call ^Y,NODES...)"
  (IFor     &word name list &list body) ; "$(foreach NAME,LIST,BODY)"
  (IString  value)                      ; "value"
  (IVar     &word name)                 ; "$(name)"
  (IBuiltin &word name  &list nodes)    ; "$(name NODES...)"
  (ICall    &word name  &list nodes)    ; "$(call name,NODES...)"
  (IConcat  &list nodes)                ; "VALUES..."
  (IBlock   &list nodes)                ; "$(if NODES,,)"
  (IWhere   &word pos)                  ; "value"
  (ICrumb   &word key value)            ; <crumb>
  (IEnv     env &list node))            ; used during phase 0


;; Environment Records
;; -------------------
;;
;; The environment is a stack of bindings: a dictionary with newer
;; (lexically closer) bindings toward the beginning.  When used as a
;; dictionary, it maps a symbol name to its in-scope *definition*.
;;
;; SCOPE describes the scope and origin of top-level bindings:
;;     "i" => defn was imported
;;     "p" => defn is private
;;     "x" => public (exported)
;;
;; NAME = the actual (global) name of the function/variable or builtin
;;     function.
;;
;; ARITY = how many arguments are required by the function, in the form of a
;;    list of valid argument counts, or `N+` for "N or more".
;;
;; DEPTH = (.. LDEPTH ADEPTH).  For EMacro and EIL, this describes
;;     the context in which the associated IL node is valid.  (Any
;;     `ups` values within the IL node are relative to this DEPTH.)
;;
;; LDEPTH = the number of lambdas enclosing the IL node, encoded as one "."
;;     for each lambda.  LDEPTH=`nil` refers to auto variables bound outside
;;     any lambda, LDEPTH="." refers to arguments of a top-level lambda or
;;     auto variables bound in a top-level lambda, and so on.
;;
;; ADEPTH = the number of enclosing `foreach` constructs in the current
;;     (lowest) enclosing lambda: one ";" for each `foreach`.
;;
;; IL = the macro definition, an IL record.
;;
;; EMarker values embed contextual data other than variable bindings.  They
;; use keys that begin with ":" to distinguish them from variable names.

(data EDefn
  &public
  (EVar     &word scope &word name)                  ;; simple global var
  (EFunc    &word scope &word name arity)            ;; recursive global var
  (EMacro   &word scope &word depth arity &list il)  ;; compound macro
  (EIL      &word scope &word depth &list il)        ;; symbol macro
  (EXMacro  &word scope &word name)                  ;; executable macro
  (ERecord  &word scope encs tag)                    ;; data record type
  (EBuiltin &word scope &word name arity)            ;; builtin function
  (EMarker  &word data))                             ;; marker


(define `(EDefn.scope defn)
  &public
  (word 2 defn))


(define `(EDefn.set-scope rec scope)
  &public
  (._. (word 1 rec) scope (nth-rest 3 rec)))


(define `(EDefn.arg name depth)
  &public
  (EIL "p" depth (IArg name ".")))


;; Not a legal symbol name; this key is used to distinguish EMarker records
;; used to convey current lambda recursion depth.
;;
(define `DepthMarkerKey
  ":")


(define `(depth-marker depth)
  &public
  { =DepthMarkerKey: (EMarker depth) })


(define `EnvErrorKey
  &public
  ";")


;; Merge consecutive (IString ...) nodes into one node.  Retain all other
;; nodes.
;;
(define (il-merge-strings nodes accum)
  (case (first nodes)
    ((IString value)
     (il-merge-strings (rest nodes) (.. accum value)))
    (else
     (append (if accum
                 [(IString accum)])
             (word 1 nodes)
             (if (word 2 nodes)
                 (il-merge-strings (rest nodes) ""))))))


(define (il-flatten nodes)
  (append-for (node nodes)
    (case node
      ((IConcat children)
       (il-flatten children))
      (else [node]))))


;; Concatenate nodes in IL domain.
;;
(define (il-concat nodes)
  &public
  (let ((nodes-out (il-merge-strings (il-flatten nodes) "")))
    (if (word 2 nodes-out)
        (IConcat nodes-out)
        (first nodes-out))))


;; Demote in IL domain
(define (il-demote node)
  &public
  (or (case node
        ((IString value) (IString (word 2 node)))
        ((ICall name args) (if (eq? name "^u")
                              (first args))))
      (ICall "^d" [node])))


;; Promote in IL domain
(define (il-promote node)
  &public
  (ICall "^u" [ node ]))


;; Construct an IL node that evaluates to a vector.  NODES is a vector of IL
;; nodes, one for each element.
;;
(define (il-vector nodes)
  &public
  (il-concat
   (intersperse (IString " ")
                (for (n nodes)
                  (il-demote n)))))


;; NODE is IL; A and B are actual strings!
;;
(define (subst-in-il a b node)
  &public
  (case node
    ((IString value) (IString (subst a b value)))
    (else (IBuiltin "subst" [ (IString a) (IString b) node ]))))


;; A, B, and C are all IL nodes.
;;
(define (il-subst a b c)
  &public
  (or (case a
        ((IString sa)
         (case b
           ((IString sb)
            (subst-in-il sa sb c)))))
      (IBuiltin "subst" [a b c])))


(define `(il-dict-key node)
  (ICall "^dk" [node]))


(define `(il-dict-value node)
  (ICall "^dv" [node]))


;; When KEY is a constant string, we can emit this more efficient code
;;
(define `(il-dict-get key dict-node)
  (define `pat
    (IString (.. (subst "%" "!8" [key]) "!=%")))
  (ICall "^dv" [(IBuiltin "filter" [pat dict-node])]))


(define `(il-word n node)
  (IBuiltin "word" [(IString n) node]))


(define `(il-nth n node)
   (ICall "^n" [(IString n) node]))


(define (il-nth-rest n node)
  (IBuiltin "wordlist" [(IString n) (IString 99999999) node]))


;; Namespacing
;; -----------
;;
;; Namespaces help avoid conflicts between compiler sources and "user"
;; code, which must coexist in the same Make instance in the following
;; scenarios:
;;
;;  - In interactive mode, expressions are compiled and then executed.
;;  - An executable may be composed of modules compiled from source and
;;    other modules that had been bundled with the compiler.
;;  - Executable macros are loaded and executed during compilation.
;;
;; A "native name" for a SCAM variable is the GNU Make variable name used to
;; hold its value.  When building the compiler, we prefix the SCAM name with
;; "~".  The compile flag "--boot" indicates that we are building the
;; compiler, and *is-boot* will be true.


;; Generate a unique symbol name derived from BASE.
;;
(define (gensym-name base env suff)
  &public
  (define `name (.. base "&" suff))
  (if (filter (.. name "!=%") env)
      (gensym-name base (.. env " .") (words env))
      name))


;; Return current nesting depth as indicated in the current environment.
;;
;; Result = (.. LDEPTH ADEPTH) as described for EDefn records.
;;
(define (current-depth env)
  &public
  (case (dict-get DepthMarkerKey env)
    ((EMarker depth) depth)
    (_ ".")))


;; Return ADEPTH given DEPTH
;;
(define `(depth.a depth)
  &public
  (subst "." nil depth))


;; Return LDEPTH given DEPTH
;;
(define `(depth.l depth)
  &public
  (subst ";" nil depth))


;; Generate a unique symbol derived from symbol BASE.  Returns new symbol.
;; The symbol is guaranteed not to conflict with any symbol in ENV, or with
;; any other symbol generated with the same ENV and a different BASE.
;;
(define (gensym base env)
  &public
  (PSymbol 0 (gensym-name (symbol-name base) env nil)))


;; Construct an error node.  The resulting error node inherits the document
;; index from FORM.
;;
(define (gen-error form fmt ...values)
  &public
  (PError (or (form-index form)
              (if (numeric? form)
                  form
                  0))
          (vsprintf fmt values)))


(define (form-description code)
  (cond
   ((eq? code "%") "form")
   ((eq? code "L") "list")
   ((eq? code "S") "symbol")
   ((eq? code "P") "target")
   ((eq? code "Q") "literal string")
   (else (form-typename code))))


(define (err-expected types form parent what where ?arg1 ?arg2)
  &public
  (gen-error (or form parent)
             (.. (if form "invalid" "missing") " " what " in " where
                 (if types (.. "; expected a "
                               (concat-for (ty types " or ")
                                 (form-description ty)))))
             arg1 arg2))


;; ARITY = the arity of the function or macro being called (as in EMacro and
;;    EFunc entries).
;; ARGS = array of arguments
;; SYM = symbol for function/form that is being invoked
;;
(define (check-arity arity args sym)
  &public
  (define `ok
    (or (filter (.. "0+ " (words args)) arity)
        (and (findstring "+" arity)
             (word (patsubst "%+" "%" arity) args))))

  (define `expected
    (subst "+" " or more" (subst " " " or " arity)))

  (if (not ok)
      (gen-error sym
                 (subst "%S" (if (eq? expected 1) "" "s")
                        "`%s` accepts %s argument%S, not %s")
                 (symbol-name sym) expected (words args))))


;; Binding Targets
;; -------------------------------------
;;
;; Targets generally may appear whereever a symbol would appear in an
;; assignment or a parameter list.  Each target contains one or more
;; symbols.  Function parameter additionally may end with optional symbols
;; and then a single "rest" symbol.
;;
;;   Params       = Target*  OptSymbol*  RestSymbol?
;;   Patterm      = Symbol | VecTarget | PairTarget | FieldTarget
;;   VecTarget    = `[` Target*  RestSymbol? `]`
;;   PairTarget   = `{` `=` Target `:` Target [, `...` `:` Target] `}`
;;   FieldTarget  = `{` (Symbol `:` Target) ,... `}`
;;   OptSymbol    = `?` .. Name
;;   RestSymbol   = `...` .. Name
;;
;; Note that ParamList is used in `lambda` and function-style `define`,
;; while only Target is used in `let/let&/let-global` and var-style `define`.


;; We can optimize (filter PAT (symbol-name SYM)) down to (filter PAT FORM)
;; when PAT will not match the record tag or form index, given knowledge of
;; PSymbol structure internals.  Use of this macro indicates that intent.
;;
(define `(sym-filter pat symbol)
  (filter pat symbol))


(data NX
  &public

  ;; A binding of a variable name to some portion of a value.
  ;;
  ;; NAME = the variable name bound
  ;; XTOR = an *extractor* function.  An extractor accepts an IL node for the
  ;;     value being bound to the entire target, and returns an IL node for the
  ;;     value being bound to the symbol.
  ;;
  (Bind name xtor))

(declare (pp form xtor))


;; Parse a binding *target*, which is a form that describes how zero or more
;; symbols are to be bound to a value.
;;
;; Input: FORM
;; Returns: [ RESULT... ], where:
;;     RESULT = (Bind NAME XTOR) | (PError ...)
;;
(define (parse-target tgt)
  &public
  ;; special-case common path: valid symbol
  (or (case tgt
        ((PSymbol _ name)
         (if (sym-filter "?% ...%" tgt)
             nil
             [(Bind name identity)])))
      (pp tgt identity)))


(define perror-pattern
  (.. [(word 1 (PError nil nil))] "!0%"))


;; Return the first error from a `parse-target` result, if there were any.
;;
(define (first-perror result)
  &public
  (first (filter perror-pattern result)))


(define `pt-NOOPT 1)
(define `pt-BADREST 2)
(define `pt-BADDICT 3)
(define `pt-BADKEY 4)
(define `pt-BADTARGET 5)


;; MODE = nil (parameter), 1 (vector), 2 (dict value)
;;
(define (pt-err form code)
  (define `index
    (subst 1 (if (filter "...%" (symbol-name form)) 2 1) code))

  (define `descs [
     "'?NAME' can appear only in parameter lists after all non-optional parameters"
     "'...NAME' can appear only as the last parameter or vector target element"
     "invalid assignment target; {=NAME: TARGET, ...} unpacks pairs, {KEY: TARGET, ...} extracts fields"
     "in dictionary field targets, keys must be symbols or string literals"
     "invalid assignment target; expected symbol, vector, or dictionary"])

  [(gen-error form (nth index descs))])


(define `(dict-first-key pairs)
  ;; dict-key actually returns the key of the first pair
  (dict-key pairs))


;; {=sym: pat}
;; {`...`: pat}
;;
;; XTOR = extractor of the dictionary.
;;
(define (pt-pair key value n is-last xtor)
  (define `(pair-xtor il)
    ;; ^dk and ^dv work on the first pair in a dictionary
    (if (filter n 1)
        (xtor il)
        (il-word n (xtor il))))

  ;; An extractor of the key.
  (define `key-xtor
    (lambda (il) (il-dict-key (pair-xtor il))))

  ;; An extractor of the value.
  (define `value-xtor
    (lambda (il) (il-dict-value (pair-xtor il))))

  (define `rest-xtor
    (lambda (il) (il-nth-rest n (xtor il))))

  (or (case key
        ((PSymbol _ name)
         (cond
          ((sym-filter "=%" key)
           (cons (Bind (patsubst "=%" "%" name) key-xtor)
                 (pp value value-xtor)))

          ((and (sym-filter "..." key) is-last)
           (pp value rest-xtor)))))

      (pt-err key pt-BADDICT)))


(define `(pt-pairs pairs xtor)
  (append-for (n (indices pairs))
    (define `p (word n pairs))
    (define `is-last (filter n (words pairs)))

    (pt-pair (dict-key p) (dict-value p) n is-last xtor)))


;; { const: pat ,* }
;;
(define `(pt-fields pairs xtor)
  (foreach (pair pairs)
    (define `key-vec
      (case (dict-key pair)
        ((PSymbol _ name) [name])
        ((PString _ str) [str])))

    (or (append-for (key key-vec)
          (pp (dict-value pair)
              (lambda (il) (il-dict-get key (xtor il)))))
        ;; non-const key
        (pt-err (dict-key pair) pt-BADKEY))))


;; { ... }
;;
(define (pt-dict pairs xtor)
  (or (case (dict-first-key pairs)
        ((PSymbol _ key-sym)
         (if (filter "... =%" key-sym)
             (pt-pairs pairs xtor))))
      (pt-fields pairs xtor)))


(define (pt-vec elems num-elems xtor)
  ;; Allow empty vectors as targets?  Eh, might as well.
  (append-for (n (indices elems))
    (define `elem
      (nth n elems))

    (or
     ;; handle potential final "rest" element
     (if (filter n num-elems)
         (case elem
           ((PSymbol _ name)
            (if (filter "...%" name)
                [(Bind (or (patsubst "...%" "%" name) name)
                       (lambda (il) (il-nth-rest (words elems) (xtor il))))]))))

     ;; ordinary element
     (pp elem (lambda (il) (il-nth n (xtor il)))))))


(define (pp form xtor)
  (case form
    ((PSymbol _ name)
     (if (sym-filter "...% ?%" form)
         (pt-err form pt-NOOPT)
         [(Bind name xtor)]))

    ((PVec _ vforms)
     (pt-vec vforms (words vforms) xtor))

    ((PDict _ pairs)
     (pt-dict pairs xtor))

    (_ (pt-err form pt-BADTARGET))))


(define (bind-nxmap nxmap scope depth il)
  &public
  (append-for (nx nxmap)
    (case nx
      ((Bind name xtor) { =name: (EIL scope depth (xtor il)) })
      (other {=EnvErrorKey: other}))))


(define (bind-target target scope depth il)
  &public
  (bind-nxmap (parse-target target) scope depth il))


;; Base Environment
;; ----------------


(define base-env
  (define `builtins
    (._. "abspath basename dir error firstword lastword notdir realpath shell"
         "sort suffix wildcard words native-eval native-flavor native-origin"
         "native-strip native-value addprefix/2 addsuffix/2 filter/2"
         "filter-out/2 findstring/2 join/2 word/2 patsubst/3 wordlist/3"
         "and/0+ or/0+ native-call/1+ if/2/3"))

  (._.
   (foreach (w builtins)
     (define `name (word 1 (subst "/" " " w)))
     (define `argc  (or (rest (subst "/" " " w)) 1))
     (define `b-name (subst "native-" nil name))

     {=name: (EBuiltin "i" b-name argc)})

   {native-var: (EBuiltin "i" "=" 1)}))


;; Resolve a symbol to its definition, or return nil if undefined.
;; For non-symbols, return "-" (meaning, essentially, "not applicable").
;;
(define (resolve form env)
  &public
  (define `(find-name name dict)
    ;; equivalent to `(dict-find (symbol-name form) dict)` but quicker
    (filter (.. (subst "!" "!1" name) "!=%") dict))

  (case form
    ((PSymbol n name) (dict-value (or (find-name name env)
                                      (find-name name base-env))))
    (else "-")))


;; Return the native name to use for SCAM variable NAME.
;;
(define (gen-native-name name flags)
  &public
  (cond
   ((filter "%&native" flags) name)
   (*is-boot* (.. "`" name))
   (else (.. "'" name))))
