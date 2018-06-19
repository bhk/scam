;--------------------------------------------------------------
;; gen : Environment and IL types; code generation utilities
;;--------------------------------------------------------------

(require "core")
(require "parse")

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
;; "IL" is an intermediate language, structured as a tree of records.  The
;; first word of each vector describes its type.  IL constructs map closely
;; to GNU Make constructs.
;;
;; Blocks
;;
;;   Blocks are sequences of expressions, as in a `begin` expression or a
;;   function body.  All code within a block is executed, and the return
;;   values are discarded for all but the last sub-node.
;;
;; IWhere
;;
;;    IWhere will be expanded to the source file name, and, if POS is
;;    non-nil, a ":LINE" suffix.  When IWhere occurs within a macro, POS
;;    will be rewritten to reflect where the macro was invoked.
;;
;;  ICrumb
;;
;;    An ICrumb holds a name/value pair that will be passed through to the
;;    `compile-text` output without affecting the behavior of the generated
;;    code.
;;
;; IEnv
;;
;;    Some expressions in a block context create new bindings for the
;;    environment.  In those cases, the phase 0 compilation of those
;;    expressions return an IEnv record containing the code *and* the new
;;    bindings.  These are "unwrapped" by c0-block-cc and replaced with
;;    `node`.  If phase 1 sees an IEnv record, it ignores the env field.
;;
;; Errors
;;
;;   PError records may occur where IL records may appear.  See parse.scm.


(data IL
      &public
      (IString  value)                      ; "value"
      (IVar     &word name)                 ; "$(name)"
      (IBuiltin &word name  &list args)     ; "$(name ARGS...)"
      (ICall    &word name  &list args)     ; "$(call name,ARGS...)"
      (ILocal   &word ndx   &word level)    ; "$(ndx)"
      (IFuncall &list nodes)                ; "$(call ^Y,NODES...)"
      (IConcat  &list nodes)                ; "VALUES..."
      (IBlock   &list nodes)                ; "$(if NODES,,)"
      (ILambda  node)                       ; (lambda-quote (c1 node))
      (IWhere   &word pos)                  ; "value"
      (ICrumb   &word key value)            ; <crumb>
      (IEnv     env &list node))            ; used during phase 0


;; Environment Records
;; -------------------
;;
;; The environment is a stack of bindings: a dictionary with newer
;; (lexically closer) bindings toward the beginning.  When used as a
;; dictionary, it maps a symbol name to its in-scope *definition*.  Each
;; definition is a vector in one of the following formats:
;;
;; SCOPE describes the scope and origin of top-level bindings.
;;
;;      When `require` imports symbols, only &public symbols will be
;;      imported.  This should not involve EBuiltin, EIL, EArg, and EMarker
;;      because they should not be in any exported environment.
;;
;;        "i" => defn was imported
;;        "p" => defn is private
;;        "x" => public (exported)
;;        "-" => private or don't care
;;
;; NAME = the actual (global) name of the function/variable or builtin.
;;         For EFunc records, the value NoGlobalName indicates that the
;;         binding is a compound macro, and not a function variable.
;;
;; ARGC = number of arguments the function/macro accepts, in a form
;;        ready for check-argc, such as: "0", "1 or 2", "1 or more".
;;
;; MACRO = For functions, this is nil.  For macros, it is (cons DEPTH NODE)
;;         where DEPTH is the lambda nesting depth where the macro was
;;         defined, and NODE is an IL node representing the macro body.
;;
;; ARGREF = description of local variable or capture.  Note that these
;;   references are absolute, an ILocal records use relative addressing.
;;      .1   --> argument #1 of a top-level function
;;      ..1  --> argument #1 of a function within a function
;;      ...1 --> argument #1 of a third-level nested function
;;
;; DEPTH = nesting depth at which the IL portion of an EIL entry was
;;     compiled.  When the EIL entry is instantiated, DEPTH tells how to
;;     translate ILocal records that identify captures.
;;
;; EMarker values embed contextual data other than variable bindings.  They
;; use keys that begin with ":" to distinguish them from variable names.

(data EDefn
      &public
      ;; data variable
      (EVar     name &word scope)
      ;; function variable or compound macro
      (EFunc    name &word scope argc &list macro)
      ;; pre-compiled IL
      (EIL      depth &word scope &list il)
      ;; executable macro
      (EXMacro  name &word scope)
      ;; data record type
      (ERecord  encs &word scope tag)
      ;; builtin function
      (EBuiltin name &word scope argc)
      ;; function argument
      (EArg     &word argref)
      ;; marker
      (EMarker  &word data))

(define `(EDefn.scope defn)
  &public
  (word 3 defn))

(define `NoGlobalName
    &public
    ":")


;; The current function nesting level: ".", "..", "...", and so on.
(define `LambdaMarkerKey &public ":")

;; Indication of error in loading environment.
(define `ErrorMarkerKey &public ":E")


(define `NoOp
  &public
  (IString ""))


;; Merge consecutive (IString ...) nodes into one node.  Retain all other
;; nodes.
;;
(define (il-merge-strings nodes accum)
  (case (first nodes)
    ((IString value)
     (il-merge-strings (rest nodes) (concat accum value)))
    (else
     (append (if accum
                 [(IString accum)])
             (word 1 nodes)
             (if (word 2 nodes)
                 (il-merge-strings (rest nodes) ""))))))


(define (il-flatten nodes)
  (append-for node nodes
              (case node
                ((IConcat children)
                 (il-flatten children))
                (else [node]))))


;; IConcatenate nodes in IL domain.
;;
(define (il-concat nodes)
  &public
  (let ((nodes-out (il-merge-strings (il-flatten nodes) "")))
    (if (word 2 nodes-out)
        (IConcat nodes-out)
        (or (first nodes-out)
            NoOp))))


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


;; NODE is IL; A and B are actual strings.
(define (il-subst a b node)
  &public
  (case node
    ((IString value) (IString (subst a b value)))
    (else (IBuiltin "subst" [ (IString a) (IString b) node ]))))



;; Namespacing
;; -----------
;;
;; Namespaces help avoid conflicts between compiler sources and "user"
;; code, which must coexist in the same Make instance in the following
;; scenarios:
;;
;;  - In interactive mode, expressions are compiled and then executed.
;;
;;  - An executable may be composed of modules compiled from source and
;;    other modules that had been bundled with the compiler.
;;
;;  - "use" statements specify modules to be executed by the compiler,
;;    perhaps after being compiled by the same compiler.
;;
;; See reference.md for more on namespaces.  The compile flag "--boot"
;; indicates that we are building the compiler, and *is-boot* will be true.
;;
;; (gen-global-name SCAM-NAME FLAGS)
;;
;;     This function generates a global name for the target code. It is
;;     by the compiler when generating code.
;;
;; (global-name SYMBOL)
;;
;;    This special form retrieves the global name associated with the SYMBOL
;;    in the current environment.  This yields a string suitable for passing
;;    to `call`, `value`, `origin`, etc..
;;
;; If FLAGS contains a word ending in "&global", the symbol name is returned
;; unmodified.
;;

(define (gen-global-name local flags)
  &public
  (concat (and *is-boot*
               (not (filter "%&global" flags))
               "~")
          local))


;; Generate a unique symbol name derived from `base`.  Returns a symbol
;; name.
;;
(define (gensym-name base env suff)
  &public
  (define `name (concat base "&" suff))
  (if (filter (concat name "!=%") env)
      (gensym-name base (concat env " .") (words env))
      name))


;; Return current lambda nesting depth.
;;
(define (current-depth env)
  &public
  (let ((defn (dict-get LambdaMarkerKey env)))
    (case defn
      ((EMarker level) level))))


;; Generate a unique symbol derived from symbol `base`.  Returns new symbol.
;; The symbol is guaranteed not to conflict with any symbol in `env`, or
;; with any other symbol generated with the same `env` and a different `base`.
;;
(define (gensym base env)
  &public
  (PSymbol 0 (gensym-name (symbol-name base) env nil)))


;; Construct an error node.  The resulting error node inherits the document
;; index from `form`.
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
   ((eq? code "Q") "literal string")
   (else (form-typename code))))


(define (err-expected types form parent what where ?arg1 ?arg2)
  &public
  (gen-error (or form parent)
             (concat
              (if form "invalid" "missing") " " what " in " where
              (if types (concat "; expected a "
                                (concat-for ty types " or "
                                            (form-description ty)))))
             arg1 arg2))


;; EXPECTED = filter pattern to match number of arguments, and also
;;      to be used to construct the error message.  E.g. "2 or 3".
;;      "N or more" can be used to verify that at least N arguments
;;      are present.
;; ARGS = array of arguments
;; SYM = symbol for function/form that is being invoked
;;
(define (check-argc expected args sym)
  &public
  (define `ok
    (or (filter expected (words args))
        (and (filter "more" expected)
             (or (eq? 0 (word 1 expected))
                 (word (word 1 expected) args)))))

  (if (not ok)
      (gen-error sym
                 (subst "%S" (if (eq? expected 1) "" "s")
                        "%q accepts %s argument%S, not %s")
                 (symbol-name sym) expected (words args))))


(define builtins-1
  (concat "abspath basename dir error eval firstword flavor"
          " info lastword notdir origin realpath shell sort"
          " strip suffix value warning wildcard words"))

(define builtins-2
  "addprefix addsuffix filter filter-out findstring join word")

(define builtins-3
  ".foreach patsubst .subst wordlist")

(define builtin-names
  &public
  (patsubst ".%" "%" (concat builtins-1 " "
                             builtins-2 " "
                             builtins-3 " "
                             "and or call if")))

(define base-env
  (append
   (foreach b builtins-1
            { =b: (EBuiltin b "i" 1) })
   (foreach b builtins-2
            { =b: (EBuiltin b "i" 2) })
   (foreach b builtins-3
            { =b: (EBuiltin (patsubst ".%" "%" b) "i" 3)})
   (foreach b "and or call"
            { =b: (EBuiltin b "i" "%") })
   {if: (EBuiltin "if" "i" "2 or 3")}

   ;; Make special variables & SCAM-defined variables
   ;; See http://www.gnu.org/software/make/manual/make.html#Special-Variables
   (foreach v ["MAKEFILE_LIST" ".DEFAULT_GOAL"]
            { =v: (EVar v "i") })))


;; Resolve a symbol to its definition, or return nil if undefined.
;; For non-symbols, return "-" (meaning, essentially, "not applicable").
;;
(define (resolve form env)
  &public
  (define `(find-name name dict)
    ;; equivalent to `(dict-find (symbol-name form) dict)` but quicker
    (filter (concat (subst "!" "!1" name) "!=%") dict))

  (case form
    ((PSymbol n name) (dict-value (or (find-name name env)
                                      (find-name name base-env))))
    (else "-")))
