;;--------------------------------------------------------------
;; gen : Environment, IL, and related utilities.
;;--------------------------------------------------------------

(require "core")
(require "io")
(require "parse")
(require "escape")

;; IL Records
;; ----------
;;
;; "IL" is an intermediate language, structured as a tree of vectors.  The
;; first word of each vector describes its type.  IL constructs map closely
;; to GNU Make constructs.

(data IL
      &public
      (IString  value)                      ; "value"
      (IVar     name)                       ; "$(name)"
      (IBuiltin &word name  &list args)     ; "$(name ARGS...)"
      (ICall    name        &list args)     ; "$(call name,ARGS...)"
      (ILocal   &word ndx   &word level)    ; "$(ndx)"
      (IFuncall &list nodes)                ; "$(call ^Y,NODES...)"
      (IConcat  &list values)               ; "VALUES..."
      (IBlock   &list nodes)                ; "$(if NODES,,)"
      (ILambda  node)                       ; (lamda-quote (c1 node))
      (IEnv     env &list node))            ; used during phase 0

;; Blocks
;;
;;   Blocks are sequences of expressions, as in a `begin` expression or a
;;   function body.  All code within a block is executed, and the return
;;   values are discarded for all but the last sub-node.
;;
;;   Expressions that are children of a block are called "block-level"
;;   expressions.  Block-level expressions are special in that they can
;;   modify the environment for expressions that follow them in the block.
;;
;; ILEnv
;;
;;    When inside of a "block" environment -- e.g. (begin ...) -- some
;;    expressions generate an environment along with code.  In that context,
;;    they will return an ILEnv record, which is then unwrapped by c0-block.
;;    ILEnv should never be presented to phase 1.
;;
;; Errors
;;
;;   PError records may occur where IL records may appear.  See parse.scm.
;;
;; Environment Records
;; -------------------
;;
;; The environment is a stack of bindings: a vector of hash entries with
;; newer (lexically closer) bindings toward the beginning.  When used as a
;; hash, it maps a symbol name to its in-scope *definition*.  Each
;; definition is a vector in one of the following formats:
;;

(data EDefn
      &public
      (EVar     name &word scope)      ; data variable
      (EFunc    name &word scope inln) ; function var or compound macro
      (ESMacro  form &word scope)      ; symbol macro
      (EXMacro  name &word scope)      ; executable macro
      (ERecord  encs &word scope tag)  ; data record type
      (EBuiltin name &word scope argc) ; builtin function
      (EIL      node)                 ; pre-compiled IL node
      (EArg     &word argref)         ; function argument
      (EMarker  &word data))          ; marker

(define `(EDefn.scope defn)
  &public
  (word 3 defn))

(define `(EDefn.set-scope defn scope)
  (append (wordlist 1 2 defn)
          scope
          (nth-rest 4 defn)))

(define `(EDefn.is-public? defn)
  &public
  (filter "x" (EDefn.scope defn)))

(define `NoGlobalName
    &public
    ":")

;; SCOPE describes the scope and origin of top-level bindings.
;;
;;      When `require` imports symbols, only &public symbols will be
;;      imported.  This should not involve EBuiltin, EIL, EArg, and EMarker
;;      because they should not be in any exported environment.
;;
;;      When expanding ESMacro.form or EFunc.inln, we "rewind" the
;;      environment to its state at the time of the definition.  If the
;;      definition was in another file, this means re-creating the
;;      environment of that other file.
;;
;;        "iMOD" => defn was imported from module MOD.
;;        "i" => defn was imported (for types that do not rewind)
;;        "p" => defn is private
;;        "x" => public (exported)
;;        "." => private or don't care
;;
;; NAME = the actual (global) name of the function/variable or builtin.
;;         For EFunc records, the value NoGlobalName indicates that the
;;         binding is a compound macro, and not a function variable.
;;
;; INLN = definition of a function or compound macro body:
;;          [ [ARGNAME...] BODY...]
;;        where each ARGNAME is a string and BODY... is a vector of forms.
;;        For inline functions and compoune macros, BODY is non-empty.
;;        For non-inline functions, BODY is empty.
;;
;; ARGREF = argument reference:
;;      .1   --> argument #1 of a top-level function
;;      ..1  --> argument #1 of a function within a function
;;      ...1 --> argument #1 of a  third-level nested function
;;
;; FORM = an AST node that describes the symbol macro definition.  This
;;     will be expanded in the scope of the bindings in effect where the
;;     macro was defined.
;;
;; EMarker values embed contextual data other than variable bindings.  They
;; use keys that begin with ":" to distinguish them from variable names.

;; The current function nesting level: ".", "..", "...", and so on.
(define `LambdaMarkerKey
  &public
  ":")

;; The source position of the macro invocation.
(define `MacroMarkerKey
  &public
  ":m")

;; Exports and Imports
;; -------------------
;;
;; Each generated .min file incldues a comment line that describes its
;; "final" environment state -- the lexical environment as it was at the end
;; of processing the source file.  This includes public as well as private
;; members.
;;
;; Exports are consumed  when `(require MOD)` is compiled.  At that time,
;; the *public* bindings exported from MOD are imported into the current
;; environment, and marked as imported: their SCOPE fields are set to "i",
;; or (for imported inline functions and macros) "iFILE", where FILE is
;; the MIN file from which they were imported.
;;
;; Exports are also used when a macro or inline function is expanded.  In
;; this case, all exports (public, private, imported) will be used, since
;; the goal is to reconstruct the environment as it was when the
;; macro/function was defined.
;;
;; Exports are written into MIN files in the following format:
;;
;;     "# Exports: " (env-compress <vector>)
;;
;; The vector has one item per binding, consisting of the key cons'ed onto
;; the value.  Non-public entries are prefixed with "(".
;;
;; Lambda markers are not written (it is actually impossible for them to
;; exist in the final environment; they only appear within nested
;; constructs).
;;
;; Data Records
;; ------------
;;
;; A TAG is a string stored in the first word of a constructed record,
;; differentiating it from other records, and from vectors.  Each tag begins
;; with "!:", which cannot appear in any of SCAM's standard subordinate data
;; types (vectors, hashes, numbers).  Record types are therefore disjoint
;; from each other and from subordinate types.  (Althouh vectors and numbers
;; are not disjoint from each other... 1 == [1] == [[1]].)
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
;; Global variable `^tags` holds a hash that maps TAGs to PATTERNs for all
;; constructors whose definitions have been executed.
;;
;;--------------------------------------------------------------


;; *compile-subject* contains the penc-encoded SCAM source being compiled
(declare *compile-subject* &public)

;; *compile-file* is the name of the source file neing compiled.
(declare *compile-file* &public)

;; *compile-outfile* is the name of the MIN file being generated.  This is
;; used to find modules when `require` expressions are encountered.
(declare *compile-outfile* &public)

;; *compile-mods* lists object files to be used to satisy `require`
;; statements encountered when compiling.  (Otherwise, bundled modules will
;; be used.)
(declare *compile-mods* &public)


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


;;--------------------------------------------------------------
;; Environment functions
;;--------------------------------------------------------------

;; Namespacing
;; -----------
;;
;; Namespaces help avoid conflicts bewteen compiler sources and "target"
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
;; See reference.md for more on namepaces.
;;
;; The global variable SCAM_NS provides the namespace prefix during
;; compilation.
;;
;; (gen-global-name SCAM-NAME FLAGS)
;;
;;     This function generates a global name using the run-time value of
;;     SCAM_NS.  It is employed by the compiler itself.
;;
;; (global-name SYMBOL)
;;
;;    This special form retrieves the global name associated with the SYMBOL
;;    in the current environment.  This can be used to convert a target
;;    function/variable name to a string suitable for passing to `call`,
;;    `value`, `origin`, etc..  Note: this reflects the value of SCAM_NS
;;    when the module defining SYMBOL was compiled, which may or may not
;;    match the NS used by the module invoking global-name -- for example,
;;    the symbol might be from target code, or from a bundled module.
;;
;; If FLAGS contains a word ending in "&global", the symbol name is returned
;; unmodified.
;;

(define (gen-global-name local flags)
  &public
  (declare SCAM_NS &global)
  (if (filter "%&global" flags)
      local
      (concat SCAM_NS local)))


;; Generate a unique symbol name derived from `base`.  Returns a symbol
;; name.
;;
(define (gensym-name base env suff)
  &public
  (define `name (concat base "&" suff))
  (if (filter (concat name "!=%") env)
      (gensym-name base (concat env " .") (words env))
      name))


;; Generate a unique symbol derived from symbol `base`.  Returns new symbol.
;; The symbol is guaranteed not to conflict with any symbol in `env`, or
;; with any other symbol generated with the same `env` and a different `base`.
;;
(define (gensym base env)
  &public
  (PSymbol 0 (gensym-name (symbol-name base) env nil)))


;; Return all words after first occurrence of `item` in `vec`.
;; Note: vec must not contain invalid (non-demoted) character sequences.
;;
(define (after item vec)
  (strip-vec
   (subst "!S" " "
          (rest (subst (concat "!S" item "!S") (concat "!S" item "!S ")
                       (concat "!S" (subst " " "!S" vec) "!S"))))))


;; Returns the bindings in effect when symbol-macro `name` was bound, plus
;; any lambda markers that are currently in effect.
;;
(define (env-rewind-M env name)
  (append (hash-find LambdaMarkerKey env)
          (after (hash-find name env) env)))


;; Construct an error node.  The resulting error node inherits the document
;; index from `form`.
;;
(define (gen-error form fmt ...values)
  &public
  (PError (form-index form) (vsprintf fmt values)))


;; Display a warning during compilation.
;;
(define (compile-warn form fmt ?a ?b ?c)
  &public
  (info (describe-error (gen-error form fmt a b c)
                        (pdec *compile-subject*)
                        *compile-file*)))

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


;; Check that FORM matches a form type in TYPES.  If not, return
;; an IL error node describing the error.
;;
;; TYPES = a list of acceptable form types, or "" if any non-empty form will do
;; WHERE = synopsis of syntax being parsed
;; WHAT = element within `where` being checked
;;
;;(define (check-type types form parent what where)
;;  (if (not (filter (or (patsubst "%" "%%" types) "%") (word 1 form)))
;;      (err-expected types form parent what where nil)))

;;(define `(check-form ctor form parent what where)
;;  (check-type (word 1 ctor) form parent what where))


;; EXPECTED = filter pattern to match number of arguments, e.g. "2 or 3"
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


;; env-compress: compress a vector of nested vectors for inclusion in a line
;; of text.  The result should contain no newlines and generally be much
;; smaller.
;;
;; Rarely occurring characters in ENV entries:  , : ; | @ { } " ' ` ( )
;;
(define `(env-compress v)
  (subst
    ;; reduce the effects of nesting vectors
    "!11" "!2" "!21" "!3" "!31" "!4" "!41" "!5"
    ;; rename some infrequently used characters
    "," "!a"  ";" "!b"   ":" "!c"   "|" "!d"  "@" "!e"
    ;; improve readability (and size)
    "!0" ","  "!10" ";"  "!20" ":" "!30" "@"
    ;; handle common substrings in ENV entries
    "!=F," "|" "!=V," "!V"  ",i " "! "
    ;; make it fit on a line of text
    "\n" "!n"
    v))


;; env-expand: undo env-compress
;;
(define `(env-expand v)
  (subst "!n" "\n" "! " ",i " "!V" "!=V," "|" "!=F," "@" "!30" ":" "!20"
         ";" "!10" "," "!0" "!e" "@" "!d" "|" "!c" ":" "!b" ";" "!a" ","
         "!5" "!41" "!4" "!31" "!3" "!21" "!2" "!11"
         v))


(define (import-binding key defn d-name)
  ;; EFunc and ESMacro can rewind
  (define `(can-rewind defn)
    (case defn
      ((EFunc _ _ _) 1)
      ((ESMacro _ _) 1)))

  (if (EDefn.is-public? defn)
      (hash-bind key
                 (EDefn.set-scope defn
                                 (concat "i" (if (can-rewind defn)
                                                 d-name))))))


;; Import the bindings from a MIN file's exports. Return an environment.
;;
;; EXPORTS = parsed exports
;; READ-PRIV = whether or not to include import "&private" members
;;    If true, all members are returned.
;;    If false, only public members are imported, and imported
;;        macros/inline functions will be tagged with `filename`.
;; D-NAME = name of the module containing the exports (demoted)
;;
(define (env-import exports read-priv d-name)
  (if read-priv
      exports
      (strip-vec
       (foreach b exports
                (import-binding (hash-key b) (hash-value b) d-name)))))


;; Generate "exports" comment line for MIN file
;;
(define (env-export env)
  &public
  (concat "# Exports: " (env-compress env) "\n"))


;; Return all bindings exported from a MIN file.  The keys of non-public
;; entries are prefixed with "(".
;;
(define `(env-parse lines)
  (env-expand (first (filtersub ["# Exports: %"] "%" lines))))


;; Read a module from a file or a bundled variable
;;
(define `(env-load filename)
  (define `(read-module-lines filename)
    (if (filter "///%" filename)
        (split "\n" (value filename))
        (read-lines filename 1 4)))

  (env-parse (read-module-lines filename)))


(define *dummy-env*
  (hash-bind "" (EIL (IString ""))))


;; Import symbols from FILENAME.  READ-PRIV means return all original
;; environment entries, not just public ones.  Return `nil` on error (bad
;; filename).
;;
(define (env-from-file filename read-priv)
  ;; This is used to ensure a harmless non-nil result.
  (define `modname (notdir (basename filename)))

  (if filename
      (or (env-import (env-load filename) read-priv [modname])
          *dummy-env*)))


(memoize (global-name env-from-file))


;; Return the location of the object file, given the module name.
;; This is either a MIN file listed in *compile-mods*, or a
;; bundled module ("///mod.min"), or nil if neither are found.
;;
(define (mod-find name)
  (define `bundlevar (concat "///" (notdir name) ".min"))

  (or (firstword (filter (concat "%" (notdir name) ".min") *compile-mods*))
      ;; don't use bundles when the runtime is given as a module file
      (if (and (not (filter "%/runtime.min" *compile-mods*))
               (bound? bundlevar))
          bundlevar
          (print "warning: cannot find module " name))))


;; Read the environment exported from a module.
;;
;; MOD : module name
;; READ_PRIV : if true, read all entries.  Otherwise, discard
;;    imported/private entries and mark other entries as imports.
;;
;; Return: ENV     on success
;;         nil     on failure  (module not found)
;;
(define (require-module mod read-priv)
  &public
  (env-from-file (mod-find mod) read-priv))


;; Perform a compile-time import of "mod", returning env entries to be added
;; to the using module.
;;
;; Returns:  ENV  on success
;;           nil  on failure (module not found)
;;
(define (use-module mod)
  &public
  (let ((imports (require-module mod nil)))
    (if imports
        (let-global ((SCAM_MODS *compile-mods*))
          (call "^require" mod)
          (or (strip-vec (foreach e imports
                                  (case (hash-value e)
                                    ((EXMacro _ _) e))))
              *dummy-env*)))))


;; Find original environment for `name` and return entries that preceded it.
;;
(define (env-rewind-x env name)
  (let ((pair (hash-find name env))
        (env env)
        (name name))
    (define `defn (hash-value pair))
    (define `scope (EDefn.scope defn))
    (define `mod (filtersub "i%" "%" scope))

    (if mod
        (env-rewind-x (require-module (promote mod) 1) name)
        (after pair env))))


;; Construct an environment for evaluation of a macro or inline function.
;; This consists of bindings that were in effect when `name` was defined,
;; plus a lambda marker for the current function nesting level (if any),
;; plus a non-inline binding for a function (when expanding an inline
;; function).
;;
;;  env  = environment
;;  name = name of macro or function
;;  defn = definition (in env) [when rewinding to an EFunc]
;;
(define (env-rewind env name)
  &public
  (append (hash-find LambdaMarkerKey env)
          (env-rewind-x env name)))


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
                             builtins-3)))

(define base-env
  (append
   (foreach b builtins-1 (hash-bind b (EBuiltin b "i" 1)))
   (foreach b builtins-2 (hash-bind b (EBuiltin b "i" 2)))
   (foreach b builtins-3 (hash-bind b (EBuiltin (patsubst ".%" "%" b) "i" 3)))
   (foreach b "and or call" (hash-bind b (EBuiltin b "i" "%")))
   (hash-bind "if" (EBuiltin "if" "i" "2 or 3"))

   ;; Make special variables & SCAM-defined variables
   ;; See http://www.gnu.org/software/make/manual/make.html#Special-Variables
   (foreach v ["MAKEFILE_LIST" ".DEFAULT_GOAL"]
            (hash-bind v (EVar v "i")))))


;; Resolve a symbol to its definition, or return nil if undefined.
;; For non-symbols, return "-" (meaning, essentially, "not applicable").
;;
(define (resolve form env)
  &public
  (define `(find-name name hash)
    ;; equivalent to `(hash-find (symbol-name form) hash)` but quicker
    (filter (concat (subst "!" "!1" name) "!=%") hash))

  (case form
    ((PSymbol n name) (hash-value (or (find-name name env)
                                      (find-name name base-env))))
    (else "-")))


;;
;; AST utilities
;;

;; Make a valid expression from a vector of forms.
;;
(define (begin-block forms)
  &public
  (if (and forms (not (word 2 forms)))
      (first forms)
      (PList 0 (cons (PSymbol 0 "begin") forms))))
