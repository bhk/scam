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
      (IConcat  &list nodes)                ; "VALUES..."
      (IBlock   &list nodes)                ; "$(if NODES,,)"
      (ILambda  node)                       ; (lambda-quote (c1 node))
      (IWhere   value)                      ; "value"
      (IEnv     env &list node))            ; used during phase 0

;; Blocks
;;
;;   Blocks are sequences of expressions, as in a `begin` expression or a
;;   function body.  All code within a block is executed, and the return
;;   values are discarded for all but the last sub-node.
;;
;; IWhere
;;
;;    IWhere represents a constant string that indicated a source file
;;    position formatted as "FILE:LINE".  When IWhere occurs within a macro,
;;    its contents will be rewritten when the macro is expanded to reflect
;;    where the macro was invoked.
;;
;; IEnv
;;
;;    Some expressions in a block context modify the environment for
;;    subsequent expressions.  In that case, the phase 0 compilation of
;;    those expressions return an IEnv record, which is then unwrapped by
;;    c0-block-cc.  IEnv records should never be presented to phase 1.
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
      ;; data variable
      (EVar     name &word scope)
      ;; function variable or compound macro
      (EFunc    name &word scope argc &list inln)
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
;; INLN = definition of a function or compound macro body:
;;           [ [ARGNAME...] BODY...]
;;        where each ARGNAME is a string and BODY... is a vector of
;;        forms.  For inline functions and compound macros, BODY is
;;        non-empty.  For non-inline functions, BODY is empty.
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

;; The current function nesting level: ".", "..", "...", and so on.
(define `LambdaMarkerKey
  &public
  ":")


;; Exports and Imports
;; -------------------
;;
;; Each generated .min file includes a comment line that describes the
;; module's "final" environment state -- the lexical environment as it was
;; at the end of processing the source file.  The comment has the following
;; format:
;;
;;     "# Exports: " (env-compress <vector>)
;;
;; Both public *and* private symbols are exported.  Imported symbols are not
;; re-exported.
;;
;; Exports are consumed when `(require MOD)` is compiled.  At that time, the
;; bindings *exported* from MOD (public in its final env) are added to the
;; current environment, and marked as imported (SCOPE = "i").
;;
;; When `(require MOD &private)` is compiled, both public and private
;; symbols from MOD are added to the current environment.
;;
;; Lambda markers and local variables are not exported -- it is actually
;; impossible for them to exist in the final environment because the end of
;; the file is necessarily outside of any lambda context.
;;
;; Data Records
;; ------------
;;
;; A TAG is a string stored in the first word of a constructed record,
;; differentiating it from other records, and from vectors.  Each tag begins
;; with "!:", which cannot appear in any of SCAM's standard subordinate data
;; types (vectors, hashes, numbers).  Record types are therefore disjoint
;; from each other and from subordinate types.  (Although vectors and numbers
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

;; *compile-file* is the name of the source file being compiled.
(declare *compile-file* &public)

;; *compile-outfile* is the name of the MIN file being generated.  This is
;; used to find modules when `require` expressions are encountered.
(declare *compile-outfile* &public)

;; *compile-mods* lists object files to be used to satisfy `require`
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
;; Namespaces help avoid conflicts between compiler sources and "target"
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
;; See reference.md for more on namespaces.
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


;; Return current lambda nesting depth.
;;
(define (current-depth env)
  &public
  (let ((defn (hash-get LambdaMarkerKey env)))
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


;; env-cmp and env-exp were generated by envcomp.scm.

(define (env-cmp s)
  (subst ";" "!A" "\\" "!B" "," "!C" "`" "!D" "'" "!E" "<" "!F" ">" "!G"
         "[" "!H" "]" "!I" "|" "!J" "@" "!K" "{" "!L" "}" "!M" "#" "!N"
         "\"" "!O" "&" "!P" "(" "!Q" ")" "!R" "+" "!S" "_" "!T" "!0" ";" "!1"
         "\\" "\\1" "," ";," "`" ":IL0" "'" ":IL2" "<" ":IL3" ">" ":IL4"
         "[" "\\0" "]" ",0" "|" ",11" "@" "111" "{" ",10" "}" "!=\\:EDefn"
         "#" "#1;~%;" "\"" "#1;:;" "&" " ml.special-" "(" "\"p;" ")" ")1 "
         "+" "\"x;" "_" s))

(define (env-exp s)
  (subst "_" "\"x;" "+" ")1 " ")" "\"p;" "(" " ml.special-" "&" "#1;:;" "\""
         "#1;~%;" "#" "!=\\:EDefn" "}" ",10" "{" "111" "@" ",11" "|" ",0" "]"
         "\\0" "[" ":IL4" ">" ":IL3" "<" ":IL2" "'" ":IL0" "`" ";," ","
         "\\1" "\\" "!1" ";" "!0" "!T" "_" "!S" "+" "!R" ")" "!Q" "(" "!P" "&"
         "!O" "\"" "!N" "#" "!M" "}" "!L" "{" "!K" "@" "!J" "|" "!I" "]"
         "!H" "[" "!G" ">" "!F" "<" "!E" "'" "!D" "`" "!C" "," "!B" "\\"
         "!A" ";" s))


;; Tokenize the key within the binding (it usually occurs once).
;;
(define (tokenize-key v)
  (foreach w v
           (concat
            (word 1 (subst "!=" "!= " w))
            (subst "%" "!p" (word 1 (subst "!=" " " w)) "%"
                   (word 2 (subst "!=" "!= " w))))))

(define (detokenize-key v)
  (foreach w v
           (concat
            (word 1 (subst "!=" "!= " w))
            (subst "%" (word 1 (subst "!=" " " w)) "!p" "%"
                   (word 2 (subst "!=" "!= " w))))))


;; Prepare environment V for inclusion in a line of text in the MIN file.
;;
(define (env-compress v)
  ;; Strip redundant spaces from record values; not reversible but
  ;; that's okay.
  (define `(strip-space v)
    (patsubst "%!0" "%" v))

  (env-cmp
   (tokenize-key
    (strip-space
     (subst "\n" "!n" v)))))


;; Recover an environment value produced by env-compress.
;;
(define (env-expand str)
   (subst "!n" "\n"
          (detokenize-key
           (env-exp str))))


(define (import-binding key defn)
  (if (EDefn.is-public? defn)
      (hash-bind key (EDefn.set-scope defn "i"))))


(declare (get-module-env mod all))


;; Import bindings from another module. Return an environment.
;;
;; ENV = env containing exported bindings
;; ALL = whether to return all bindings.
;;    If true, return the final environment of the module.
;;    If false, return only public bindings from the final env.
;;
(define (env-import env all)
  (if all
      ;; Add all symbols, public and private.
      env
      ;; Add only public symbols.
      (strip-vec
       (foreach b env
                (import-binding (hash-key b) (hash-value b))))))


;; Discard imported bindings.  Leave other public and private bindings.
;;
;; ENV = the final environment of the module
;;
(define `(env-export env)
  (strip-vec
   (foreach b env
            (if (not (filter "i" (EDefn.scope (hash-value b))))
                b))))


;; Generate "exports" comment line for MIN file
;;
(define (env-export-line env)
  &public
  (concat "# Exports: " (env-compress (env-export env)) "\n"))


;; Return all bindings exported from a MIN file.  The keys of non-public
;; entries are prefixed with "(".
;;
(define `(env-parse lines)
  (subst "!n" "\n"
         (env-expand (first (filtersub ["# Exports: %"] "%" lines)))))


;; Read a module from a file or a bundled variable and return its final
;; environment ("exports").
;;
(define `(env-load filename)
  (define `(read-module-lines filename)
    (if (filter "///%" filename)
        (split "\n" (value filename))
        (read-lines filename 1 4)))

  (env-parse (read-module-lines filename)))


(define *dummy-env*
  (hash-bind "" (EIL "" "-" NoOp)))


;; Import symbols from FILENAME.  ALL means return all original environment
;; entries, not just public ones.  Return `nil` on error (bad filename).
;;
(define (get-file-env filename all)
  (if filename
      (or (env-import (env-load filename) all)
          *dummy-env*)))


(memoize (global-name get-file-env))


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
;; ALL : True => return all environment entries that were visible at
;;               the end of MOD.
;;       False => return &public symbols defined/declared in MOD.
;;
;; Return: ENV on success
;;         nil on failure  (module not found)
;;
(define (get-module-env mod all)
  &public
  (get-file-env (mod-find mod) all))


;; Perform a compile-time import of "mod", returning env entries to be added
;; to the using module.
;;
;; Returns:  ENV  on success
;;           nil  on failure (module not found)
;;
(define (use-module mod)
  &public
  (let ((imports (get-module-env mod nil)))
    (if imports
        (let-global ((SCAM_MODS *compile-mods*))
          (call "^require" mod)
          (or (strip-vec (foreach e imports
                                  (case (hash-value e)
                                    ((EXMacro _ _) e))))
              *dummy-env*)))))


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
