;;--------------------------------------------------------------
;; gen : code generation utilities
;;
;; Compilation is divided into two stages:
;;
;;   c0:  form --> IL        [gen0.scm]
;;   c1:  IL --> object      [gem1.scm]
;;
;; A "form" is an AST node, as described in parse.scm, or an additional
;; node type that is used for code generation purposes:
;;
;;   I <IL>   -->  IL pass-through
;;
;; "Object" is GNU Make syntax, e.g. `$(subst $1,$2,$3)`.
;;
;; "IL" is an intermediate language, structured as a tree of vectors.  The
;; first word of each vector describes its type.  IL constructs map closely
;; to GNU Make constructs.
;;
;;   Q.<n> str             literal            str
;;   V str                 var reference      $(str)
;;   F str <a> <b> ...     call builtin       $(str <a>,<b>,...)
;;   f str <a> <b> ...     call user func     $(call str,<a>,<b>,...)
;;   U <n> <ups>           local var ref      $n  or  $(call ^e,$n,UPS)
;;   Y <f> <a> <b> ...     lambda call        $(call ^Y,<a>,...,<f>)
;;   C <a> <b> <c> ...     concatenation      <a><b><c>
;;   B <a> <b> <c> ...     sequence (block)
;;   X <a>                 nested function    quote (c1 <a>)
;;   E.<n> <description>   error
;;
;; NOTE: "Q" and "E" nodes may have ".LOC" appended to their first word.
;;
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
;; Errors
;;
;;  Errors should always include document positions.  The <decription>
;;  field is a demoted string.  E.g. "E.250 undefined!0symbol"
;;
;;--------------------------------------------------------------
;; Environment
;;
;; The environment is a stack of bindings: a vector of hash entries with
;; newer (lexically closer) bindings toward the beginning.  When used as a
;; hash, it maps a symbol name to its in-scope *definition*.  Each
;; definition is a vector in one of the following formats:
;;
;;    F <name> <priv> <dfn>  ==  function variable (recursive) or compound macro
;;    V <name> <priv>        ==  data variable (simple)
;;    B <name> <priv>        ==  builtin <name>
;;    M <form> <priv>        ==  symbol macro
;;    A <arg>  <priv>        ==  function argument
;;    I <il>   <priv>        ==  pre-compiled IL node
;;    X <name> <priv>        ==  executable macro
;;
;; The special key "$" is used for "lambda markers" that identify the
;; current level of function nesting.  Values for these bindings are also
;; tagged with "$":
;;
;;    $ <level>       == lambda marker
;;
;; <name> = actual name of global function/variable or builtin
;;          The name "#" indicates that the binding is a symbol macro, and
;;          not an actual function variable.
;;
;; <priv> describes the scope and origin of top-level bindings.  This scope
;;        is used to decide which symbols to export from a file.  The origin
;;        is used to construct environments for the purpose of expanding
;;        imported macros.
;;
;;          nil => ordinary global declarations/definitions (to be exported),
;;                 or a local definition.  Locals are not exported, but also they
;;                 are not present in the environment at the end of the file.
;;          "b" => a default (base) environment member
;;          "p" => declared/defined with "&private"
;;          "i<FILENAME>" => imported from via "require".  <FILENAME> is
;;               the file from which it was imported IF this binding
;;               is a macro or inline function; nil otherwise.
;;
;; <dfn> = definition of an '&inline' function or compound macro body.
;;         (first <dfn>) = vector of argument names
;;         (rest dfn) = a vector of forms
;;
;; <arg> = argument reference:
;;         A $1   --> argument #1 to top-level function
;;         A $$1  --> argument #1 to function within a function
;;         A $$$$1  --> argument #1 to third-level nested function
;;
;; <form> = an AST node that describes the symbol macro definition.  This
;;     will be expanded in the scope of the bindings in effect where the
;;     macro was defined.
;;
;; <level> = string of "$" characters (one per level of nesting)
;;
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
;; environment, and marked as imported: their <priv> fields are set to "i",
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

;;--------------------------------------------------------------

(require "core")
(require "io")
(require "parse")
(require "escape")


;; *compile-subject* contains the penc-encoded SCAM source being compiled
(declare *compile-subject*)

;; *compile-file* is the name of the source file neing compiled.
(declare *compile-file*)

;; *compile-outfile* is the name of the MIN file being generated.  This is
;; used to find modules when `require` expressions are encountered.
(declare *compile-outfile*)

;; *compile-mods* lists object files to be used to satisy `require`
;; statements encountered when compiling.  (Otherwise, bundled modules will
;; be used.)
(declare *compile-mods*)


;;--------------------------------------------------------------
;; Environment functions
;;--------------------------------------------------------------

;; Return the global name that should be assigned, given a local name.
;;
;; We use `SCAM_NS` when compiling the compiler.  This allows code built by
;; two different compilers to co-exist in one Make instance, and it helps
;; avoid conflicts between compiler-internal symbols and user code.
;;
(define (gen-global-name local)
  (declare SCAM_NS &global)
  (concat SCAM_NS local))


(define (filtersub pat repl str)
  &private
  (patsubst pat repl (filter pat str)))


;; Generate a unique symbol name derived from `base`.  Returns a symbol
;; name.
;;
(define (gensym-name base env suff)
  (define `name (concat base "&" suff))
  (if (filter (concat name "!=%") env)
      (gensym-name base (concat env " .") (words env))
      name))

;; Generate a unique symbol derived from symbol `base`.  Returns new symbol.
;; The symbol is guaranteed not to conflict with any symbol in `env`, or
;; with any other symbol generated with the same `env` and a different `base`.
;;
(define (gensym base env)
  (concat "S " (gensym-name (symbol-name base) env)))


;; Return all words after first occurrence of `item` in `vec`.
;; Note: vec must not contain invalid (non-demoted) character sequences.
;;
(define (after item vec)
  (wordlist
   1 999999999  ;; strip leading and trailing spaces
   (subst " " "" "!S" " "
          (rest (subst (concat "!S" item "!S") (concat "!S" item "!S ")
                       (concat "!S" (subst " " "!S" vec) "!S"))))))


;; Returns the bindings in effect when symbol-macro `name` was bound, plus
;; any lambda markers that are currently in effect.
;;
(define (env-rewind-M env name)
  (append (hash-find "$" env)
          (after (hash-find name env) env)))


;; Construct an error node.  The resulting error node inherits the document
;; index from `form`.
;;
(define (gen-error form fmt ...)
  [(concat "E." (form-index form)) (vsprintf (rest *args*))])


;; Display a warning during compilation.
;;
(define (compile-warn form fmt a b c)
  (info (describe-error (gen-error form fmt a b c)
                        (pdec *compile-subject*)
                        *compile-file*)))

;; Check that `form` matches form type `type`.  If not, return
;; an IL error node describing the error.
;;
;; type = a list of acceptable form types, or "" if any non-empty form will do
;; context = synopsis of syntax being parsed
;; desc = element within `context` being checked
;;
(define (check-type type form parent desc context)
  (if (filter (or (patsubst "%" "%%" type) "%") (word 1 form))
      nil
      (gen-error (or form parent)
                 (concat
                  (if form "invalid" "missing") " " desc " in " context
                  (if type (concat "; expected a "
                                   (concat-vec (map-call (global-name form-typename) type)
                                               " or ")))))))


;; cnt = number, or string containing more than one number, e.g. "2 or 3"

(define (check-argc cnt form)
  (define `args (rrest form))
  (if (filter-out cnt (words args))
      (gen-error (nth 2 form)
                "%q accepts %s arguments, not %s"
                (symbol-name (nth 2 form))
                cnt
                (words args))))


;; env-compress: compress a vector of nested vectors for inclusion in a line
;; of text.  The result should contain no newlines and generally be much
;; smaller.
;;
;; Rarely occurring characters in ENV entries:  , : ; | @ { } " ' ` ( )
;;
(define `(env-compress v)
  &private
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
  &private
  (subst "!n" "\n" "! " ",i " "!V" "!=V," "|" "!=F," "@" "!30" ":" "!20"
         ";" "!10" "," "!0" "!e" "@" "!d" "|" "!c" ":" "!b" ";" "!a" ","
         "!5" "!41" "!4" "!31" "!3" "!21" "!2" "!11"
         v))


(define (import-binding key defn d-name priv)
  ;; Return the demoted form of the nth item, but with nil represented as nil, not "!."
  (define `(dnth n vec)
    (subst "!." "" (word n vec)))

  (if (not (dnth 3 defn))
      (hash-bind key (append (wordlist 1 2 defn)
                             (concat "i" (if (or (dnth 4 defn)
                                                 (type? "M%" defn))
                                             d-name))
                             (nth-rest 4 defn)))))


;; Import the bindings from a MIN file's exports. Return an environment.
;;
;; exports = parsed exports
;; priv = whether or not to include import "&private" members
;;    If true, all members are returned.
;;    If false, only public members are imported, and imported
;;        macros/inline functions will be tagged with `filename`.
;; d-name = name of the module containing the exports (demoted)
;;
(define (env-import exports priv d-name)
  &private
  (if priv
      exports
      (strip-vec
       (foreach b exports
                (import-binding (hash-key b) (hash-value b) d-name)))))


;; Generate "exports" comment line for MIN file
;;
(define (env-export env)
  (concat "# Exports: " (env-compress env) "\n"))


;; Return all bindings exported from a MIN file.  The keys of non-public
;; entries are prefixed with "(".
;;
(define `(env-parse lines)
  &private
  (env-expand (first (filtersub ["# Exports: %"] "%" lines))))


;; Read a module from a file or a bundled variable
;;
(define `(env-load filename)
  &private
  (define `(read-module-lines filename)
    (if (filter "///%" filename)
        (split "\n" (value filename))
        (read-lines filename 1 4)))

  (env-parse (read-module-lines filename)))


(define *dummy-env*
  &private
  (hash-bind "" ["I" "Q" 1]))

;; Import symbols from `filename`.  `priv` means return all original
;; environment entries, not just public ones.  Return `nil` on error (bad
;; filename).
;;
(define (env-from-file filename priv)
  &private
  ;; This is used to ensure a harmless non-nil result.
  (define `modname (notdir (basename filename)))

  (if filename
      (or (env-import (env-load filename) priv [modname])
          *dummy-env*)))


(memoize (global-name env-from-file))


;;----------------------------------------------------------------
;; Load imports from an object file


;; Return the location of the object file, given the module name.
;; This is either a MIN file listed in *compile-mods*, or a
;; bundled module ("///mod.min"), or nil if neither are found.
;;
(define (mod-find name)
  &private
  (define `bundlevar (concat "///" (notdir name) ".min"))

  (or (firstword (filter (concat "%" (notdir name) ".min") *compile-mods*))
      ;; don't use bundles when the runtime is given as a module file
      (if (and (not (filter "%/runtime.min" *compile-mods*))
               (bound? bundlevar))
          bundlevar
          (print "warning: cannot find module " name))))


;; Read the environment exported from a module.
;;
;; mod  : module name
;; priv : if true, read all entries.  Otherwise, discard imported/private
;;        entries and mark other entries as imports.
;;
;; Return: ENV     on success
;;         nil     on failure  (module not found)
;;
(define (require-module mod priv)
  (env-from-file (mod-find mod) priv))


;; Perform a compile-time import of "mod", returning env entries to be added
;; to the using module.
;;
;; Returns:  ENV  on success
;;           nil  on failure (module not found)
;;
(define (use-module mod)
  (let ((imports (require-module mod nil)))
    (if imports
        (let-global ((SCAM_MODS *compile-mods*))
          (^require mod)
          (or (strip-vec (foreach e imports
                                  (if (type? "X" (hash-value e))
                                      e)))
              *dummy-env*)))))


;; Find original environment for `name` and return entries that preceded it.
;;
(define (env-rewind-x env name)
  (let ((pair (hash-find name env))
        (env env)
        (name name))
    (define `defn (hash-value pair))
    (define `priv (word 3 defn))
    (define `mod (filtersub "i%" "%" priv))
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
;;  defn = definition (in env) [when rewinding to an "F" definition]
;;
(define (env-rewind env name defn)
  (append (hash-find "$" env)
          ;; non-inline binding
          (and defn
               (not (filter "#" (word 2 defn)))
               (hash-bind name (wordlist 1 2 defn)))
          (env-rewind-x env name)))


;;--------------------------------------------------------------
;; Default (base) environment
;;--------------------------------------------------------------


;; those that accept one argument
(define builtins-1
  &private
  (concat "abspath basename dir error eval firstword flavor"
          " info lastword notdir origin realpath shell sort"
          " strip suffix value warning wildcard words"))

;; two arguments
(define builtins-2
  &private
  "addprefix addsuffix filter filter-out findstring join word")

;; three arguments
(define builtins-3
  &private
  "foreach patsubst subst wordlist")

;; others: if accepts 2 or 3, and/or accept one or more
(define builtins-other
  &private
  "if and or call")

(define (builtin-argc name)
  (cond ((filter name "if")          "2 or 3")
        ((filter name "and or call") "%")
        ((filter name builtins-3)    3)
        ((filter name builtins-2)    2)
        (else                        1)))  ;; (filter name builtins-1)

(define base-env
  (append
   ;; `.foreach` names the actual builtin; `foreach` is a special form
   (foreach b (append builtins-1 builtins-2
                      builtins-3 builtins-other)
            (hash-bind (concat (if (filter "subst foreach" b) ".") b)
                       ["B" b "b"]))

   ;; Make special variables & SCAM-defined variables
   ;; See http://www.gnu.org/software/make/manual/make.html#Special-Variables
   (hash-bind "*args*" ["V" "^av" "b"])
   (foreach v ["MAKEFILE_LIST" ".DEFAULT_GOAL"]
            (hash-bind v ["V" v "b"]))))


;; Resolve a symbol to its definition, or return nil if undefined.
;; For non-symbols, return "-" (meaning, essentially, "not applicable").
;;
(define (resolve form env)
  (define `(find-name form hash)
    ;; equivalent to `(hash-find (symbol-name form) hash)` but quicker
    (filter (concat (word 2 form) "!=%") hash))

  (if (symbol? form)
      (hash-value (or (find-name form env)
                      (find-name form base-env)))
      "-"))
