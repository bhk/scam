;;--------------------------------------------------------------
;; gen : code generation utilities
;;
;; Compilation is divided into two stages:
;;
;;   c0:  form --> IL        [gen0.scm]
;;   c1:  IL --> object      [gem1.scm]
;;
;; A form is an AST node, as described in parse.scm, with one additional
;; node that is used for code generation purposes:
;;
;;   I <IL>   -->  IL pass-through
;;
;; "Object" is GNU Make syntax, e.g. `$(subst $1,$2,$3)`.
;;
;; "IL" is an intermediate language, structured as a tree of vectors.  The
;; first word of each vector describes its type.  IL constructs map closely
;; to GNU Make constructs.
;; 
;;   Q str               ->   literal           str
;;   V str               ->   var reference     $(str)
;;   F str <a> <b> ...   ->   call builtin      $(str <a>,<b>,...)
;;   f str <a> <b> ...   ->   call user func    $(call str,<a>,<b>,...)
;;   Y <f> <a> <b> ...   ->   lambda call       $(call ^Y,<a>,...,<f>)
;;   C <a> <b> <c> ...   ->   concatenation     <a><b><c>
;;   R str               ->   raw object code   str
;;   B <a> <b> <c> ...   ->   sequence (block)
;;   X <a>               ->   nested function   quote (c1 <a>)
;;   E <description>     ->   error
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
;; The environment is a hash that maps symbol names to *definitions*.  An
;; additional constraint on the environment is that order is significant:
;; newer bindings are prepended and redundant ones are not removed, so it can
;; be thought of as a stack (similar to the layering of nested scopes in the
;; source file).
;;
;; Each binding maps a symbol name to a definition, which is a vector of one
;; of the following forms:
;;
;;    F <name> <priv> <dfn>  ==  function variable (recursive) or compound macro
;;    V <name> <priv>        ==  data variable (simple)
;;    B <name> <priv>        ==  builtin <name>
;;    M <form> <priv>        ==  symbol macro
;;    A <arg>  <priv>        ==  function argument
;;    I <il>                 ==  pre-compiled IL node
;;
;; The special key "$" is used for "lambda markers" that identify the
;; current level of function nesting.  Values for these bindings are:
;;
;;    $ <level>       == lambda marker
;;
;; <name> = actual name of global function/variable or builtin
;;          The name "#" indicates that the binding is a symbol macro, and
;;          not an actual function variable.
;;
;; <priv> = "p" if declared/defined with "&private"
;;          "b" if a default (base) environment member
;;          nil for ordinary in-file declarations/definitions (to be exported)
;;          "i<FILENAME>" if imported from via "require".  <FILENAME> is
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


;; *compile-text* contains the SCAM source being compiled.
(declare *compile-text*)

;; *compile-subject* contains (penc *compile-text*)
(declare *compile-subject*)

;; *compile-file* is the name of the source file neing compiled.
(declare *compile-file*) 

;; *compile-outfile* is the name of the MIN file being generated.  This is
;; used to find modules when `require` expressions are encountered.
(declare *compile-outfile*)


;; Construct vector of [a b c], omitting trailing empty items.
;;
(define (trimvec a b c)
  &private
  (concat [a]
          (if (or b c)
              (concat " " [b]
                      (if c
                          (concat " " [c]))))))

;;--------------------------------------------------------------
;; Environment functions
;;--------------------------------------------------------------

(define (filtersub pat repl str)
  &private
  (patsubst pat repl (filter pat str)))


;; Add a binding for symbol `sym` to `env`.  `type` = "F", "V", ...
(define (bind-sym symbol type priv fdef env)
  (append 
   (bind (symbol-name symbol)
         (concat type " " (trimvec (symbol-name symbol) priv fdef))
         env)))


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
  (append (find "$" env)
          (after (find name env) env)))


;; Construct an error node.  The resulting error node inherits the document
;; index from `form`.
;;
(define (gen-error form fmt ...)
  [(concat "E." (form-index form)) (vsprintf (rest *args*))])


;; Display a warning during compilation.
;;
(define (compile-warn form fmt a b c)
  (info (describe-error (gen-error form fmt a b c)
                        *compile-text*
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
                                   (concat-vec (map-call "form-typename" type)
                                               " or ")))))))


;; cnt = number, or string containing more than one number, e.g. "2 or 3"
;;
(define (check-argc cnt args form)
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
(define (env-compress v)
  (subst "," "!a" ";" "!b" ":" "!c" "/" "!d" "=" "!e" "|" "!f" "~" "!g"
         "!!e" "=" "!11111" "!5" "!1111" "!4" "!111" "!3" "!11" "!2"
         "!0" "," "!10" ";" "!20" ":" "!30" "/" "!40" "|" "!1." "~" "\n" "!n"
         v))

;; env-expand: undo env-compress
;;
(define (env-expand v)
  (subst "!n" "\n" "~" "!1." "|" "!40" "/" "!30" ":" "!20" ";" "!10" "," "!0"
         "!2" "!11" "!3" "!111" "!4" "!1111" "!5" "!11111" "=" "!!e"
         "!g" "~" "!f" "|" "!e" "=" "!d" "/" "!c" ":" "!b" ";" "!a" ","
         v))


;; Return all bindings exported from a MIN file.  The keys of non-public
;; entries are prefixed with "(".
;; 
(define (env-parse text)
  (env-expand (first (filtersub ["# Exports:%"] "%" (split "\n" text)))))


(define (tag-binding key defn filename)
  (bind key (append (wordlist 1 2 defn)
                    (concat "i" (if (or (nth 4 defn)
                                        (filter "M%" (word 1 defn)))
                                    [filename]))
                    (nth-rest 4 defn))))


;; Import the bindings from a MIN file's exports. Return an environment.
;;
;; exports = parsed exports
;; priv = whether or not to include import "&private" members
;;    If true, all members are returned.
;;    If false, only public members are imported, and imported
;;        macros/inline functions will be tagged with `filename`.
;; filename = name of the MIN file containing the exports
;;
(define (env-import exports priv filename)
  (if priv
      (patsubst "(%" "%" exports)
      (foreach b (filter-out "(%" exports)
               (tag-binding (hash-key b) (hash-value b) filename))))


;; Generate "exports" comment line for MIN file
;;
(define (env-export env)
 (concat "# Exports: "
          (env-compress 
           (foreach b env
                    (concat
                     (if (nth 3 (hash-value b))
                         "(")
                     b)))
          "\n"))


(define (cache funcname a b c)
  (define `varname (concat "**memo*" funcname "*" a "*" b "*" c "**"))
  (if (not (bound? varname))
      (set-global varname (call funcname a b c)))
  (value varname))


(define (mod-read filename)
  (if (filter "///%" filename)
      (value filename)
      (read-file filename)))


(define (env-load filename)
  (env-parse (mod-read filename)))


(memoize "env-load")


;; Import symbols from `filename`.  `priv` means return all
;; original environment entries, not just public ones.
;;
(define (env-from-file filename priv)
  (env-import (env-load filename) priv filename))

(memoize "env-from-file")


;; Find original environment for `name` and return entries that preceded it.
;;
(define (env-rewind-x env name)
  (let ((pair (find name env))
        (env env)
        (name name))
    (define `defn (hash-value pair))
    (define `_priv (word 3 defn))
    (define `_file (filtersub "i%" "%" _priv))
    (if _file
        (env-rewind-x (env-from-file (promote _file) 1) name)
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
  (append (find "$" env)
          ;; non-inline binding
          (and defn
               (not (filter "#" (word 2 defn)))
               (bind name (wordlist 1 2 defn)))
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
            (bind (concat (if (filter "subst foreach" b) ".") b)
                  ["B" b "b"]))

   ;; Make special variables & SCAM-defined variables
   ;; See http://www.gnu.org/software/make/manual/make.html#Special-Variables
   (foreach v ["*file*" "*args*" "MAKEFILE_LIST" ".DEFAULT_GOAL" "MAKE_RESTARTS"
               ".RECIPEPREFIX" ".VARIABLES" ".FEATURES" ".INCLUDE_DIRS"]
            (bind v ["V" v "b"]))))



;; Resolve a symbol to its definition, or return nil if undefined.
;; For non-symbols, return "-" (meaning, essentially, "not applicable").
;;
(define (resolve form env)
  ;(if (symbol? form)
  ;    (or (get (symbol-name form) env)
  ;        (get (symbol-name form) base-env))
  ;    "-"))))
  (if (symbol? form)
      (nth 2 (subst "!=" " " 
                    (or (filter (concat (word 2 form) "!=%") env)
                        (filter (concat (word 2 form) "!=%") base-env))))
      "-"))
