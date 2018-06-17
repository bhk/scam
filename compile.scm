;;----------------------------------------------------------------
;; compile.scm
;;----------------------------------------------------------------

(require "core")
(require "parse")
(require "gen")
(require "gen0")
(require "gen1")
(require "io")

;; The following diagram summarizes the stages of compiling a SCAM
;; expression:
;;
;;               pos                 env
;;                |                   |
;;                v                   v
;;   text    +---------+   form   +------+    IL    +------+   exe
;;  -------->|  parse  |--------->|  c0  |--------->|  c1  |-------->
;;           +---------+          +------+          +------+
;;                |                   |                 |    errors
;;                v                   v                 +----------->
;;               pos                 env
;;
;; Each expression begins at a position "pos" (a numeric index into the
;; sequence of tokens in the subject text).  Parsing emits a "form" (an AST
;; node) and a position at which to look for subsequent expressions.
;;
;; The compiler front end (c0) operates on a form and an environment (a set
;; of symbol bindings), and emits an IL node and a new environment, since
;; expressions (e.g. `declare` and `define`) can alter the environment for
;; subsequent expressions.
;;
;; The compiler back end (c1) emits executable code (Make source) and a
;; (hopefully empty) vector of errors.  The form and IL data structures can
;; convey errors as well as successful results; c1 must output a separate
;; value for error information.


(begin
  ;; Load macros. We don't directly call from this module, but it registers
  ;; functions called from gen0.
  (require "macros"))

(and nil
     ;; Do not load these modules, but treat them as dependencies and bundle
     ;; them with the compiler.
     (require "utf8")
     (require "scam-ct"))


;; *file-mods* is a list of IDs of modules that should be read from files at
;; run-time, rather than bundles.This enables interactive mode and
;; executable macros, and simplifies unit-testing.
(define *file-mods* nil)

;; Root directory for compiled binaries.
(define *obj-dir* &public ".scam/")

;; When non-nil, comile, link, and test operations emit messages.
(define *is-quiet* &public nil)


;;--------------------------------------------------------------
;; Module management
;;--------------------------------------------------------------

;; We have different ways of referring to modules in different contexts:
;;
;;  NAME: This is the string given as a literal argument to (require).
;;
;;  ORIGIN: If NAME identifies a source file, it will be the complete path to
;;       the source file path.  If NAME identifies a builtin module, it will be
;;       that module's ID (which begins with `'`).
;;
;;  ID: This is what will be passed to ^require at run-time.  Ordinarily it
;;      identifies a bundle, but when modules execute in the context of the
;;      compiler and ID is found in *file-mods*, the object code will be
;;      loaded directly from the file.
;;
;;                  --boot           not --boot       not --boot
;;                  Source File      Source File      Builtin
;;                  ----------       ---------        -----------
;;   ORIGIN         io.scm           io.scm           'io
;;   ID             'io              io               'io
;;   Load File      .scam/io.min     .scam/io.min
;;   Load Bundle                                      [mod-'io]
;;   Bundle as      [mod-'io]         [mod-io]        [mod-'io]
;;


;; Return the file that holds (or will hold) the module's compiled code
;; (valid only for modules compiled from source).
;;
(define (modid-file id)
  &public
  (concat *obj-dir* (patsubst "'%" "%" id) ".min"))


;; Return the bundle variable that holds (or will hold) the modules's code.
;;
(define `(modid-var id)
  (concat "[mod-" id "]"))


(define (module-opath origin)
  (escape-path (basename origin)))


;; Construct the ID corresponding to a module origin.  When building the
;; compiler (booting), we add a "'" prefix to allow them to coexist with
;; user source modules of the same name.
;;
(define (module-id origin)
  &public
  (or (filter "'%" origin)
      (concat (if *is-boot* "'") (module-opath origin))))


(define (module-var origin)
  &public
  (modid-var (module-id origin)))


(define `(module-is-source? origin)
  &public
  (filter-out "'%" origin))


(define (module-has-binary? origin)
  (or (filter "'%" origin)
      (filter (module-id origin) *file-mods*)))


;; Get the "origin" of the module (a SCAM source file or a builtin bundle).
;; Note that when source is found, a compiled version may not be present.
;; Return nil on failure.
;;
;; SOURCE-FILE =  the source file calling `require`
;; NAME = the literal string passed to "require"
;;
(define (locate-module source-file name)
  &public
  (define `srcname
    (if *is-boot*
        (patsubst "'%" "%" name)
        name))

  (or (firstword
       (foreach dir (append (dir source-file)
                            (subst ":" " " (value "SCAM_LIBPATH")))
                (file-exists? (resolve-path dir (concat srcname ".scm")))))

      ;; builtin?
      (and (not *is-boot*)
           ;; Either "name" or "'name" will match "'name"...
           (let ((id (subst "''" "'" (concat "'" name))))
             (if (bound? (modid-var id))
                 id)))))


;; Return the first 4 lines of a compiled module as an array of lines.
;;
(define (module-read-lines origin)
  (if (filter "'%" origin)
      (wordlist 1 4 (split "\n" (value (module-var origin))))
      (read-lines (modid-file (module-id origin)) 1 4)))


;;----------------------------------------------------------------
;; Environment Imports/Exports
;;----------------------------------------------------------------

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
  (define `(EDefn.set-scope defn scope)
    (append (wordlist 1 2 defn)
            scope
            (nth-rest 4 defn)))

  (define `scope (word 3 defn))

  (if (filter "x" scope)
      {=key: (EDefn.set-scope defn "i")}))


;; Import bindings from another module. Return an environment.
;;
;; ENV = env containing exported bindings
;; ALL = whether to return all bindings.
;;    If true, return the final environment of the module.
;;    If false, return only public bindings from the final env.
;;
(define (env-strip-imports env all)
  (if all
      ;; Add all symbols, public and private.
      env
      ;; Add only public symbols.
      (strip-vec
       (foreach b env
                (import-binding (dict-key b) (dict-value b))))))


;; Discard imported bindings.  Leave other public and private bindings.
;;
;; ENV = the final environment of the module
;;
(define (env-strip-exports env)
  (strip-vec
   (foreach b env
            (if (not (filter "i" (EDefn.scope (dict-value b))))
                b))))


;; Generate "exports" comment line for MIN file
;;
(define (env-export-line env)
  (concat "# Exports: " (env-compress (env-strip-exports env)) "\n"))


;; Return all bindings exported from a MIN file.  The keys of non-public
;; entries are prefixed with "(".
;;
(define `(env-parse lines)
  (subst "!n" "\n"
         (env-expand (first (filtersub ["# Exports: %"] "%" lines)))))


;; Return the environment exported from a module, given its ID.
;;
(define (env-import origin all)
  (env-strip-imports (env-parse (module-read-lines origin))
                     all))


(memoize (global-name env-import))

;; Extend the runtime's ^require to handle file-based modules during
;; compilation.
;;
(define (load-ext id)
  (if (filter id *file-mods*)
      (begin
        (eval (concat "include " (modid-file id)))
        1)))


(declare (compile-module infile outfile excludes))


;; Locate or create the compiled form of a module.
;;
;;   NAME = file name or module name
;;   BASE = directory/file to which NAME may be relative
;;   PRIVATE = include private as well as public bindings
;;
;; Returns: a CMod record
;;
(define (get-module name base private is-use)
  &public
  (let ((origin (locate-module base name))
        (private private))
    (define `id (module-id origin))
    (define `exports (env-import origin private))

    (or (if (not origin)
            (ModError (sprintf "cannot find %q" name)))
        (if (not (module-has-binary? origin))
            (begin
              (or *is-quiet*
                  (print "... compiling " origin))
              (if (compile-module origin (modid-file id) nil)
                  (ModError (sprintf "compilation of %q failed" origin))
                  ;; success => nil => proceed to ordinary result
                  (set *file-mods* (append *file-mods* id)))))
        (if is-use
            (begin
              (call "^require" id)
              (ModSuccess id origin (strip-vec
                                   (foreach e exports
                                        (case (dict-value e)
                                          ((EXMacro _ _) e))))))
            (ModSuccess id origin exports)))))


;; Return the exports from a module.  Errors are fatal. (!)
;;
(define (get-module-env name is-use)
  (let ((o (get-module name "." nil is-use)))
    (case o
      ((ModSuccess id origin exports)
       exports)
      ((ModError desc)
       (error desc)))))


;;----------------------------------------------------------------

;; Return an initial environment (standard prelude).
;;
;; We construct this by calling `require` and `use` on "implicit" modules,
;; bute that is a compiler implementation detail.  User programs do not know
;; of these modules, and should always receive builtin versions.  When
;; compiling the compiler itself, however, we obtain these from source
;; files.
;;
;; EXCLUDES = string of characters that disable implicit dependencies:
;;            "" = default; "R" avoids runtime; "C" avoids scam-ct, "RC" both.
;;
;; On error, the result wil contain {=ErrorMarkerKey:...} pair.
;;
(define (compile-prelude excludes)
  &public
  (append (if (not (findstring "R" excludes))
              (get-module-env "'runtime" nil))
          (if (and (not (findstring "C" excludes))
                   (not *is-boot*))
              (get-module-env "'scam-ct" "use"))))


;; Compile SCAM source to executable code.
;;
;; Returns:
;;    { code: CODE, errors: ERRORS, env: ENV-OUT, requires: MODS, uses: MODS }
;;
;; TEXT = SCAM source
;; ENV = Initial environment. This is normally generated by compile-prelude.
;;       It includes exports from implicit modules, unless the file being
;;       compiled is itself an implicit module.  When called from the REPL,
;;       this will contain additional bindings from the user's session.
;; INFILE = Input file name (or '[command line]').
;; IS-FILE = When nil, code will be compiled for function syntax.  When
;;           non-nil, code will be compiled for file syntax.
;;
(define (compile-text text env infile is-file)
  &public
  (let-global ((*compile-subject*  (penc text))
               (*compile-file*     infile))

    (c0-block-cc env
                 (parse-subject *compile-subject*)
                 (lambda (env-out nodes)
                   (concat (gen1 nodes is-file) " " {env: env-out})))))


(define (construct-file infile env exe reqs uses)
  (define `(to-ids origins)
    (for m origins
         (module-id m)))
  (concat "# compiled from " infile "\n"
          (if reqs (concat "# Requires: " (to-ids reqs) "\n"))
          (if uses (concat "# Uses: " (to-ids uses) "\n"))
          (env-export-line env)
          exe))


;; Replace the first line with a blank line if it begins with "#".
;;
(define (trim-hashbang text)
  (if (filter "#%" (word 1 text))
      (concat "\n" (concat-vec (rest (split "\n" text)) "\n"))
      text))


(define (implicit-mod name flag flags)
  (if (not (findstring flag flags))
      (if *is-boot*
          (concat name ".scm")
          (concat "'" name))))


;; Compile a SCAM source file and write out a .min file.  On failure,
;; display errors.
;;
;; INFILE = source file name (to be read)
;; OUTFILE = object file name (to be written)
;; EXCLUDES = see compile-prelude
;;
;; Returns: nil on success, error description on failure.
;;
(define (compile-module infile outfile excludes)
  &public
  (define `text (trim-hashbang (read-file infile)))

  (define `imports
    (let-global ((*compile-file* infile))
      (compile-prelude excludes)))

  (let ((o (compile-text text imports infile outfile))
        (ireq (implicit-mod "runtime" excludes "R"))
        (iuse (implicit-mod "scam-ct" excludes "C"))
        (infile infile)
        (outfile outfile))
    (define `errors (dict-get "errors" o))
    (define `exe (dict-get "code" o))
    (define `env-out (dict-get "env" o))
    (define `reqs (append (dict-get "require" o) ireq))
    (define `uses (append (dict-get "use" o) iuse))

    (if errors
        ;; Error
        (begin
          (for e errors
               (info (describe-error e text infile)))
          (subst "S" (if (word 2 errors) "s" "")
                 "compilation errorS"))
        ;; Success
        (begin
          (mkdir-p (dir outfile))
          (write-file outfile
                      (construct-file infile env-out exe reqs uses))))))


(define (error-if desc)
  (if desc
      (error desc)))


;; Compile a SCAM source file and write out a .min file.  On failure,
;; display errors and terminate execution.
;;
;; INFILE = source file name (to be read)
;; OUTFILE = object file name (to be written)
;; FILE-MODS = module origins that have been compiled
;; EXCLUDES = see compile-prelude
;;
(define (compile-file infile outfile file-mods excludes)
  &public
  (let-global ((*file-mods* (for m file-mods
                                 (if (module-is-source? m)
                                     (module-id m)))))
    (error-if (compile-module infile outfile excludes))))
