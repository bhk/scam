;; # compile: SCAM Compilation
;;
;; The following diagram summarizes the stages of compiling a SCAM
;; expression:
;;
;;                  pos                 env
;;                   |                   |
;;                   v                   v
;;      text    +---------+   form   +------+    IL    +------+   exe
;;     -------->|  parse  |--------->|  c0  |--------->|  c1  |-------->
;;              +---------+          +------+          +------+
;;                   |                   |                 |    errors
;;                   v                   v                 +----------->
;;                  pos                 env
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
;; convey errors as well as successful results, so the previous stages do
;; not need a separate error output value.
;;
;; ## Build Directory
;;
;; During compilation, intermediate files are created under a directory
;; called the "build directory".  Subsequent compilations with the same
;; object directory can proceed faster by reusing these results.  This
;; defaults to ".scam/" if NIL is passed.
;;
;; No two compilations should occur at the same time (e.g. in different
;; instances of SCAM) using the same directory.


(require "core.scm")
(require "parse.scm")
(require "gen.scm")
(require "gen0.scm")
(require "gen1.scm")
(require "io.scm")
(require "memo.scm")
(require "escape.scm")

;; "macros" has no exports, but its functions are called from gen0 via
;; computed names.
(require "macros.scm")


;; When non-nil, emit progress messages.
(declare *is-quiet*)


;; The directory containing the memo DB file and object files.
;;
(declare *obj-dir*)


;; A vector of files being compiled (to check for cycles)
(define *compiling* nil)


;; Display a progress message.
;;
(define (build-message action file)
  (or *is-quiet*
      (write 2 (.. "... " action " " file "\n"))))


(define `(drop-if cond ?on-fail ?on-succ)
  (if cond
      (begin on-fail (memo-drop) 1)
      (begin on-succ nil)))


(define (bail-if message)
  (drop-if message (fprintf 2 "scam: %s\n" message)))


;; Return a hash of the version of the running compiler and the working
;; directory.
;;
(define `(get-instance)
  (declare ^uid &native)
  (declare cache-instance)
  (or cache-instance
      (set cache-instance (hash-output "echo %s $(pwd)" ^uid))
      cache-instance))


;; Evaluate EXPR within a memo session for compilation.
;;
(define `(compile-memo-on build-dir is-quiet expr)
  (define `fix-build-dir
    (if build-dir
        (patsubst "%//" "%/" (.. build-dir "/"))
        ".scam/"))

  (let-global ((*obj-dir* (.. fix-build-dir (get-instance) "/"))
               (*is-quiet* is-quiet))
    (memo-on (.. *obj-dir* "db") expr)))


;; Return transitive closure of a one-to-many relationship.
;; Ordering is per first occurrence in a breadth-first search.
;;
(define (descendants fn children ?out)
  (define `new-children
    (vec-subtract (fn (first children))
                  (._. children out)))

  (if children
      (descendants fn (append (rest children) new-children)
                   (append out (word 1 children)))
      out))


;;----------------------------------------------------------------
;; Environment Imports/Exports
;;----------------------------------------------------------------

;; Each generated object file includes two comment lines that describes the
;; symbols that were declared or defined in the module (and visible in the
;; final environment state).
;;
;;     "# Exports: " DEFNS
;;     "# Private: " DEFNS
;;
;; The "Exports" like includes `&public` symbols, and the "Private" line
;; includes non-public symbols.
;;
;; Each DEFNS value is a compressed form of the vector of EDefn entries that
;; make up the environment.
;;
;; Exports are consumed when `(require MOD)` is compiled.  The "Exports"
;; line is read from MOD, added to the caller's environment, and marked as
;; imported (EDefn.scope = "i").
;;
;; When `(require MOD &private)` is compiled, both the "Exports" and
;; "Private" lines are added to the current environment.
;;
;; When a module is bundled, the "Private" line is omitted.
;;
;; Lambda markers and local variables are not exported -- it is actually
;; impossible for them to exist in the final environment because the end of
;; the file is necessarily outside of any lambda context.


;; env-cmp and env-exp were generated by envcomp.scm.

(define (env-cmp s)
  (subst "'" "!A" "[" "!B" "]" "!C" "|" "!D" "@" "!E" "{" "!F" "}" "!G"
         "#" "!H" "!10" "'" "!11" "[" "1:IL" "]" "!0[:IL" "|" "!=!1:EDefn"
         "@" "!0!]7!0" "{" "@1!0i!0%!0" "}" "@2!0i!0..!0" "#" s))

(define (env-exp s)
  (subst "#" "@2!0i!0..!0" "}" "@1!0i!0%!0" "{" "!0!]7!0" "@" "!=!1:EDefn"
         "|" "!0[:IL" "]" "1:IL" "[" "!11" "'" "!10" "!H" "#" "!G" "}"
         "!F" "{" "!E" "@" "!D" "|" "!C" "]" "!B" "[" "!A" "'" s))


;; Tokenize the key within the binding (it usually occurs once).
;;
(define `(tokenize-key v)
  (foreach (w v)
    (define `N (word 1 (subst "!=" " " w)))
    (define `V (word 2 (subst "!=" ". " w)))
    (.. N "!=" (subst "%" "!p" (.. "`" N) "%" V))))


(define `(detokenize-key v)
  (foreach (w v)
    (define `N (word 1 (subst "!=" " " w)))
    (define `V (word 2 (subst "!=" ". " w)))
    (.. N "!=" (subst "%" (.. "`" N) "!p" "%" V))))


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
(define `(env-expand str)
   (subst "!n" "\n"
          (detokenize-key
           (env-exp str))))


;; Return all bindings exported from a MIN file.  The keys of non-public
;; entries are prefixed with "(".
;;
(define (env-parse lines all)
  (subst "!n" "\n"
         (env-expand
          (foreach (prefix (._. "Exports" (if all "Private")))
            (promote (filtersub [(.. "# " prefix ": %")]
                                "%" lines))))))


;; Generate two comment lines that describe public and private bindings.
;;
(define `(env-export-lines env)
  (define `(export-defn name rec)
    (.. (EDefn.scope rec) ":" {=name: (EDefn.set-scope rec "i")}))

  ;; Prefix each entry with its scope (e.g. "x:..." or "p:...")
  ;; and replace the scope with "i".
  (define `(prefix-entries e)
    (filter "p:% x:%"
            (foreach ({=k: v} (dict-compact e))
              (export-defn k v))))

  (let ((e (prefix-entries env)))
    (.. "# Exports: " (env-compress (filtersub "x:%" "%" e)) "\n"
        "# Private: " (env-compress (filtersub "p:%" "%" e)) "\n")))


;;--------------------------------------------------------------
;; Module management
;;--------------------------------------------------------------

;; There are two types of modules: source modules and builtin modules
;; (bundled with the compiler).
;;
;; A module's NAME is the string passed to `require`.  If it ends in ".scm"
;; it identifies a source module; otherwise it refers to a builtin.
;;
;; A module's ID is the string passed to `^R` at run-time.
;;
;;                 Source File       Source File      Builtin
;;                 (normal)          *is-boot*
;;                 ------------      ------------     ------------
;;   Name          X.scm             B.scm            B
;;   Source        D/X.scm           B.scm            --
;;   ID            .scam/D/X.scm     B                B
;;   Object File   <ID>.o            .scam/<ID>.o
;;   Bundle        [mod-<ID>]        [mod-<ID>]       [mod-<ID>]


;; Get ID for a builtin
;;
(define `(modid-from-builtin name)
  name)


;; Get ID for a source module.  Ordinarily this is the location of the
;; object file, excluding the ".o" extension.  However, when *is-boot* is
;; true (we are building the compiler), the ID of a source modules is
;; `(basename NAME)` so that they can later serve as builtin modules.
;;
;; We include the complete path in the ID -- instead of always treating it
;; as local to *obj-dir* -- because the ID will be baked into generated code
;; (of the requiring module) that may be executed after it is returned from
;; `compile-text` -- at which point *obj-dir* is indeterminate.  [In an
;; *is-boot* scenario, `compile-text` is not supported.]
;;
(define (modid-from-source path)
  (if *is-boot*
      (basename (escape-path path))
      (.. *obj-dir* (escape-path path))))


;; Non-nil when ID names a module being compiled from source, not read from
;; a bundle.
;;
(define `(modid-is-source? id)
  (or (filter "%.scm" [id]) *is-boot*))


;; Return the file that will hold the module's compiled code after it is
;; compiled.
;;
(define `(modid-object id)
  (if *is-boot*
      (.. *obj-dir* id ".o")
      (.. id ".o")))


;; Return the bundle variable for a module.  In the case of builtin modules,
;; this is the bundle from which the module's object code will be read.  In
;; the case of all modules, this is the variable into which the object code
;; will be bundled (when an executable is linked).
;;
;; Note: this must be kept consistent with the behavior of `^load`.
;;
(define `(modid-var id)
  (.. "[mod-" id "]"))


;; Return the first 4 lines of a compiled module as an array of lines.
;;
(define (modid-read-lines id ?max)
  (if (modid-is-source? id)
      ;; load file
      (begin
        (memo-hash-file (modid-object id))
        (read-lines (modid-object id) (and max 1) max))
      ;; load bundle
      (wordlist 1 (or max 99999999) (split "\n" (native-value (modid-var id))))))


;; Scan a builtin module for `require` dependencies.
;;
(define (modid-deps id)
  (let ((lines (modid-read-lines id 4)))
    ;; should not happen... module was not compiled?
    (assert lines)
    (promote (filtersub ["# Requires: %"] "%" lines))))


;; Return the environment exported from a module, given its ID.
;;
(define (modid-import id all)
  (env-parse (modid-read-lines id 4) all))


;; Find a source file in the base directory or one of the LIBPATH
;; directories.  Return the resolved path, or nil if not found.
;;
(define (locate-source base-dir name)
  (define `path-dirs
    (addsuffix "/" (split ":" (native-var "SCAM_LIBPATH"))))

  (vec-or
   (for (dir (cons base-dir path-dirs))
     (file-exists? (resolve-path dir name)))))


;; locate-source is safe to memoize because:
;;  1. results will not change during a program invocation.
;;  2. it does not call memo-io or memo-call
;;(memoize (native-name locate-source))


(define (m-locate-source base name)
  (memo-io (native-name locate-source) base name))


;; Skip initial comment lines, retaining comment lines that match
;; the pattern RETAIN-PAT.
;;
(define (skip-comments lines retain-pat)
  (if (filter ["#%" ""] (word 1 lines))
      (append (filter retain-pat (word 1 lines))
              (skip-comments (rest lines) retain-pat))
      lines))


;; Construct a bundle for a compiled module.
;;
(define (construct-bundle id keep-syms)
  (define `body
    (skip-comments (modid-read-lines id)
                   (if keep-syms
                       ["# Req%" "# Exp%"])))

  (.. "\ndefine " (modid-var id) "\n"
      (concat-vec body "\n") "\n"
      "endef\n"))


;; This preamble makes the resulting file both a valid shell script and a
;; valid makefile.  When invoked by the shell, the script invokes `make` to
;; process the script as a makefile.
;;
;; LC_ALL=C allows makefiles to contain non-UTF-8 byte sequences, which is
;; needed to enable SCAM's UTF-8 support.
;;
(define `(construct-file main-mod bundles uid)
  ;; Initial line = bash "boot"
  (.. "#!/bin/bash\n"
      ":; for v in \"${@//!/!1}\" ; "
      "do v=${v// /!0} ; v=${v//\t/!+}; a[++n]=${v:-!.} ; done ; "
      "LC_ALL=C "
      "SCAM_ARGS=${a[*]} "
      "exec make -Rr --no-print-directory -f\"$0\" 9>&1\n"
      ;; Remaining lines = makefile content
      "SCAM_MAIN := "
      (protect-rhs (.. [main-mod] " " (gen-native-name "main" nil))) "\n"
      "^uid := " uid "\n"
      bundles
      "$(eval $(value " (modid-var "runtime") "))\n"))


;;----------------------------------------------------------------
;; Module compilation
;;----------------------------------------------------------------


(data Mod
  &public
  ;; ID = string to be passed to ^R
  ;; ENV = exported environment entries
  (ModSuccess id &list env)
  (ModError message))


(declare (compile-module src-file))
(declare (compile-and-test-module src-file))

(define `(m-compile-module file)
  (memo-call (native-name compile-module) file))

(define `(m-compile-and-maybe-test-module file untested)
  (memo-call (if untested
                 (native-name compile-module)
                 (native-name compile-and-test-module))
             file))


;; Return 1 if ENV contains an EXMacro record, nil otherwise.
;;
(define `(has-xmacro? env)
  (word 1 (foreach ({=_: value} env)
            (case value
              ((EXMacro _ _) 1)))))


;; See get-module.
;;
(define (get-source-module name private path)
  (or (if (not path)
          (ModError (sprintf "cannot find `%s`" name)))
      (if (m-compile-and-maybe-test-module path private)
          (ModError (sprintf "compilation of `%s` failed" path)))
      (ModSuccess (modid-from-source path)
                  (memo-blob-call (native-name modid-import)
                                  (modid-from-source path) private))))


;; See get-module.
;;
(define `(get-builtin-module name)
  (if *is-boot*
      (ModError "standard modules not available with --boot"))
      (if (native-bound? (modid-var name))
          (ModSuccess name (modid-import name nil))
          (ModError (sprintf "standard module `%s` not found" name))))


;; Get Mod record for a module, compiling it if necessary.
;;
;; NAME = string passed to `require`
;; BASE = directory/file to which NAME may be relative
;; PRIVATE = include private as well as public bindings
;;
(define (get-module name base private)
  (define `mod
    (if (filter "%.scm" [name])
        (get-source-module name private (m-locate-source (path-dir base) name))
        (get-builtin-module name)))

  (let ((mod mod))
    (or (case mod
          ((ModSuccess id exports)
           (if (has-xmacro? exports)
               (if *is-boot*
                   ;; do not require at run-time
                   (ModError "module has executable macros (boot=true)")
                   ;; require module and continue
                   ;; TODO: mark binary as a dependency!
                   (begin
                     (memo-hash-file (modid-object id))
                     (native-call "^R" id))))))
        mod)))


;; Generate IL nodes including a call to ^R and a "require" crumb to track
;; the module dependency, and wrap it in an IEnv that exports the new
;; symbols.
;;
;; Result = IL node that calls REQUIRE + includes a "require crumb"
;;
(define (M.require env sym args)
  (define `[module] args)
  (define `flags (get-flags args))
  (define `body (skip-flags args))
  (define `mod-name (string-value module))
  (define `read-priv (filter "&private" flags))

  (or
   (if body
       (gen-error (first body) "too many arguments to require"))

   (case module
     ((PString _ name)
      (case (get-module name *compile-file* read-priv)
        ((ModError message)
         (gen-error module "require: %s" message))
        ((ModSuccess id exports)
         (define `arg (IConcat [(IString id) (ICrumb "require" id)]))
         (IEnv exports (ICall "^R" [arg])))))
     (else
      (err-expected "Q" module sym "STRING" "(require STRING)")))))


;; Get the name of the runtime module, or nil if the runtime should not
;; be used with SOURCE.  When booting, we build the runtime from source.
;; To avoid a circular dependencies, we skip the runtime as an implicit
;; dependency of runtime.scm or runtime-q.scm.
;;
(define (runtime-module-name source)
  (if *is-boot*
      (filter-out (subst "-q.scm" ".scm" [source]) "runtime.scm")
      "runtime"))


;; Return an initial environment (standard prelude).
;;
;; We construct this environment by effectively calling `require` on
;; "implicit" modules (user programs do not know they exist).  Normally this
;; pulls in symbols from a builtin module, but during compiler "boot" phase
;; this will ensure compilation of these modules from source.
;;
(define (compile-prelude source)
  (define `(get-module-env name)
    (case (get-module name "." nil)
      ((ModSuccess id exports)
       exports)
      ((ModError desc)
       (error desc))))

  (foreach (m (runtime-module-name source))
    (get-module-env m)))


;; Compile SCAM source to executable code.
;;
;; Returns:
;;    { code: CODE, errors: ERRORS, env: ENV-OUT, require: MODS }
;;
;; TEXT = SCAM source
;; ENV = Initial environment. This is normally generated by compile-prelude.
;;       It includes exports from implicit modules, unless the file being
;;       compiled is itself an implicit module.  When called from the REPL,
;;       this will contain additional bindings from the user's session.
;; FILE = Input file name (or '[command line]').
;; IS-FILE = When nil, code will be compiled for function syntax.  When
;;           non-nil, code will be compiled for file syntax.
;;
(define (parse-and-gen text env file is-file)
  (if (not text)
      {errors: [(PError 0 (.. "File is empty or does not exist"))]}

      (let-global ((*compile-subject*  (penc text))
                   (*compile-file*     file))
        (let (([env-out ...nodes] (gen0 (parse-subject *compile-subject*) env))
              (is-file is-file))
          (._. (gen1 nodes is-file) {env: env-out})))))


;; Replace the first line with a blank line if it begins with "#".
;;
(define (trim-hashbang text)
  (if (filter "#%" (word 1 text))
      (.. "\n" (concat-vec (rest (split "\n" text)) "\n"))
      text))


(define `(check-cycle file)
  (if (vec-intersect *compiling* [file])
      (bail-if (.. "dependency loop: "
                   (concat-vec (conj *compiling* file) " -> ")))))


;; Compile a SCAM source file and all its dependencies, writing
;; the object file to `(modid-object (modid-from-source FILE))`.
;;
;; On success, return `nil`.
;; On failure, display message and return 1.
;;
;; FILE = source file name (to be read)
;;
(define (compile-module file)
  (define `text (trim-hashbang (memo-read-file file)))
  (define `imports (compile-prelude file))

  (or
   (check-cycle file)
   (let-global ((*compiling* (conj *compiling* file)))

     (build-message "compile" file)

     (let (({errors: errors, code: exe, env: env-out, require: reqs}
            (parse-and-gen text imports file 1))
           (file file))
       (define `outfile (modid-object (modid-from-source file)))
       (define `content
         (.. "# Requires: " reqs "\n"
             (env-export-lines env-out)
             exe))

       (drop-if
        errors
        ;; Error case
        (for (e errors)
          (print (describe-error e text file)))

        ;; Success
        (bail-if (memo-write-file outfile content)))))))


;; Return a vector of all direct and indirect module dependencies of ID
;; and include ID.
;;
(define (modid-deps-all id)
  (define `runtime-id
    ;; Use `foreach` as cheap `let`
    (foreach (m (runtime-module-name nil))
      (if m
          [ (if (filter "%.scm" m)
                (modid-from-source m)
                (modid-from-builtin m)) ])))

  (descendants modid-deps (uniq (cons id runtime-id))))


;; Construct a bundled executable from a compiled module.
;;
;; EXE-FILE = executable file to create
;; MAIN-ID = module ID for the main module (previously compiled, so that
;;     object files for it and its dependencies are available).
;;
(define (link exe-file main-id)
  (build-message "link" exe-file)

  (define `exe-code
    (let ((mod-ids (modid-deps-all main-id)))
      ;; Symbols are valuable only if compile is present
      (define `keep-syms
        (filter "compile" mod-ids))

      (define `bundles
        (concat-for (id mod-ids "")
          (construct-bundle id keep-syms)))

      (define `uid
        (hash-output "cat -- %V"
                     (filter-out [""]
                                 (for (id mod-ids)
                                   (if (modid-is-source? id)
                                       (modid-object id))))))

      (construct-file main-id bundles uid)))

  (bail-if (or (memo-write-file exe-file exe-code)
               (memo-chmod-file exe-file "+x"))))


;; Link and run a test module.
;; On success, return `nil`.
;; On failure, return 1.
;;
(define (run src argv show-status)
  (define `mod (modid-from-source src))

  (if show-status
      (build-message "run" src))

  ;; This will read mod and its descendants, marking them as
  ;; dependencies; we don't need to track other IO.
  (modid-deps-all mod)

  (define `runner
    (if *is-boot*
        (modid-object "runtime")
        (firstword (native-var "MAKEFILE_LIST"))))

  ;; (native-value "MAKE") does not seem to provide the actual value
  (declare MAKE &native)

  (define `scam-main
    (.. [mod] " " (gen-native-name "main" nil)))

  (define `scam-dir
    (if *is-boot*
        *obj-dir*))

  (define `run
    (shellf (.. "SCAM_ARGS=%A %s -Rr --no-print-directory -f%A SCAM_MAIN=%A "
                "SCAM_TMP=%A SCAM_DIR=%A 1>&9 ; echo \" $?\"")
            argv MAKE runner scam-main *obj-dir* scam-dir))

  (drop-if (filter-out 0 (lastword run))))


;; Compile a module and test it.
;; On success, return `nil`.
;; On failure, display message and return 1.
;;
(define (compile-and-test-module src-file)
  (define `test-src (subst ".scm" "-q.scm" src-file))

  (or (m-compile-module src-file)
      ;; Does a test file exist?
      (and (memo-hash-file test-src)
           (or (m-compile-module test-src)
               (if (memo-call (native-name run) test-src nil 1)
                   (bail-if (.. test-src " failed")))))))


;; Compile a SCAM program.
;;
;; SRC-FILE = name of the SCAM source file\
;; EXE-FILE = name of an executable file to create.\
;; ARGV = a vector to pass to the program's `main` function\
;; BUILD-DIR = nil, or the [object directory](#object-directory)\
;; IS-QUIET = non-nil to suppress compilation progress messages
;;
;; On success, return `nil`.\
;; On failure, display message and return 1.
;;
(define (build-program src-file exe-file ?build-dir ?is-quiet)
  &public
  (define `main-id (modid-from-source src-file))

  (compile-memo-on
   build-dir is-quiet
   (if (not (memo-hash-file src-file))
       (begin
         (fprintf 2 "scam: file `%s` does not exist\n" src-file)
         1)
       (or (m-compile-and-maybe-test-module src-file nil)
           (memo-call (native-name link) exe-file main-id)))))


;; Compile and execute a SCAM program.
;;
;; SRC-FILE = name of the SCAM source file\
;; ARGV = a vector to pass to the program's `main` function\
;; BUILD-DIR = nil, or the [object directory](#object-directory)\
;; IS-QUIET = non-nil to suppress compilation progress messages
;;
;; On success, return `nil`.\
;; On failure, display message and return 1.
;;
(define (run-program src-file argv ?build-dir ?is-quiet)
  &public
  (compile-memo-on
   build-dir is-quiet
   (or (m-compile-and-maybe-test-module src-file nil)
       (run src-file argv nil))))


;; Compile SCAM source code to a function.
;;
;; TEXT = a string of SCAM source code containing a sequence of one or more
;;     expressions.\
;; FILE = the file from which the source was obtained; this will be
;;     available to the compiled code via `(current-file)`.\
;; ENV-IN = the environment (symbol definitions) visible to TEXT
;;    at the outset.  If nil, SCAM's default environment will be used.
;;    Otherwise, it must be a previously returned ENV-OUT value.\
;; BUILD-DIR = nil, or the [object directory](#object-directory).\
;; IS-QUIET = non-nil to suppress compilation progress messages
;;
;; Result = `{ code: CODE, errors: ERRORS, env: ENV-OUT, requires: MODS }`
;;
;; On success, the `code` member of the result contains a SCAM function that
;; will execute the compiled code, returning the value of the last
;; expression.
;;
;; Example:
;;
;;     > (define result
;;     +   (compile-text "(print 123) (require \"math\") (+ 1 2)" "--"))
;;     > (define f (dict-get "code" result))
;;     > (define out (f))
;;     123
;;     > out
;;     3
;;
(define (compile-text text file ?env-in ?build-dir ?is-quiet)
  &public
  (assert (not *is-boot*))
  (define `env (or env-in (compile-prelude nil)))
  (compile-memo-on
   build-dir is-quiet
   (parse-and-gen text env file nil)))
