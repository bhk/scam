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
;; convey errors as well as successful results; c1 must output a separate
;; value for error information.


(require "core.scm")
(require "parse.scm")
(require "gen.scm")
(require "gen0.scm")
(require "gen1.scm")
(require "io.scm")
(require "memo.scm")


(begin
  ;; Load macros. We don't directly call these modules, but they register
  ;; functions called from gen0.
  (require "macros.scm"))

(and nil
     ;; We don't use this module, but we want it bundled with the compiler.
     (require "utf8.scm"))


;; When non-nil, emit progress messages.
(define *is-quiet* &public nil)

;; Files currently being compiled (to check for cycles)
(define *compiling* nil)


;; Display a progress message.
;;
(define (build-message action file)
  (or *is-quiet*
      (write 2 (concat "... " action " " file "\n"))))


(define `(drop-if cond ?fail ?succ)
  (if cond
      (begin fail (memo-drop) 1)
      (begin succ nil)))


(define (bail-if message)
  (drop-if message (fprintf 2 "scam: %s\n" message)))


;; Return the name of the DB file for caching compilation results.
;;
(define `(compile-cache-file)
  (declare ^uid &native)
  (concat *obj-dir* ^uid ".db"))


;; Evaluate EXPR within a memo session for compilation.
;;
(define `(compile-memo-on expr)
  (memo-on (compile-cache-file) expr))


;; Return transitive closure of a one-to-many relationship.
;; Ordering is per first ocurrence in a breadth-first search.
;;
(define (descendants fn children ?out)
  (define `new-children
    (vec-subtract (fn (first children))
                  (concat children " " out)))

  (if children
      (descendants fn (append (rest children) new-children)
                   (append out (word 1 children)))
      out))


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
  (subst ";" "!A" "\\" "!B" "," "!C" "'" "!D" "[" "!E" "]" "!F" "|" "!G"
         "@" "!H" "{" "!I" "}" "!J" "#" "!K" "\"" "!L" "&" "!M" "(" "!N"
         "!0" ";" "!11" "\\" "\\1" "," "!10" "'" ";\\" "[" ";i[:" "]"
         ":IL0" "|" ":IL4" "@" "!=!1:EDefn" "{" "{1;~%;i;" "}" "}1 " "#"
         "}2 " "\"" "{1;:;i;" "&" "&2;!1.;!1:IL" "(" s))

(define (env-exp s)
  (subst "(" "&2;!1.;!1:IL" "&" "{1;:;i;" "\"" "}2 " "#" "}1 " "}" "{1;~%;i;"
         "{" "!=!1:EDefn" "@" ":IL4" "|" ":IL0" "]" ";i[:" "[" ";\\" "'" "!10"
         "," "\\1" "\\" "!11" ";" "!0" "!N" "(" "!M" "&" "!L" "\"" "!K" "#"
         "!J" "}" "!I" "{" "!H" "@" "!G" "|" "!F" "]" "!E" "[" "!D" "'"
         "!C" "," "!B" "\\" "!A" ";" s))


;; Tokenize the key within the binding (it usually occurs once).
;;
(define `(tokenize-key v)
  (foreach w v
           (concat
            (word 1 (subst "!=" "!= " w))
            (subst "%" "!p" (word 1 (subst "!=" " " w)) "%"
                   (word 2 (subst "!=" "!= " w))))))

(define `(detokenize-key v)
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
          (foreach prefix (append "Exports" (if all "Private"))
                   (promote (filtersub (concat ["# "] prefix [": %"])
                                       "%" lines))))))


(define (export-defn name rec)
  (define `(EDefn.set-scope rec scope)
    (append (wordlist 1 2 rec) scope (nth-rest 4 rec)))

  (concat (EDefn.scope rec) ":"
          {=name: (EDefn.set-scope rec "i")}))


;; Generate two comment lines that describe public and private bindings.
;;
(define `(env-export-lines env)
  ;; Prefix each entry with its scope (e.g. "x:..." or "p:...")
  ;; and replace the scope with "i".
  (define `(prefix-entries e)
    (filter "p:% x:%"
            (foreach b (dict-compact e)
                     (export-defn (dict-key b) (dict-value b)))))

  (let ((e (prefix-entries env)))
    (concat "# Exports: " (env-compress (filtersub "x:%" "%" e)) "\n"
            "# Private: " (env-compress (filtersub "p:%" "%" e)) "\n")))


;;--------------------------------------------------------------
;; Module management
;;--------------------------------------------------------------

;; We have different ways of referring to modules in different contexts:
;;
;; NAME: This is the literal string argument to `require`.
;;
;; ORIGIN: If NAME identifies a source file, it will be the path to the
;;    source file, ending in ".scm".  If NAME identifies a builtin
;;    module, it will be that module's ID (which begins with `'`).
;;
;; ID: This is passed to ^R at run-time.  The bundle variable name
;;    and the compiled module name are based on this string, which is
;;    escaped using `escape-path`.  Modules that are bundled with the
;;    compiler are named differently to avoid conflicts with user
;;    modules [both user modules and builtin modules can be bundled in a
;;    user program].
;;
;;                  (normal)         (normal)        *is-boot*
;;                  Source File      Builtin         Source File
;;                  ------------     ------------    ------------
;;   NAME           io.scm           io              io.scm
;;   ORIGIN         io.scm           io              io.scm
;;   ID             io.scm           io              io
;;   Load File      .scam/io.scm.o                   .scam/io.o
;;   Load Bundle                     [mod-io]
;;   Bundle as      [mod-io.scm]     [mod-io]        [mod-io]
;;


;; Return the file that holds (or will hold) the module's compiled code
;; (valid only for modules compiled from source).
;;
;; Note: this must be kept consistent with the behavior of `^load`.
;;
(define `(modid-file id)
  (concat *obj-dir* id ".o"))


;; Return the bundle variable that holds (or will hold) the modules's code.
;;
;; Note: this must be kept consistent with the behavior of `^load`.
;;
(define `(modid-var id)
  (concat "[mod-" id "]"))


;; Non-nil when ID names a compiled module, not a bundled one.
;;
(define `(modid-is-file id)
  (or (filter "%.scm" id) *is-boot*))


;; Return the first 4 lines of a compiled module as an array of lines.
;;
(define (modid-read-lines id ?max)
  (if (modid-is-file id)
      ;; load file
      (begin
        (memo-hash-file (modid-file id))
        (read-lines (modid-file id) (and max 1) max))
      ;; load bundle
      (wordlist 1 (or max 99999999) (split "\n" (value (modid-var id))))))


;; Scan a builtin module for `require` dependencies.
;;
(define (modid-deps id)
  (let ((lines (modid-read-lines id 4)))
    ;; should not happen... module was not compiled?
    (assert lines)
    (promote (filtersub [(concat "# Requires: %")] "%" lines))))


;; Return the environment exported from a module, given its ID.
;;
(define (modid-import id all)
  (env-parse (modid-read-lines id 4) all))


;; Construct the ID corresponding to a module origin.
;;
(define (module-id orgn)
  (let ((e (escape-path orgn)))
    (if *is-boot*
        (basename e)
        e)))


;; Get the "origin" of the module.  This is either the path of a SCAM
;; source file (ending in .scm) or a builtin module ("core", etc.).
;; Return nil on failure.
;;
;; SOURCE-DIR = directory containing the source file calling `require`
;; NAME = the literal string passed to "require"
;;
(define (locate-module source-dir name)
  (define `path-dirs
    (addsuffix "/" (split ":" (value "SCAM_LIBPATH"))))

  (or (and (filter "%.scm" [name])
           (vec-or
            (for dir (cons source-dir path-dirs)
                 (wildcard (resolve-path dir name)))))
      (and (not *is-boot*)
           (if (bound? (modid-var name))
               name))))


;; do-locate-module is safe to memoize because:
;;  1. results will not change during a program invocation.
;;  2. it does not call memo-io or memo-call
;;
(memoize (native-name locate-module))

(define (m-locate-module source-dir name)
  (memo-io (native-name locate-module) source-dir name))


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

  (concat "\ndefine " (modid-var id) "\n"
          (concat-vec body "\n") "\n"
          "endef\n"))


;; This preamble makes the resulting file both a valid shell script and a
;; valid makefile.  When invoked by the shell, the script invokes `make` to
;; process the script as a makefile.
;;
;; LC_ALL=C allows makefiles to contain non-UTF-8 byte sequences, which is
;; needed to enable SCAM's UTF-8 support.
;;
(define `(construct-file main-id bundles uid)
  (concat
   "#!/bin/bash\n"
   ":; for v in \"${@//!/!1}\" ; "
   "do v=${v// /!0} ; v=${v//\t/!+}; a[++n]=${v:-!.} ; done ; "
   "LC_ALL=C "
   "SCAM_ARGS=${a[*]} "
   "exec make -Rr --no-print-directory -f\"$0\" 9>&1\n"
   "SCAM_MOD := " main-id "\n"
   "^uid := " uid "\n"
   bundles
   "$(eval $(value " (modid-var "runtime") "))\n"))


;;----------------------------------------------------------------
;; Module compilation
;;----------------------------------------------------------------

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
  (word 1 (foreach pair env
                   (case (dict-value pair)
                     ((EXMacro _ _) 1)))))


;; Locate or create the compiled form of a module.
;;
;;   NAME = file name or module name
;;   BASE = directory/file to which NAME may be relative
;;   PRIVATE = include private as well as public bindings
;;
;; Returns: a CMod record
;;
(define (get-module name base private)
  (let ((orgn (m-locate-module (dir base) name))
        (private private))
    (define `id (module-id orgn))

    (or (if (not orgn)
            (ModError (sprintf "cannot find %q" name)))
        (if (filter "%.scm" [orgn])
            (if (m-compile-and-maybe-test-module orgn private)
                (ModError (sprintf "compilation of %q failed" orgn))
                ;; success => nil => proceed to ordinary result
                nil))
        (let ((exports (memo-blob-call (native-name modid-import) id private))
              (id id) (orgn orgn))
          (or (if (has-xmacro? exports)
                  (if *is-boot*
                      ;; do not require at run-time, and
                      (ModError "module has executable macros (boot=true)")
                      ;; require module and continue
                      (call "^R" id)))
              (ModSuccess id exports))))))


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
    (let ((o (get-module name "." nil)))
      (case o
        ((ModSuccess id exports)
         exports)
        ((ModError desc)
         (error desc)))))

  (foreach m (runtime-module-name source)
           (get-module-env m)))


;; Compile SCAM source to executable code.
;;
;; Returns:
;;    { code: CODE, errors: ERRORS, env: ENV-OUT, requires: MODS }
;;
;; TEXT = SCAM source
;; ENV = Initial environment. This is normally generated by compile-prelude.
;;       It includes exports from implicit modules, unless the file being
;;       compiled is itself an implicit module.  When called from the REPL,
;;       this will contain additional bindings from the user's session.
;; FILE = Input file name (or '[command line]').
;; FORMS-IN = if non-nil, the results of (parse-text text).
;; IS-FILE = When nil, code will be compiled for function syntax.  When
;;           non-nil, code will be compiled for file syntax.
;;
(define (parse-and-gen text env file is-file ?forms-in)
  (let-global ((*compile-subject*  (penc text))
               (*compile-file*     file))
    (define `forms
      (or forms-in
          (parse-subject *compile-subject*)))

    (let ((o (gen0 forms env))
          (is-file is-file))
      (define `env-out (first o))
      (define `nodes (rest o))
      (concat (gen1 nodes is-file) " " {env: env-out}))))


;; Replace the first line with a blank line if it begins with "#".
;;
(define (trim-hashbang text)
  (if (filter "#%" (word 1 text))
      (concat "\n" (concat-vec (rest (split "\n" text)) "\n"))
      text))


(define `(check-cycle file)
  (if (vec-intersect *compiling* [file])
      (bail-if (concat "dependency loop: "
                       (concat-vec (conj *compiling* file) " -> ")))))


;; Return parsed form of file.  This is a separate function for the sake
;; of memoizing it separately, so we can avoid re-parsing unchanged
;; sources even when they need to be recompiled due to changing imports.
;;
(define (parse-file file)
  (parse-text (trim-hashbang (memo-read-file file))))


;; Compile a SCAM source file and all ites dependencies.
;;
;; On success, return `nil`.
;; On failure, display message and return 1.
;;
;; FILE = source file name (to be read)
;;
(define (compile-module file)
  (define `text (trim-hashbang (memo-read-file file)))
  (define `outfile (modid-file (module-id file)))
  (define `imports (compile-prelude file))

  (or
   (check-cycle file)
   (let-global ((*compiling* (conj *compiling* file)))

     (build-message "compile" file)

     (define `forms
       (memo-blob-call (native-name parse-file) file))

     (let ((o (parse-and-gen text imports file outfile forms))
           (file file)
           (outfile outfile))
       (define `errors (dict-get "errors" o))
       (define `exe (dict-get "code" o))
       (define `env-out (dict-get "env" o))
       (define `reqs (dict-get "require" o))

       (drop-if
        errors
        ;; Error case
        (for e errors
             (info (describe-error e text file)))

        ;; Success
        (begin
          (define `content
            (concat "# Requires: " reqs "\n"
                    (env-export-lines env-out)
                    exe))

          (bail-if (memo-write-file outfile content))))))))


;; Return a vector of all direct and indirect module depdendencies of ID
;; and include ID.
;;
(define (modid-deps-all id)
  (define `runtime-id
    (foreach m (runtime-module-name nil)
             (module-id m)))
  (descendants modid-deps (uniq (append id runtime-id))))


;; Construct a bundled executable from a compiled module.
;;
;; EXE-FILE = exectuable file to create
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
        (concat-for id mod-ids ""
                    (construct-bundle id keep-syms)))

      (define `uid
        (hash-output (concat
                      "cat "
                      (foreach id mod-ids
                               (if (modid-is-file id)
                                   (quote-sh-arg (modid-file id)))))))

      (construct-file main-id bundles uid)))

  (bail-if (or (memo-write-file exe-file exe-code)
               (memo-chmod-file exe-file "+x"))))


;; Link and run a test module.
;; On success, return `nil`.
;; On failure, return 1.
;;
(define (run src argv show-status)
  (define `mod (module-id src))

  (if show-status
      (build-message "run" src))

  ;; This will read mod and its descendants, marking them as
  ;; dependencies; we don't need to track other IO.
  (modid-deps-all mod)

  (define `runner
    (if *is-boot*
        (modid-file "runtime")
        (firstword MAKEFILE_LIST)))

  ;; (value "MAKE") does not sem to provide the actual value
  (declare MAKE &native)

  (define `cmd-line
    (concat "SCAM_ARGS=" (quote-sh-arg argv) " "
            MAKE " -f " (quote-sh-arg runner) " "
            "SCAM_MOD=" (quote-sh-arg mod) " "
            "SCAM_DIR=" (quote-sh-arg *obj-dir*) " "
            "1>&9 ; echo \" $?\""))

  (drop-if (filter-out 0 (lastword (ioshell cmd-line)))))


;; Compile a module and test it.
;; On success, return `nil`.
;; On failure, display message and return 1.
;;
(define (compile-and-test-module src-file)
  (define `test-src (subst ".scm" "-q.scm" src-file))

  (or (m-compile-module src-file)
      (and (file-exists? test-src)
           (or (m-compile-module test-src)
               (if (memo-call (native-name run) test-src nil 1)
                   (bail-if (concat test-src " failed")))))))


;; Compile SCAM source text and write an executable file.
;;
;; On success, return `nil`.\
;; On failure, display message and return 1.
;;
;; EXE-FILE = name of an exectuable file to create.\
;; SRC-FILE = name of the source file of the main module.
;;
(define (build-program src-file exe-file)
  &public
  (define `main-id (module-id src-file))

  (compile-memo-on
   (if (not (memo-hash-file src-file))
       (begin
         (fprintf 2 "scam: file '%s' does not exist\n" src-file)
         1)
       (or (m-compile-and-maybe-test-module src-file nil)
           (memo-call (native-name link) exe-file main-id)))))


;; Compile and execute a SCAM source file.
;;
;; On success, return `nil`.\
;; On failure, display message and return 1.
;;
(define (run-program src-file argv)
  &public
  (compile-memo-on
   (or (m-compile-and-maybe-test-module src-file nil)
       (run src-file argv nil))))


;; Compile SCAM source code to a function.
;;
;; TEXT = SCAM source\
;; FILE = the file from which the source was obtained; this will be
;;        available to the compiled code via `(current-file)`.\
;; ENV = Initial environment.  If nil, SCAM's initial bindings will be
;;       supplied.  Otherwise, it must begin with the initial bindings.
;;
;; Returns: `{ code: CODE, errors: ERRORS, env: ENV-OUT, requires: MODS }`
;;
(define (compile-text text file ?env-in)
  &public
  (define `env (or env-in (compile-prelude nil)))
  (compile-memo-on
   (parse-and-gen text env file nil)))
