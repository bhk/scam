;;----------------------------------------------------------------
;; compile.scm
;;----------------------------------------------------------------

(require "core")
(require "parse")
(require "gen")
(require "gen0")
(require "gen1")
(require "io")
(require "memo")

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
  ;; Load macros. We don't directly call these modules, but they register
  ;; functions called from gen0.
  (require "macros"))

(and nil
     ;; We don't use this module, but we want it bundled with the compiler.
     (require "utf8"))


;; Root directory for intermediate files.
(define *obj-dir* &public ".scam/")

;; When non-nil, emit progress messages.
(define *is-quiet* &public nil)


;; Display a progress message.
;;
(define (build-message action file)
  (or *is-quiet*
      (write 2 (concat "... " action " " file "\n"))))


;; Return the name of the DB file for caching compilation results.
;;
(define (compile-cache-file)
  (concat *obj-dir*
          (hash-file (word 1 (value "MAKEFILE_LIST")))
          ".cache"))


(define (compile-eval text)
  (eval text))


;; Return entries in vector V that do not appear in vector FLT.
;; This may also be used to operation on dictionaries.
;;
(define (literal-filter-out flt v)
  (if (findstring "%" flt)
      ;; escaping would be expensive
      (subst "!P" "%" (filter-out (subst "%" "!P" flt)
                                  (subst "%" "!P" v)))
      (filter-out flt v)))


;; Return transitive closure of a one-to-many relationship.
;; Ordering is per first ocurrence in a breadth-first search.
;;
(define (descendants fn children ?out)
  (define `new-children
    (literal-filter-out (concat children " " out)
                        (fn (first children))))

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


;; Filter bindings by scope.
;;
;; ENV = the final environment of the module
;; SCOPES = "x" for public bindings only; "p x" for private and public
;;
(define (env-filter-scope env scopes)
  (strip-vec
   (foreach b env
            (if (filter scopes (EDefn.scope (dict-value b)))
                b))))


;; Generate "exports" comment line for MIN file
;; SCOPE = see env-filter-scope
;;
(define (env-export-line env scope)
  (concat "# Exports: " (env-compress (env-filter-scope env scope)) "\n"))


;; Return all bindings exported from a MIN file.  The keys of non-public
;; entries are prefixed with "(".
;;
(define (env-parse lines)
  (subst "!n" "\n"
         (env-expand (first (filtersub ["# Exports: %"] "%" lines)))))


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
;;      identifies a bundle, but when no such bundle is found the object
;;      code will be loaded directly from the file.
;;
;;                  (normal)         (normal)        *is-boot*
;;                  Source File      Builtin         Source File
;;                  ------------     ------------    ------------
;;   NAME           io               'io             io
;;   ORIGIN         io.scm           'io             io.scm
;;   ID             io               'io             'io
;;   Load File      .scam/io.min                     .scam/io.min
;;   Load Bundle                     [mod-'io]
;;   Bundle as      [mod-io]         [mod-'io]       [mod-'io]
;;


;; Return the file that holds (or will hold) the module's compiled code
;; (valid only for modules compiled from source).
;;
(define (modid-file id)
  (concat *obj-dir* (patsubst "'%" "%" id) ".min"))


;; Return the bundle variable that holds (or will hold) the modules's code.
;;
(define `(modid-var id)
  (concat "[mod-" id "]"))


;; Return the first 4 lines of a compiled module as an array of lines.
;;
(define (modid-read-lines id ?max)
  (if (and (filter "'%" id) (not *is-boot*))
      (wordlist 1 (or max 99999999) (split "\n" (value (modid-var id))))
      (begin
        (memo-io (global-name hash-file) (modid-file id))
        (read-lines (modid-file id) (and max 1) max))))


;; Scan a builtin module for `require` dependencies.
;;
(define (modid-deps id)
  (let ((lines (modid-read-lines id 4)))
    (assert lines) ;; TODO?
    (promote (filtersub [(concat "# Requires: %")] "%" lines))))


;; Return the environment exported from a module, given its ID.
;;
(define (modid-import id all)
  (env-strip-imports (env-parse (modid-read-lines id 4))
                     all))


(define `(module-opath orgn)
  (escape-path (basename orgn)))


;; Construct the ID corresponding to a module origin.  When building the
;; compiler (booting), we add a "'" prefix to allow them to coexist with
;; user source modules of the same name.
;;
(define (module-id orgn)
  (or (filter "'%" orgn)
      (concat (if *is-boot* "'") (module-opath orgn))))


(define (module-object-file orgn)
  (modid-file (module-id orgn)))


;; Get the "origin" of the module (a SCAM source file or a builtin bundle).
;; Note that when source is found, a compiled version may not be present.
;; Return nil on failure.
;;
;; SOURCE-FILE =  the source file calling `require`
;; NAME = the literal string passed to "require"
;;
(define (module-locate source-file name)
  (or (firstword
       (foreach dir (append (dir source-file)
                            (subst ":" " " (value "SCAM_LIBPATH")))
                (file-exists? (resolve-path dir (concat (basename name)
                                                        ".scm")))))

      ;; builtin?
      (and (not *is-boot*)
           ;; Either "name" or "'name" will match "'name"...
           (let ((id (subst "''" "'" (concat "'" name))))
             (if (bound? (modid-var id))
                 id)))))


;; Skip initial comment lines.
;;
(define (skip-comments lines)
  (if (filter ["#%" ""] (word 1 lines))
      (skip-comments (rest lines))
      lines))


;; Construct a bundle for a compiled module.
;;
(define (construct-bundle id keep-syms)
  (let ((lines (modid-read-lines id))
        (var (modid-var id))
        (keep-syms keep-syms))
    (define `headers (wordlist 1 4 lines))
    (define `env (env-parse headers))
    (define `req (concat (first (filter ["# Req%"] headers)) "\n"))
    (concat "\ndefine " var "\n"
            (if keep-syms
                (concat req (env-export-line env "x")))
            (concat-vec (skip-comments lines) "\n")
            "\nendef\n")))


;; This preamble makes the resulting file both a valid shell script and a
;; valid makefile.  When invoked by the shell, the script invokes `make` to
;; process the script as a makefile.
;;
;; LC_ALL=C allows makefiles to contain non-UTF-8 byte sequences, which is
;; needed to enable SCAM's UTF-8 support.
;;
;; Some make distros (Ubuntu) ignore the environment's SHELL and set it to
;; /bin/sh.  We set it to bash rather than bothering to test `io` with
;; others.
;;
(define `prologue
"#!/bin/bash
:; for v in \"${@//!/!1}\" ; do v=${v// /!0} ; v=${v//	/!+}; a[++n]=${v:-!.} ; done ; LC_ALL=C SCAM_ARGS=${a[*]} exec make -Rr --no-print-directory -f\"$0\" 9>&1
SHELL:=/bin/bash
")

(define `(epilogue main-id main-func)
  (concat "$(eval $(value " (modid-var "'runtime") "))\n"
          "$(call ^start," main-id "," main-func ",$(value SCAM_ARGS))\n"))


;; Extend the runtime's ^require to handle file-based modules during
;; compilation.
;;
(define (load-ext id)
  (begin
    (eval (concat "include " (modid-file id)))
    1))


(declare (compile-module-and-test src-file is-test))


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
  &public
  (let ((orgn (module-locate base name))
        (private private))
    (define `id (module-id orgn))

    (or (if (not orgn)
            (ModError (sprintf "cannot find %q" name)))
        (if (not (filter "'%" orgn))
            (if (compile-module-and-test orgn private)
                (ModError (sprintf "compilation of %q failed" orgn))
                ;; success => nil => proceed to ordinary result
                nil))
        (let ((exports (modid-import id private))
              (id id) (orgn orgn))
          (or (if (has-xmacro? exports)
                  (if *is-boot*
                      ;; do not require at run-time, and
                      (ModError "module has executable macros (boot=true)")
                      ;; require module and continue
                      (call "^require" id)))
              (ModSuccess id exports))))))


;;----------------------------------------------------------------

;; Get the name of the runtime module.
;;
(define (runtime-module-name source)
  (if *is-boot*
      ;; When booting, build runtime from source, and avoid a circular
      ;; dependency.  Avoid treating "runtime.scm" as an implicit
      ;; dependency of runtime-q.scm to avoid a circular dependency on
      ;; testing.
      (filter-out (basename (subst "-q.scm" ".scm" [source])) "runtime")
      ;; Normal compilation: use builtin
      "'runtime"))


;; Return an initial environment (standard prelude).
;;
;; We construct this environment by effectively calling `require` on
;; "implicit" modules (user programs do not know they exist).  Normally this
;; pulls in symbols from a builtin module, but during compiler "boot" phase
;; this will ensure compilation of these modules from source.
;;
(define (compile-prelude source)
  &public

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


;; Replace the first line with a blank line if it begins with "#".
;;
(define (trim-hashbang text)
  (if (filter "#%" (word 1 text))
      (concat "\n" (concat-vec (rest (split "\n" text)) "\n"))
      text))


;; Compile a SCAM source file and all ites dependencies.
;;
;; INFILE = source file name (to be read)
;;
;; Returns: nil on success, error description on failure.
;;
(define (do-compile-module infile)
  (define `text (trim-hashbang (memo-read-file infile)))
  (define `outfile (modid-file (module-id infile)))
  (define `imports (compile-prelude infile))

  (build-message "compiling" infile)

  (let ((o (compile-text text imports infile outfile))
        (infile infile)
        (outfile outfile))
    (define `errors (dict-get "errors" o))
    (define `exe (dict-get "code" o))
    (define `env-out (dict-get "env" o))
    (define `reqs (dict-get "require" o))

    (if errors
        ;; Error
        (begin
          (for e errors
               (info (describe-error e text infile)))
          (error "compilation failed"))

        ;; Success
        (begin
          (define `content
            (concat "# Requires: " reqs "\n"
                    (env-export-line env-out "p x")
                    exe))

          (mkdir-p (dir outfile))
          (memo-write-file outfile content)))))


(define (compile-module infile)
  (memo-call (global-name do-compile-module) infile))


(define (error-if desc)
  (if desc
      (error desc)))


;; Construct a bundled executable from a compiled module.
;;
;; EXE-FILE = exectuable file to create
;; MAIN-ID = module ID for the main module (previously compiled, so that
;;     object files for it and its dependencies are available).
;;
(define (do-link exe-file main-id)
  (build-message "linking" exe-file)

  (define `main-func (gen-global-name "main" nil))
  (define `roots (uniq (append main-id
                               (foreach m (runtime-module-name nil)
                                        (module-id m)))))

  (define `bundles
    (let ((mod-ids (descendants modid-deps roots)))
      ;; Symbols are valuable only if 'compile is present
      (define `keep-syms (filter "'compile" mod-ids))
      (concat-for id mod-ids ""
                  (construct-bundle id keep-syms))))

  (define `exe-code
    (concat prologue bundles (epilogue main-id main-func)))

  (memo-write-file exe-file exe-code)
  (shell (concat "chmod +x " (quote-sh-arg exe-file))))


(define (link exe-file main-id)
  (memo-call (global-name do-link) exe-file main-id))


(define (do-run exe)
  ;; ensure it begins with "./" or "/"
  (build-message "running" exe)

  ;; track dependency for memoization
  (memo-io (global-name hash-file) exe)

  (define `cmd-name (concat (dir exe) (notdir exe)))
  (define `cmd-line
    (concat "TEST_DIR=" (quote-sh-arg *obj-dir*) " "
            (quote-sh-arg cmd-name) " >&2 ;"
            "echo \" $?\""))

  (lastword (logshell cmd-line)))


(define (run exe)
  (memo-call (global-name do-run) exe))


;; Compile a module and test it.
;;
(define (do-compile-module-and-test src-file untested)
  (define `test-src (subst ".scm" "-q.scm" src-file))
  (define `test-mod (module-id test-src))
  (define `test-exe (basename (modid-file test-mod)))

  (or (compile-module src-file)
      (and (not untested)
           (file-exists? test-src)
           (or (compile-module test-src)
               (link test-exe test-mod)
               (if (filter-out 0 (run test-exe))
                   (error (concat test-src " failed")))))))


(define (compile-module-and-test src-file untested)
  (memo-on (compile-cache-file)
           (memo-call (global-name do-compile-module-and-test) src-file untested)))


;; Compile a SCAM program.
;;
;; EXE-FILE = exectuable file to create
;; SRC-FILE = source file of the main module
;; OPTS = command-line options
;;
(define (compile-program exe-file src-file)
  &public
  (define `main-id (module-id src-file))
  (memo-on (compile-cache-file)
           (begin
             (error-if (compile-module-and-test src-file 1))
             (link exe-file main-id)
             nil)))


;; Compile a program and then execute it.
;;
(define (compile-and-run src-file argv)
  &public

  (define `exe-file
    (basename (module-object-file src-file)))

  ;; Option 1: link and run via rule

  (compile-program exe-file src-file)
  (define `run-cmd
    (concat "SCAM_ARGS=" (quote-sh-arg argv)
            " make -f " (quote-sh-arg exe-file)))

  (compile-eval (concat ".PHONY: [run]\n"
                        "[run]: ; @" (subst "$" "$$" run-cmd)))

  ;; Option 2: directly load and run module
  ;; (compile-module ...)
  ;; todo: prevent main & compile-and-run from being traced
  ;; (trace (value "SCAM_ARGS"))
  ;; (eval (concat "include " outfile))
  ;; (declare (main a))
  ;; (trace (value "SCAM_ARGS"))
  ;; (main argv)
  nil)
