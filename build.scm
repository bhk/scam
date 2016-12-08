;; build.scm: build .min files and standalone eecutables
;;
;; Rule Processing
;; ----
;;
;; SCAM's build step leverages Make's built-in rule processing by
;; constructing and eval'ing Make rules:
;;
;;   target: prerequisites...
;;           command
;;
;; When SCAM's `main` function returns, Make proceeds to process all rules,
;; with the first-appearing rule serving as the default target.  The
;; "command" section of a Make rule is expanded just before it is passed to
;; the shell.  This allows us to write anonymous SCAM functions into the
;; command block that do all of the work required to build the target.
;;
;; There are three types of rules that we generate:
;;
;; 1. compile: These rules compile a .scm file to a .min file, using the
;;    `compile-file` function.
;;
;; 2. test: These rules generate a .ok file after executing a .min file, if
;;    it completes successfully.  This is done with a shell command that
;;    invokes `make` to execute the .min file.
;;
;; 3. link: These rules generate a "bundled" executable from one or more
;;    .min files, using the `link` function.
;;
;; Boot Phase
;; ----
;;
;; Building the compiler involves a "boot" step.  First, a "scam" executable
;; (call it version A) is built from some previous, "golden" compiler.  Then
;; version A compiles the "boot" files (the rt-mod and ct-mod modules)
;; before compiling other source files (for whom the boot files are implicit
;; dependencies).
;;
;; When `--boot` is given on the command line:
;;
;;  1. No "bundled" modules will be used.  All dependencies must be
;;     satisfied by source files.
;;  2. The rt-mod and ct-mod source files will be compiled, and this
;;     compilation output will be used to compile all other modules.  These
;;     will be the only rt-mod and ct-mod in the generated program.
;;  3. Tests are executed using:  make -f <rt-mod> SCAM_MAIN=<TEST.MIN>
;;
;; When `--boot` is NOT specified:
;;
;;  1. Ordinary dependencies (`require` or `use`) may be satisfied by SCAM
;;     sources *or* by modules that are bundled with the compiler.
;;  2. The boot modules bundled with the compiler will be bundled in the
;;     generated exectuable.
;;  3. Tests are executed using:  make -f *self* SCAM_MAIN=<TEST.MIN>
;;
;;--------------------------------------------------------------

(require "core")
(require "io")
(require "parse")
(require "compile")
(require "gen")


(define (filtersub pat repl str)
  &private
  (patsubst pat repl (filter pat str)))


;; enough for our purposes
(define `(clean-path f)
  (patsubst "./%" "%" (subst "/./" "/" f)))


;; Resolve module name to file path or bundle path.
;;  source = path to source (or object) file that references a module
;;  mod = module mentioned in `(require ...)`, `(use ...)`, or MIN metadata.
;;
(define (resolve-mod source mod)
  (clean-path (concat (dir source) mod (suffix source))))


(define (dbg-print code fmt str)
  (if (findstring code (value "SCAM_DEBUG"))
      (printf fmt str))
  str)


;; Traverse a graph from a set of starting nodes, and return a vector of
;; all nodes.
;;
(define (traverse-graph nodes get-children visited-nodes)
  (let& ((node (first nodes))
         (children (get-children node))
         (new-children (filter-out (append nodes visited-nodes) children))
         (new-nodes (append (rest nodes) new-children))
         (new-visited (append node visited-nodes)))
    (if node
        (cons node (traverse-graph new-nodes get-children new-visited)))))


(define (skip-comments lines)
  (if (or (and lines (not (first lines)))
          (filter "#%" (word 1 (first lines))))
      (skip-comments (rest lines))
      lines))

;; remove initial comment lines from a .min file
;;
(define (strip-comments src)
  (concat-vec (skip-comments (split "\n" src)) "\n"))


;; files is a word list
(define (modnames-of files)
  &private
  (basename (notdir files)))


(define `bundle-dir "///")

;; Return a fake filename that represents a bundled object file (not to be
;; confused with an actual filename).
;;
(define `(bundle-path source)
  (concat bundle-dir (modnames-of source) ".min"))

;; True if path identifies a bundled object file.
;;
(define `(bundled? path)
  (filter (concat bundle-dir "%") path))

;; Return the name of the variable that a bundle, given a source or object
;; file name.
;;
(define `(bundle-var file)
  (concat bundle-dir (notdir file)))

;; Read the contents of a MIN file.
;;
(define (read-object file)
  (if (bundled? file)
      (value (bundle-var file))
      (read-file file)))


;; This preamble makes the resulting file both a valid shell script and a
;; valid makefile.  When invoked by the shell, the script invokes `make` to
;; process the script as a makefile.
;;
(define `prologue
"#!/bin/bash
:; for v in \"${@//!/!1}\" ; do v=${v// /!0} ; v=${v//	/!+}; a[++n]=${v:-!.} ; done ; SCAM_ARGS=${a[*]} exec make --no-print-directory -j ${SCAM_JOBS:-9} -f\"$0\"

")

(define `(epilogue main-mod main-func rt)
  (concat "$(eval $(value " (bundle-var rt) "))\n"
          "$(call ^start," (bundle-var main-mod) "," main-func ",$(SCAM_ARGS))\n"))


;; Construct a bundled executable.  This function is typically executed when
;; rules are expanded during Make's rule processing phase.
;;
;;  runtime = object file to be used as runtime
;;
(define (link outfile objects main runtime keep-syms)
  (define `(bundle object)
    (define `var (bundle-var object))
    (define `text ((if keep-syms identity strip-comments)
                   (read-object object)))
    (concat "\ndefine " var "\n" text "\nendef\n"))

  (write-file outfile
              (concat prologue
                      (concat-for obj objects " "
                               (bundle obj))
                      (epilogue main (gen-global-name "main") runtime)))

  (shell (concat "chmod +x " (quote-sh-arg outfile))))


;; *self* = the compiler itself
(define *self* (firstword MAKEFILE_LIST))


(define *quiet*
  (if (bound? "_@")
      (value "_@")
      "@"))

(define (echo-cmd ...)
  (subst "$" "$$"
         (concat "@ echo " (quote-sh-arg (concat-vec *args*)))))


;; Scan a source file for `require` and `use` dependencies.
;; Returns:  [requires uses]
;;
;; Each "require" statement implies a source file name (relative to the
;; location of the requiring file), but the dependency might actually be
;; satisfied by a bundled module or previously-specified file matching the
;; same module name.  This function returns the implied source file.
;;
(define (scan-source src)
  (define `sedcmd "sed -E 's/ //g;s/^\\((require|use)\"([^\"]*)\".*|.*/\\1\\2/g;/../!d'")

  (let ((out (shell (concat sedcmd " " (quote-sh-arg src)))))
    (filter-out
     "!."
     (foreach symbol "require% use%"
              (demote
               (foreach
                   w (filtersub symbol "%" out)
                   (demote (resolve-mod src w))))))))


;; Scan an object file for `require` and `use` dependencies.
;; Returns:  [requires uses]
;;
(define (scan-object file)
  (let ((lines (wordlist 1 4 (split "\n" (read-object file)))))
    (define `(collect key)
      (foreach m (promote (filtersub [(concat "# " key ": %")] "%" lines))
               (resolve-mod file m)))

    (foreach key "Requires Uses"
             [ (collect key) ])))



(define (strip-comments src)
  (concat-vec (skip-comments (split "\n" src)) "\n"))


;; MMAP = [ MOD... ]
;;
;; MOD = [
;;    modname   ; name passed to require
;;    source    ; nil => no compile step [bundled, supplied as min, file not found]
;;    object    ; object file (given, or to be compiled)
;;    testmod   ; modname of test
;;    requires  ; modnames in `require` deps (explicit or implicit)
;;    uses      ; modnames in `use` deps (explicit or implicit)
;;    is-boot   ; module does NOT have implicit dependencies
;;  ]

(define `(mod-name mod)      (nth 1 mod))
(define `(mod-source mod)    (nth 2 mod))
(define `(mod-object mod)    (nth 3 mod))
(define `(mod-testmod mod)   (nth 4 mod))
(define `(mod-requires mod)  (nth 5 mod))
(define `(mod-uses mod)      (nth 6 mod))
(define `(mod-is-boot mod)   (nth 7 mod))

(define `(mod-deps mod)      (promote (wordlist 5 6 mod)))


(define `(mmap-add mmap modname source object testmod requires uses is-boot)
  (cons [modname source object testmod requires uses is-boot]
        mmap))

;; plural accessors (each takes a list of module names)

(define `(mmap-objects mmap names)
  (foreach m names (mod-object (assoc m mmap))))

(define `(mmap-minfiles mmap names)
  (filter-out "///%" (mmap-objects mmap names)))

(define `(mmap-testmods mmap names)
  (foreach m names (mod-testmod (assoc m mmap))))

(define `(mmap-okfiles mmap names)
  (addsuffix ".ok"
             (mmap-objects mmap (mmap-testmods mmap names))))



;; Return file name if file exists
(define (if-exists file)  (wildcard file))

;; Return bundle name if fiole is bundled
(define (if-bound var)     (if (bound? var) var))
(define (if-bundled file)  (if-bound (bundle-path file)))


;; Return the `require` and `use` dependencies for a module
;;
(define (scan-deps source object)
  (if source
      (scan-source source)
      (scan-object object)))


;; Resolve a set of sources to actual source file, object file, or bundle
;; locations, and recursively descend depencencies.
;;
;; `sources` is a list of files or bundles to be included in the executable.
;;    Files may be `.scm` or `.min`.  Names matching "///MODNAME.min" refer
;;    to bundled modules.
;;
;; `mmap` is a vector of MOD entries (see above)
;;
;; `env.odir` is the directory into which object files should be written.
;;
;; `env.rebundle` is truthy when bundles can be used to satisfy dependencies.
;;
;; `env.boot` is true when the compiled files do NOT have implicit dependencies
;;      on boot files (runtime and scam-ct)
;;
(define (scan-modules env sources mmap)
  (define `file (first sources))
  (define `others (rest sources))
  (define `env.odir (hash-get "odir" env))
  (define `env.rebundle (hash-get "rebundle" env))
  (define `env.ct (hash-get "ct" env))

  (define `modname (modnames-of file))
  (define `source (if-exists (filter "%.scm" file)))
  (define `object
    (or (if source (concat env.odir (modnames-of file) ".min"))
        (if-exists file)
        (if-exists (concat (basename file) ".min"))
        (if env.rebundle (if-bundled file))
        file))

  (cond
   ;; done?
   ((not file) mmap)

   ;; Already visited?
   ((assoc modname mmap) (scan-modules env others mmap))

   (else
    (let ((env env)
          (others others)
          (mmap mmap)
          (modname modname)
          (source source)
          (object object)
          (deps (scan-deps source object)))

      (define `is-boot (hash-get "boot" env))
      (define `requires (append (nth 1 deps)
                                (if is-boot nil (concat rt-mod ".scm"))))
      (define `uses     (append (nth 2 deps)
                                (if is-boot nil (concat ct-mod ".scm"))))
      (define `test-source (if-exists (patsubst "%.scm" "%-q.scm" source)))

      (scan-modules env
                    (append others (promote deps) test-source)
                    (mmap-add mmap modname
                              source
                              object
                              (modnames-of test-source)
                              (modnames-of requires)
                              (modnames-of uses)
                              is-boot))))))


;;======================== Rule Construction ========================

;; Construct a Make "rule"
;;
;;   target = file name
;;   prereqs = list of prerequisites (space-delimited)
;;   commands = vector of commands (function values).  These are evaluated
;;              when the target is built, and the return value is passed
;;              to the shell.
;;
(define (rule target deps oodeps commands)
  (define `prefixed-commands
    (for cmd commands
         (if (filter "@%" (word 1 cmd))
             cmd
             (concat *quiet* cmd))))

  (concat target ": " deps (if oodeps " | ") oodeps
          (concat-vec (cons "" prefixed-commands) "\n\t")
          "\n\n"))


;; Generate a rule for compiling .scm to .min
;;
(define (compile-rule object source deps oodeps mod-files reqs uses is-boot)
  (rule object (append source deps *self*) oodeps
        [ (concat
           "@ true "
           (let ((source source)
                 (mod-files mod-files)
                 (reqs reqs)
                 (uses uses)
                 (is-boot is-boot))
             (lambda ()
               (print "=> compiling " object)
               (compile-file source object is-boot mod-files reqs uses)))) ]))


;; Generate a rule for testing a .min
;;
;;  runtime = runtime module to be used for the executable (either a bundled
;;             module or an actual file)
;;  mod-files = list of compiled files to be used instead of bundled modules
;;
(define (test-rule ok-file test-object runtime mod-files)
  (declare MAKE &global)

  (define `runner
    (if (bundled? runtime)
        (quote-sh-arg *self*)
        runtime))

  (define `main
    (patsubst "%.min" "%" test-object))

  (rule ok-file test-object nil
        [ (echo-cmd "=> running " (basename test-object))
          (concat MAKE " -s -f " runner " "
                  "SCAM_MAIN='" main "' "
                  "SCAM_MODS='" mod-files "'")
          (concat "touch " ok-file) ]))


;; Generate a rule for constructing an executable.
;;
(define (link-rule exe deps oodeps objects runtime keep-syms)
  ;; sort objects to ensure agreement between .v2/scam and .v3/scam
  (rule exe deps oodeps
        [ (concat
           "@ true "
           (let ((mins (sort-by (lambda (f) (notdir f))
                                objects))
                 (main (modnames-of (first objects))))
             (lambda ()
               (print "=> linking " exe)
               (link exe mins main runtime keep-syms)))) ]))


;; Closure over mods and their requires.  This should give the set of modules
;; needed when the program executes, and modules that need to be bundled.
;;
(define (mmap-all-requires mmap mods)
  (declare *mmap*)
  (let-global ((*mmap* mmap))
    (traverse-graph mods
                    (lambda (mod) (mod-requires (assoc mod *mmap*)))
                    nil)))

;; This gives all modules required or used, directly or indirectly, by mods.
;;
(define (mmap-all-deps mmap mods)
  (declare *mmap*)
  (let-global ((*mmap* mmap))
    (traverse-graph mods
                    (lambda (mod) (mod-deps (assoc mod *mmap*)))
                    nil)))


;; Construct compile and test rules, given an MMAP.
;;
;; `runtime` is the runtime to be used to run tests.
;;
(define (mmap-rules mmap runtime)
  (define `rule-list
    (for
        mod mmap
        (begin
          (define `modname (mod-name mod))
          (define `source (mod-source mod))
          (define `object (mod-object mod))
          (define `testmod (mod-testmod mod))
          (define `requires (mod-requires mod))
          (define `uses (mod-uses mod))
          (define `is-boot (mod-is-boot mod))
          (define `deps (mod-deps mod))

          (define `depfiles (mmap-minfiles mmap deps))
          (define `compile-mods
            (mmap-minfiles mmap (mmap-all-deps mmap deps)))

          ;; compile this module after running tests on its depedencies (but
          ;; avoid circular when building "foo-q.min")
          (define `oodepfiles
            (filter-out (concat object ".ok")
                        (mmap-okfiles mmap deps)))

          ;; rule to compile the source file for this module
          (define `obj-rule
            (if source
                (if (filter "%.min" object)
                    (compile-rule object source depfiles oodepfiles
                                  compile-mods requires uses is-boot))))

          (define `test-obj (mmap-minfiles mmap testmod))

          ;; all object files used when running `mod`
          (define `(all-reqs mod)
            (mmap-minfiles mmap (mmap-all-requires mmap [mod])))

          ;; rule to test this module
          (define `ok-rule
            (if testmod
                (test-rule (addsuffix ".ok" test-obj)
                           test-obj
                           runtime
                           (all-reqs testmod))))

          (concat obj-rule ok-rule))))

  (concat-vec (filter-out [""] rule-list) nil))


;; Return a set of rules to build, test, and link an executable.
;;
;;  boot => use SOURCE files for rt-mod and ct-mod
;;  keep-syms => do not strip symbol information
;;  no-trace => do not include the `trace` module
;;
(define (exe-rules exe sources no-trace boot keep-syms)
  (define `(boot-file name)
    (subst "%" name (if boot "%.scm" "///%.min")))

  ;; Include the trace module unless no-trace is specified.
  (define `top-sources
    (append sources
            (if (not no-trace)
                (boot-file "trace"))
            ;; bundled ct-mod will be needed by the bootstrapped compiler
            (if boot
                (boot-file ct-mod))))

  ;; Allow rebundling except when we (the compiler) do not generate code
  ;; compatible with our bundled runtime (which should be the case only for
  ;; .out/a/scam).
  (define `rebundle
    (if (eq (local-to-global "apply") (global-name apply))
        1
        (if boot
            nil
            (error "Warning: must use --boot; bundles are unusable"))))

  ;; Scan dependencies for rt & ct, giving them no implicit deps.
  (define `env1 (append (hash-bind "odir" (dir exe))
                        (hash-bind "rebundle" rebundle)))
  ;; Boot modules do NOT implicitly depend on rt and ct.
  (define `env0 (hash-bind "boot" 1 env1))

  (define `mm0 (scan-modules env0 [(boot-file rt-mod)
                                   (boot-file ct-mod)]))
  (define `mm1 (scan-modules env1 top-sources mm0))

  (let ((mmap mm1)
        (top-mods (notdir (basename top-sources))))
    (define `rt-obj (mmap-objects mmap rt-mod))
    (define `mod-rules (mmap-rules mmap rt-obj))
    (define `deps (mmap-minfiles mmap top-mods))
    (define `oodeps (mmap-okfiles mmap top-mods))
    (define `objects (mmap-objects mmap (mmap-all-requires mmap top-mods)))

    (concat (link-rule exe deps oodeps objects rt-obj keep-syms)
            mod-rules)))


(define `(prefix-of sym name)
  (subst name "" (global-name sym)))


;; Compile a source file and its dependencies to MIN files, verifying each
;; MIN file that has an associated test program, and then (after all are
;; built) bundle them in a self-contained executable.
;;
(define (build exe files opts)
  (define `rules
    (concat (rule ".PHONY" nil ["/exe" "/dir"])
            (rule "/exe" nil ["/dir" exe])
            (rule "/dir" nil [] [(lambda () (concat "mkdir -p " (dir exe)))])
            (exe-rules exe files
                       (hash-get "no-trace" opts)
                       (hash-get "boot" opts)
                       (hash-get "symbols" opts))))

  (eval (dbg-print "B" "Eval: %s" rules)))
