;;----------------------------------------------------------------
;; build.scm: generate make rules to compile SCAM programs
;;----------------------------------------------------------------

(require "core")
(require "io")
(require "parse")
(require "compile")
(require "gen")

;; Rule Processing
;; ----
;;
;; This module leverages Make's built-in rule processing by constructing and
;; eval'ing Make rules.  When SCAM's `main` function returns, Make proceeds
;; to process all rules, with the first-appearing rule serving as the
;; default target.  The "command" section of a Make rule is expanded just
;; before it is passed to the shell.  This allows us to write anonymous SCAM
;; functions into the command block that do all of the work required to
;; build the target.
;;
;; There are three types of rules that we generate:
;;
;; 1. compile: These rules compile a .scm file to a .min file, using the
;;    `compile-file` function.
;;
;; 2. test: These rules construct a SCAM executable and then run it,
;;    generating a .ok file on success.
;;
;; 3. link: These rules generate a "bundled" executable from one or more
;;    .min files, using the `link` function.
;;
;; Boot Phase
;; ----
;;
;; Building a compiler from sources involves a "boot" phase.  First, an
;; executable (call it version A) is built from some previous, "golden"
;; compiler.  Then, version A is used to compile version B, with the
;; "--boot" option.  The handling of implicit modules (e.g. the runtime
;; library) is different in these cases.
;;
;; * Version A was generated by the golden compiler as an ordinary SCAM
;;   program.  It therefore uses the golden compiler's implicit modules
;;   (if the golden compiler even has that notion), and does not have
;;   access to them, and would not be able to use them even if it did.
;;
;; * Version B is built by A.  Code generated by A needs implicit
;;   modules compatible with A, so those are compiled from source.
;;   Version A also makes the bundle modules accessible by version B
;;   (because both A and B are built from the same sources).
;;
;; When `--boot` is given on the command line:
;;
;;  * No "bundled" modules will be used.  All dependencies must be
;;    satisfied by source files, including implicit dependencies
;;    (`runtime` and `scam-ct`).
;;
;; When `--boot` is NOT specified:
;;
;;  * Ordinary dependencies (`require` or `use`) may be satisfied by
;;    SCAM sources *or* by modules that are bundled with the compiler.
;;  * Implicit dependencies will be satisfied by bundled modules.
;;
;;--------------------------------------------------------------

(define (fatal fmt ...args)
  (print (vsprintf (concat "scam: " fmt) args))
  (error "fatal error"))


;; *self* = the compiler itself
(define *self* (firstword MAKEFILE_LIST))


(define (build-eval str)
  (eval str))


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
  (if (filter ["#%" ""] (word 1 lines))
      (skip-comments (rest lines))
      lines))


;; Remove initial comment lines from a .min file.
;;
(define (strip-comments text)
  (concat-vec (skip-comments (split "\n" text)) "\n"))


;; Generic module manipulation

(define (module-object-file origin)
  (modid-file (module-id origin)))


(define (origins-to-objs origins)
  (for m origins
       (module-object-file m)))


;; For modules that are satsified by files, return the object file names.
(define (origins-to-obj-files origins)
  (origins-to-objs (filter-out "'%" origins)))


;; Read the compiled (binary) form of a module.
;;
(define (module-read-obj origin)
  (if (filter "'%" origin)
      (value (module-var origin))
      (or (read-file (modid-file (module-id origin)))
          ;; TODO: remove
          (printf "module-read-obj: file '%s' not found!" (modid-file (module-id origin))))))


;; Scan a source file for `require` and `use` dependencies.
;; Returns:  [REQUIRES USES]
;;
;; REQUIRES and USES are vectors of module names found in the source file
;; (the constant strings passed to `require` and `use`).
;;
(define (scan-source filename)
  (define `sedcmd
    "sed -E 's/ //g;s/^\\((require|use)\"([^\"]*)\".*|.*/\\1\\2/g;/../!d'")

  (let ((out (shell (concat sedcmd " " (quote-sh-arg filename)))))
     (for symbol ["require%" "use%"]
          (foreach f (filtersub symbol "%" out)
                   [f]))))


;; Scan a builtin module for `require` and `use` dependencies.
;;
(define (scan-builtin origin)
  (let ((lines (wordlist 1 4 (split "\n" (value (module-var origin))))))
    (define `(collect key)
      (promote (filtersub [(concat "# " key ": %")] "%" lines)))

    (assert lines)
    (foreach key "Requires Uses"
             [ (collect key) ])))


;; Return [REQUIRES USES], where both REQUIRES and USES are vectors of
;; module origins.
;;
(define (scan-deps origin)
  (if (module-is-source? origin)
      (for v (scan-source origin)
           (for name v
                (or (locate-module origin name)
                    (print "Could not find module: " name))))
      (scan-builtin origin)))


;; MMAP = [ MOD... ]
;;
;; MOD = [
;;    origin    : When compiled from source: source file name
;;                When builtin, (concat "'" MODNAME)
;;    testmod   : test module origin (if one exists)
;;    requires  : "sources" in `require` deps (explicit and implicit)
;;    uses      : "sources" in `use` deps (explicit and implicit)
;;    excludes  : see compile-file
;; ]

(define `(mod-origin mod)    (nth 1 mod))
(define `(mod-testmod mod)   (nth 2 mod))
(define `(mod-requires mod)  (nth 3 mod))
(define `(mod-uses mod)      (nth 4 mod))
(define `(mod-excludes mod)  (nth 5 mod))
(define `(mod-deps mod)      (append (mod-requires mod) (mod-uses mod)))

(define `(mod-obj mod)       (module-object-file (mod-origin mod)))

(define `(mod-new origin testmod requires uses excludes)
  [origin testmod requires uses excludes])


;; plural accessors (each takes a list of module names)

(define (mmap-testmods mmap sources)
  (filter-out [""]
              (for s sources
                   (mod-testmod (assoc s mmap)))))

(define (mmap-okfiles mmap sources)
  (addsuffix ".ok" (origins-to-objs (mmap-testmods mmap sources))))


;; Transitive closure over `mod-requires`.  This should give the set of
;; modules that are run-time dependencies.
;;
(define (mmap-all-requires mmap mods)
  (declare *mmap*)
  (let-global ((*mmap* mmap))
    (traverse-graph mods
                    (lambda (mod) (mod-requires (assoc mod *mmap*)))
                    nil)))



;; Visit a set of source files, scanning dependencies, and visiting them
;; recursively. Return an mmap describing all object files to be built.
;;
;; ORIGINS is a list of module src properties.
;;   If a source file: name = (basename FILE)
;;   If a bundled module:  name = (concat "'" MOD)
;;
;; MMAP is a vector of MOD entries (see above).
;;
(define (scan-modules env origins ?mmap)
  (define `origin (first origins))
  (define `others (rest origins))

  (cond
   ;; done?
   ((not origin) mmap)

   ;; Already visited?
   ((assoc origin mmap) (scan-modules env others mmap))

   (else
    (let ((env env)
          (others others)
          (mmap mmap)
          (origin origin)
          (deps (scan-deps origin)))

      (define `test-origin (and (filter-out "'%" origin)
                             (file-exists? (filtersub "%.scm" "%-q.scm" origin))))
      (define `requires (append (nth 1 deps) (dict-get "rt" env)))
      (define `uses (append (nth 2 deps) (dict-get "ct" env)))
      (define `excludes
        (concat (if (dict-get "rt" env) nil "R")
                (if (dict-get "ct" env) nil "C")))

      (scan-modules env
                    (append others (promote deps) test-origin)
                    (cons (mod-new origin test-origin requires uses excludes)
                          mmap))))))


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
  (define `prefix (if (findstring "@" (value "SCAM_DEBUG")) nil "@"))

  (concat target ": " deps (if oodeps " | ") oodeps
          (concat-vec (cons "" (addprefix prefix commands)) "\n\t")
          "\n\n"))


;; Generate a rule for compiling .scm to .min
;;
;; OBJECT = output file
;; SOURCE = SCAM source file
;; DEPS = other files directly and indirectly used in compilation
;; OODEPS = order-only deps (qualification tests for dependencies)
;; FILE-MODS = list of module locations that have been freshly compiled
;; REQS, USES, EXCLUDES = see compile.scm
;;
(define (compile-rule object source deps oodeps file-mods reqs uses excludes)
  (define `compile-lambda
    (lambda ()
      (compile-file source object file-mods reqs uses excludes)))

  (rule object (append source deps *self*) oodeps
        [ (concat ": " compile-lambda) ]))


;;----------------------------------------------------------------
;; Linking
;;----------------------------------------------------------------


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
(define prologue
"#!/bin/bash
:; for v in \"${@//!/!1}\" ; do v=${v// /!0} ; v=${v//	/!+}; a[++n]=${v:-!.} ; done ; LC_ALL=C SCAM_ARGS=${a[*]} exec make -Rr --no-print-directory -j ${SCAM_JOBS:-9} -f\"$0\"
SHELL:=/bin/bash
")

(define (epilogue main main-func rt)
  (concat "$(eval $(value " (module-var rt) "))\n"
          "$(call ^start," (module-id main) "," main-func ",$(value SCAM_ARGS))\n"))


;; Construct a bundled executable.  This function is typically executed when
;; rules are expanded during Make's rule processing phase.
;;
;; OUTFILE = file name
;; MODULES = vector of module origins
;; MAIN = main module origin
;; RUNTIME = runtime module origin
;; KEEP-SYMS = If not true, symbols willbe stripped.
;;
(define (link outfile modules main runtime keep-syms)
  (define `(bundle mod)
    (define `var (module-var mod))
    (define `text ((if keep-syms identity strip-comments)
                   (module-read-obj mod)))
    (concat "\ndefine " var "\n" text "\nendef\n"))

  (or *is-quiet*
      (print "=> linking " outfile))

  (write-file outfile
              (concat prologue
                      (concat-for mod modules " "
                               (bundle mod))
                      (epilogue main (gen-global-name "main" nil) runtime)))

  (shell (concat "chmod +x " (quote-sh-arg outfile))))


;; Generate a rule for constructing an executable.
;;
(define (link-rule outfile modules main deps oodeps runtime keep-syms)
  (define `link-lambda
    (let ((mods (sort-by (lambda (f) (notdir f))
                         modules)))
      (lambda ()
        (link outfile mods main runtime keep-syms))))

  ;; sort objects to ensure agreement between .v2/scam and .v3/scam
  (rule outfile deps oodeps
        [ (concat ": " link-lambda) ]))


;; Generate a rule that compiles and runs a test module.
;;
;; TESTMOD = origin of the test module
;; MODULES = origins of all modules required directly/indirectly
;; RUNTIME = origin of the runtime module to be used for the executable
;; MOD-FILES = list of compiled files to be used instead of bundled modules
;;
(define (test-rule testmod modules runtime)
  (declare MAKE &global)

  ;; test modules are always source modules, so module-object-file returns a file.
  (let ((obj-file (module-object-file testmod)))
    (define `exe (basename obj-file))
    (define `ok-file (concat obj-file ".ok"))

    (define `link-lambda
      (let ((mods (sort-by (lambda (f) (notdir f))
                           modules))
            (exe exe))
        (lambda ()
          (link exe mods testmod runtime nil))))

    (rule ok-file obj-file nil
          [ (concat (if (not *is-quiet*)
                        (concat "echo '=> running '" (subst "$" "$$" (quote-sh-arg exe))))
                    link-lambda)
            (concat MAKE " -s -f " exe)
            (concat "touch " ok-file) ])))


;; This gives all modules required or used, directly or indirectly, by mods.
;;
(define (mmap-all-deps mmap mods)
  (declare *mmap*)
  (let-global ((*mmap* mmap))
    (traverse-graph mods
                    (lambda (mod) (mod-deps (assoc mod *mmap*)))
                    nil)))


(define (mmap-rule mmap mod runtime)
  (define `origin (mod-origin mod))
  (define `object (mod-obj mod))
  (define `testmod (mod-testmod mod))
  (define `deps (mod-deps mod))

  (define `depfiles (origins-to-obj-files deps))
  (define `file-mods (mmap-all-deps mmap deps))

  ;; compile this module after running tests on its dependencies (but
  ;; avoid circular when building "foo-q.min")
  (define `oodepfiles
    (filter-out (concat object ".ok")
                (mmap-okfiles mmap deps)))

  ;; rule to compile the source file for this module
  (define `obj-rule
    (if (module-is-source? origin)
        (compile-rule object origin depfiles oodepfiles
                      file-mods (mod-requires mod) (mod-uses mod)
                      (mod-excludes mod))))

  ;; rule to test this module
  (define `ok-rule
    (if testmod
        (test-rule testmod
                   (mmap-all-requires mmap [testmod])
                   runtime)))

  (concat obj-rule ok-rule))


;; Construct compile and test rules, given an MMAP.
;;
;; RUNTIME = runtime origin
;;
(define (mmap-rules mmap runtime)
  (concat-for mod mmap ""
              (mmap-rule mmap mod runtime)))


;; Return a set of rules to build, test, and link an executable.
;;
;;  keep-syms => do not strip symbol information
;;  no-trace => do not include the `trace` module
;;
(define (exe-rules exe sources no-trace keep-syms)
  ;; Use sources when booting.  Otherwise, use builtins.
  (define `(implicit mod)
    (if *is-boot*
        (concat mod ".scm")
        (concat "'" mod)))

  (define `rt-origin (implicit "runtime"))
  (define `ct-origin (implicit "scam-ct"))

  ;; Validate provided source file names.  Include trace module.
  (define `top-origins
    (append (for s sources
                 (or (file-exists? s)
                     (fatal "source file %s does not exist" s)))
            (if (not no-trace)
                (implicit "trace"))))

  ;; If the compiler has been compiled as user code, enforce `--boot` to
  ;; prevent re-bundling of the compiler's modules in target programs.
  (and (not *is-boot*)
       (eq? "mmap-rules" (global-name mmap-rules))
       (fatal "compiler not compatible with its output; use --boot"))

  (define `mm-rt (scan-modules {} [rt-origin]))
  (define `mm-ct (scan-modules {rt: rt-origin} [ct-origin] mm-rt))
  (define `mm1 (scan-modules {rt: rt-origin, ct: ct-origin} top-origins mm-ct))

  (let ((mmap mm1)
        (top-origins top-origins)
        (main-origin (first sources)))
    (define `mod-rules (mmap-rules mmap rt-origin))
    (define `deps (origins-to-obj-files top-origins))
    (define `oodeps (mmap-okfiles mmap top-origins))
    ;; Include all run-time dependencies, but not necessarily the
    ;; compile-time dependencies.
    (define `modules (mmap-all-requires mmap top-origins))

    (concat (link-rule exe modules main-origin deps oodeps rt-origin keep-syms)
            mod-rules)))


;; Construct & eval rules to compile a program.  The rules will be executed
;; after `main` returns.
;;
;; If OPTS conains a `run` field, then run the program also.
;;
;; The first source file is treated as the "main" module; others will be
;; built and bundled in the executable.  Other source files used by the
;; program, directly or indirectly, are also built.  Any "-q.scm" validation
;; programs are also built and executed.
;;
(define (build exe files opts)
  &public

  ;; Phony target names; avoid conflict with actual executable or intermediate files.
  (define `ALL "ALL")
  (define `DIR "DIR")

  (set *is-boot* (dict-get "boot" opts))

  ;; When running the program, do so directly with Make.  Running as a bash
  ;; script can generate "make[1]: warning: -jN forced in submake".
  (define `run-command
    (concat "SCAM_ARGS=" (quote-sh-arg (dict-get "run" opts))
              " $(MAKE) -f " (quote-sh-arg exe)))

  (define `rules
    (concat (rule ".PHONY" [ALL DIR] nil nil)
            (rule ALL [DIR exe] nil (if (dict-find "run" opts) [run-command]))
            (rule DIR [] nil [(lambda () (concat "mkdir -p " (dir exe)))])
            (exe-rules exe files
                       (dict-get "no-trace" opts)
                       (not (dict-get "no-syms" opts)))))

  ;; Set SCAM_DEBUG=B to see rules.
  (build-eval rules))
