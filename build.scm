;; build.scm: build .min files and standalone eecutables
;;
;; We leverage Make's built-in rule processing by constructing and eval'ing
;; Make rules:
;;
;;   target: prerequisites...
;;           command
;;
;; When our `main` function returns, Make proceeds to process all rules,
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
;;--------------------------------------------------------------

(require "core")
(require "io")
(require "parse")
(require "compile")


;; This preamble makes the resulting file both a valid shell script and a
;; valid makefile.  When invoked by the shell, the script invokes `make` to
;; process the script as a makefile.
;;
(define `bootstrap
"#!/bin/bash
:; for v in \"${@//!/!1}\" ; do v=${v// /!0} ; v=${v//	/!+}; a[++n]=${v:-!.} ; done ; SCAM_ARGS=${a[*]} exec make --no-print-directory -f\"$0\"

")


;; remove initial comment lines from a .min file
;;
(define (strip-comments src)
  ;; split string at "\n" but not "\n#"
  (concat-vec (rest (subst " #" "#" (split "\n" (concat "#\n" src)))) "\n"))


(define `bundle-dir "///")

;; Return a fake filename that represents a bundled object file (not to be
;; confused with an actual filename).
;;
(define `(bundle-path source)
  (concat bundle-dir (notdir (basename source)) ".min"))

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
(define `(read-object file)
  (if (bundled? file)
      (value (bundle-var file))
      (read-file file)))


;; Construct a bundled executable.  This function is typically executed when
;; rules are expanded during Make's rule processing phase.
;;
(define (link outfile objects main keep-syms)
  (define `(bundle object)
    (define `var (bundle-var object))
    (define `text ((if keep-syms identity strip-comments)
                   (read-object object)))
    (concat "\ndefine " var "\n" text "\nendef\n"))

  (write-file outfile
              (concat bootstrap
                      (foreach obj objects
                               (bundle (promote obj)))
                      "\nSCAM_MAIN = " (bundle-var main) "\n"
                      "$(eval $(value " (bundle-var "runtime.min") "))\n"))

  (shell (concat "chmod +x " (quote-sh-arg outfile))))


;; *self* = the compiler itself
(define *self* (firstword MAKEFILE_LIST))


(define *quiet*
  (if (bound? "_@")
      (value "_@")
      "@"))

(define (echo-cmd ...)
  (subst "$" "$$"
         (concat "echo " (quote-sh-arg (concat-vec *args*)))))


;; Return a vector of source files require'd by `src`.
;;
(define (requires-of src)
  (define `sedcmd "sed -e '/^ *(require *\"..*\"/!d;s/!/!1/g;s/ /!0/g'")

  (patsubst "./%" "%"
            (for r (shell (concat sedcmd " " src))
                 (concat (dir src) (nth 2 (split "\"" r)) ".scm"))))


;; The source file that tests `src` (or nil if there is no test)
;;
(define `(test-of src)
  (wildcard (concat (patsubst "%.scm" "%" src) "-q.scm")))


;; The .min file to be generated from `src`
;;
(define (object-of src odir)
  (concat odir (basename (notdir src)) ".min"))


;; Construct a Make "rule"
;;
;;   target = file name
;;   prereqs = list of prerequisites (space-delimited)
;;   commands = vector of commands (function values).  These are evaluated
;;              when the target is built, and the return value is passed
;;              to the shell.
;;
(define (rule target deps oodeps commands)
  (concat target ": " deps (if oodeps " | ") oodeps
          (concat-vec (cons "" commands) (concat "\n\t" *quiet*))
          "\n\n"))


;; Generate a rule for compiling .scm to .min
;;
(define (compile-rule object source deps oodeps)
  (rule object (append source deps *self*) oodeps
        [ (echo-cmd "compiling " object)
          (let ((source source)
                (object object))
            (lambda () (concat "true " (compile-file source object)))) ]))


;; Generate a rule for testing a .min
;;
;;  runner-obj = scam program to use to run the executable
;;  mod-files = list of compiled files to be used instead of bundled modules
;;
(define (test-rule ok-file test-object runner-obj mod-files)
  (rule ok-file test-object nil
        [ (echo-cmd "running " (basename test-object) "...")
          (let ((self (quote-sh-arg *self*))
                (main (quote-sh-arg (patsubst "%.min" "%" test-object)))
                (runner (if (bundled? runner-obj)
                            *self*
                            runner-obj)))
            (lambda ()
              (declare MAKE)
              (concat MAKE " -s -f " runner
                      ; SCAM_MAIN specifies which module to run
                      " SCAM_MAIN=" main
                      ;; SCAM_MODS lists files to use instead of bundled modules
                      " SCAM_MODS='" mod-files "'")))
          (concat "touch " ok-file) ]))


;; Generate a rule for constructing an executable.
;;
(define (link-rule exe deps oodeps objects keep-syms)
  ;; sort objects to ensure agreement between .v2/scam and .v3/scam
  (rule exe deps oodeps
        [ (echo-cmd "bundling " exe)
          (let ((mins (sort-by (lambda (f) (notdir f))
                               objects))
                (main (basename (notdir (first objects)))))
            (lambda () (link exe mins main keep-syms))) ]))


;; `mods` data structure is a hash that maps source file names to
;; the things we need to know about them:
;;
;;   object: the source's object (.min) file
;;   okfile: the .ok file that is generated when `object` has been tested,
;;           or nil if there is no test.
;;   requires: source files required by `source`
;;

(define `(mods-add mods source object ok-file requires)
  (hash-bind source [object ok-file requires] mods))

(define `(mods-object mods file)   (nth 1 (hash-get file mods)))
(define `(mods-okfile mods file)   (nth 2 (hash-get file mods)))
(define `(mods-requires mods file) (nth 3 (hash-get file mods)))


;; Generate compile and test rules for a set of sources.
;;
;; On entry:
;;    files = source file names
;;    odir = output directory
;;    mods = described above; has one entry for each filename already visited.
;;
;; Returns:  [mods rules]
;;
(define (compile-rules sources odir mods)
  (define `file (first sources))

  (cond
   ;; done?
   ((not file)
    [mods])

   ;; If this source file is visited already, there is nothing to do.
   ((hash-find file mods)
    (compile-rules (rest sources) odir mods))

   ;; If the source file does not exist, emit no compile rule (assuming the
   ;; object file will be satisfied by a bundled module).
   ((not (file-exists? file))
    (compile-rules (rest sources)
                   odir
                   (mods-add mods file (bundle-path file) nil nil)))

   ;; Compile from source.
   (else
    (let ((required-files (append (requires-of file)
                                  (filter-out file "runtime.scm")))
          (object (object-of file odir))
          (sources sources)
          (mods mods))

      ;; Traverse dependencies.

      (define `test-source (test-of file))
      (define `test-object (object-of test-source odir))
      (define `ok-file (if test-source (addsuffix ".ok" test-object)))
      (define `use-dep (if test-source ok-file object))

      (let ((o (compile-rules (append required-files test-source (rest sources))
                              odir
                              (mods-add mods file object ok-file required-files))))

        ;; Construct rules.

        (define `mods (nth 1 o))
        (define `rules (nth 2 o))
        (define `obj-deps (foreach obj
                                   (foreach f required-files (mods-object mods f))
                                   (if (not (bundled? obj))
                                       obj)))
        (define `obj-oodeps (filter-out (concat object ".ok")
                                        (foreach f required-files
                                                 (mods-okfile mods f))))
        (define `use-tests (strip (foreach r required-files (mods-okfile mods r))))

        (define `runner (mods-object mods "runtime.scm"))

        ;; all object files used by the test program
        (define `mod-files (uniq (foreach w mods
                                          (nth 1 (hash-value w)))))

        (define `rules
          (concat (compile-rule object file obj-deps obj-oodeps)
                  (if test-source (test-rule ok-file test-object runner mod-files))
                  rules))

        [ mods rules ])))))


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


;; Return a set of rules to build, test, and link an executable.
;;
(define (exe-rules exe sources keep-syms)
  ;; Force use of the bundled runtime.min unless `runtime.scm` is mentioned
  ;; explicitly.
  (define `mods
    (if (not (filter "runtime.scm" sources))
        (mods-add nil "runtime.scm" (bundle-path "runtime.scm") nil nil)))

  (let ((o (compile-rules sources (dir exe) mods)))
    (let& ((mods (nth 1 o))
           (rules (nth 2 o))
           (deps (foreach f sources (mods-object mods f)))
           (oodeps (foreach f sources (mods-okfile mods f)))
           (required-sources (traverse-graph sources (lambda (source)
                                                       (mods-requires mods source))))
           (required-objects (for src required-sources
                                  (mods-object mods src)))
           (rule (link-rule exe deps oodeps required-objects keep-syms)))

      (concat rule rules))))


;; Compile a source file and its dependencies to MIN files, verifying each
;; MIN file that has an associated test program, and then (after all are
;; built) bundle them in a self-contained executable.
;;
(define (build exe sources keep-syms)
  (define rules
    (concat (rule ".PHONY" nil ["/exe" "/dir"])
            (rule "/exe" nil ["/dir" exe])
            (rule "/dir" nil [] [(lambda () (concat "mkdir -p " (dir exe)))])
            (exe-rules exe sources keep-syms)))

  (if (filter "B%" SCAM_DEBUG)
      (print rules))

  (eval rules))
