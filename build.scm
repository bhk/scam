;; build.scm: build .min files and standalone eecutables

(require "core")
(require "io")
(require "parse")
(require "compile")

;;--------------------------------------------------------------
;; Linking: bundling several .min files into a self-contained exe
;;--------------------------------------------------------------

(define `bootstrap
"#!/bin/bash
:; for v in \"${@//!/!1}\" ; do v=${v// /!0} ; v=${v//	/!+}; a[++n]=${v:-!.} ; done ; ARGS=${a[*]} exec make -f\"$0\"

")


;; remove initial comment lines from a .min file
;;
(define `(strip-comments src)
  ;; split string at "\n" but not "\n#"
  (concat-vec (rest (subst " #" "#" (split "\n" (concat "#\n" src)))) "\n"))


(define (bundle-text varname text)
  (concat "\ndefine " varname "\n" text "\nendef\n"))


(define `(bundle-file minfile)
  (define `text (read-file minfile))
  (define `mod (notdir (basename minfile)))
  (bundle-text (concat "///" mod ".min") text))


;; Read the contents of a MIN file.
;;
(define `(read-module name)
  (declare (^mod-find))
  (let ((modpath (^mod-find name)))
    (if (filter "///%" modpath)
        (value modpath)
        (read-file modpath))))


;; A note on runtimes:
;;
;; $(^runtime.min) = the runtime bundled for the executable itself (the one
;;      that runs before any other modules execute).
;;
;; $(///runtime.min) = a module named "runtime" loaded as a dependency.
;;       This is present only when we build the compiler.  It does not
;;       execute when the compiler runs, but the compiler (this module
;;       actually) reads it with 'read-module' to bundle it with programs
;;       that it compiles.
;;
;;       There is a special case in which ///runtime.min is executed first:
;;       this is when the scam executable is asked to invoke a .min file
;;       that it has compiled (prior to generating a standalone executable).
;;       This happens when SCAM_XRT is non-nil.
;;
(define (link outfile minfiles main)
  (define `main (basename (notdir main)))
  (define `runtime (read-module "runtime"))
  (define `notused
    (require "runtime"))  ;; mark this as a dependency

  (write-file outfile
              (concat bootstrap
                      (foreach _f minfiles
                               (bundle-file (promote _f)))
                      (bundle-text "^runtime.min"
                                   (strip-comments runtime))
                      "\nSCAM_MAIN = ///" main "\n"
                      "$(eval $(value $(if $(SCAM_XRT),///,^)runtime.min))\n"))

  (shell (concat "chmod +x " (quote-sh-arg outfile))))


;;--------------------------------------------------------------
;; Building: managing interdependencies
;;
;; We leverage Make's built in rule processing by constructing and
;; eval'ing Make rules.
;;
;; Note that the "command" section of a Make rule is expanded just before it
;; is passed to the shell ... so the content of a command is essentialy a
;; SCAM function value that evaluates to a legal shell command. This allows
;; us to perform build operations within make, without constructing a shell
;; command that does everything.
;;
;;--------------------------------------------------------------

(define *quiet*
  (if (not (bound? "SCAM_VERBOSE"))
      "@"))


;; *self* = the compiler itself
(define *self* (firstword MAKEFILE_LIST))


(define (echo-cmd ...)
  (subst "$" "$$"
         (concat "echo " (quote-sh-arg (concat-vec *args*)))))


;; Construct a Make "rule"
;;
;;   target = file name
;;   prereqs = list of prerequisites (space-delimited)
;;   commands = vector of commands (function values).  These are evaluated
;;              when the target is built, and the return value is passed
;;              to the shell.
;;
(define (rule target prereqs commands)
  (concat target ": " prereqs
          (concat-vec (cons "" commands) (concat "\n\t" *quiet*))
          "\n\n"))


;; Return a vector of source files require'd by `src`.
;;
(define (src-requires src)
  (define `sedcmd "sed -e '/^ *(require *\"..*\"/!d;s/!/!1/g;s/ /!0/g'")

  (patsubst "./%" "%"
            (for r (shell (concat sedcmd " " src))
                 (concat (dir src) (nth 2 (split "\"" r)) ".scm"))))


;; The source file that tests `src` (or nil if there is no test)
;;
(define `(src-qsrc src)
  (wildcard (concat (patsubst "%.scm" "%" src) "-q.scm")))

;; The .min file to be generated from `src`
;;
(define (src-min src odir)
  (concat odir (patsubst "%.scm" "%.min" src)))

;; The preqreq file for `src` (as described in outputs)
;;
(define `(src-prereq src outputs)
  (first (get src outputs)))

;; All .min files that need to be compiled to run `src` (as dscribed in outputs)
;;
(define `(src-mindeps src outputs) 
  (nth 2 (get src outputs)))


(declare (build-min src odir outputs rules))

;; Build all files in sources
;;
(define (build-mins sources odir o)
  (if sources
      (build-mins (rest sources) odir
                  (build-min (first sources) odir (nth 1 o) (nth 2 o)))
      o))
      

;; Generate rules to build and test a SCAM source file and its dependencies,
;; including a qualification step (building and running a test module if
;; present).  Identify all .min files that are run-time dependencies, and
;; identify the build pre-requisite (either a -q.min.ok file, if there is a
;; test, or the .min file otherwise).
;;
;; On entry:
;;   src = source file
;;   odir = output directory
;;   outputs = a map :: src -> [prereq mindeps]
;;   rules = rules to be eval'ed
;; 
;; Return [outputs rules]
;;
(define (build-min src odir outputs rules)
  (define `min (src-min src odir))

  ;; First, mark src as "visited" to curtail src->qsrc->src... infinite
  ;; recursion.  The -q.min should list src.min as a dep (not src-q.min.ok)
  (define `o [ (bind src [min min] outputs) rules ])

  (cond
   ;; already built?
   ((get src outputs)   [outputs rules])

   ;; not a source file?
   ((filter-out "%.scm" src)   [ (bind src [src src] outputs) rules ] )

   ;; build dependencies (and qsrc)
   (else
    (let ((reqs (src-requires src))
          (qsrc (src-qsrc src))
          (o o)
          (min min))
      
      (let ((o (build-mins (append qsrc reqs) odir o)))
        
        (define `outputs (nth 1 o))
        (define `rules (nth 2 o))
        
        ;; .min rule for this source file
        
        (define `min-rule
          (rule min
                (append src
                        (foreach r reqs (src-prereq r outputs))
                        *self*)
                [ (echo-cmd "compiling " min)
                  (let ((src src) (min min))
                    (lambda () (concat "true" (compile-file src min)))) ]))
        
        ;; .ok rule for this source file: X-q.min -> X-q.min.ok
        
        (define `qmin (src-min qsrc odir))
        (define `okfile (concat qmin ".ok"))
        (define `ok-rule
          (rule okfile
                [ qmin ]
                [ (echo-cmd "running " (basename qsrc) "...")
                  (let ((self (quote-sh-arg *self*))
                        (main (quote-sh-arg (patsubst "%.min" "%" qmin))))
                    (lambda () 
                      (concat "make -f " self " SCAM_XRT=1 SCAM_MAIN=" main)))
                  (concat "touch " okfile) ]))


        (define `mindeps (append min 
                                 (uniq (foreach r reqs (src-mindeps r outputs)))))
        (define `prereq (if qsrc okfile min))
        (define `outputs (bind src [prereq mindeps] outputs))
        
        (define `rules (concat min-rule
                               (if qsrc ok-rule)
                               rules))
        
        [outputs rules])))))



;; Generate rules to build a bundled SCAM executable from a set of sources
;; or compiled makefiles.  For sources, follow dependencies on other sources
;; and generate build rules recursively.
;;
(define (build-exe exe sources)
  (let ((o (build-mins sources (dir exe) nil))
        (exe exe)
        (sources sources))

    (define `outputs (nth 1 o))
    (define `rules (nth 2 o))

    (define `r 
      (rule exe
            (foreach src sources (src-prereq src outputs))
            [ (echo-cmd "bundling " exe)
              (let ((mins ( uniq (foreach src sources (src-mindeps src outputs))))
                    (main (basename (notdir (first sources)))))
                (lambda () (link exe mins main))) ]))

    (concat r rules)))


;; Compile a source file and its dependencies to MIN files, verifying each
;; MIN file that has an associated test program, and then (after all are
;; built) bundle them in a self-contained executable.
;; 
(define (build exe sources)
  (declare (^eval))
  (^eval
   (concat (rule ".PHONY" ["/exe" "/dir"])
           (rule "/exe" ["/dir" exe])
           (rule "/dir" [] [(lambda () (concat "mkdir -p " (dir exe)))])
           (build-exe exe sources))))
