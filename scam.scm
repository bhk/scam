;;----------------------------------------------------------------
;; scam.scm: implements `main` for the SCAM compiler/interpreter.
;;----------------------------------------------------------------

(require "core")
(require "repl")
(require "build")
(require "getopts")
(require "compile")
(require "gen")

;; Supported command-line options are documented in `usage` function, below.
;; The following private options are used only when compiling the compiler:
;;
;;  --symbols : Retain symbol information when building an executable.  This
;;              is used when building the interpreter/compiler.
;;  --boot    : Selects "bootstrap" mode, in which the run-time and compile-time
;;              implied dependencies are read from sources, not bundles.

(define (usage ?fmt ...values)
  (if fmt
      (print "scam: " (vsprintf fmt values)))
  (print "Usage:\n
    scam [-i]                 : enter interactive mode
    scam -o FILE [OPTS] FILE  : build an executable from SRC
    scam -e EXPR              : eval and print value of expression
    scam -x FILE ARGS...      : compile and execute FILE, passing ARGS
    scam FILE                 : compile and execute FILE
    scam -v                   : show version
    scam -h                   : show this message

Options:

  --no-trace    : Omit tracing functionality from the executable.
  --quiet       : Do not display progress messages.
  --out-dir DIR : Specify directory for intermediate files.

")
  (if fmt 1))


(define `version "1.3")

(define `default-out-dir ".scam/")

(define (opt-err opt)
  (usage "Unrecognized command option '%s'" opt))


;; Construct & eval rules that will build an execute a program.  These rules
;; will be executed after `main` returns.
;;
(define (build-and-run file-and-args)
  (define `file (first file-and-args))
  (build (concat *obj-dir* (basename (notdir file)))
         (word 1 file-and-args)
         { run: (rest file-and-args) }))


(define (main argv)
  (define `opt-names
    "-o= -e= -v -h -x=... -i --no-trace --quiet --out-dir= --symbols --boot")

  (let ((o (getopts argv opt-names opt-err)))
    (define `files (nth 1 o))
    (define `opts (nth 2 o))
    (define `(opt name) (dict-get name opts))

    (define `out-dir (or (opt "out-dir")
                         (if (opt "o")
                             (concat (dir (opt "o")) ".scam/")
                             default-out-dir)))
    (set *obj-dir* (subst "//" "/" (concat out-dir "/")))
    (set *is-quiet* (opt "quiet"))

    (cond
     ((opt "o")
      (if (word 2 files)
          (error "Yow"))
      (build (opt "o") files opts))

     ((opt "e")
      (repl-rep (opt "e") nil))

     ((opt "v")
      (print "SCAM version " version))

     ((opt "h")
      (usage))

     ((opt "x")
      (build-and-run (opt "x")))

     ((not files) ;; handles valid `-i` case
      (repl))

     ((opt "i")
      (usage "scam: extra arguments with -i"))

     (else
      (build-and-run files)))))
