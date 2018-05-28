;;----------------------------------------------------------------
;; scam.scm: implements `main` for the SCAM compiler/interpreter.
;;----------------------------------------------------------------

(require "core")
(require "repl")
(require "build")
(require "getopts")
(require "compile")
(require "gen")
(require "io")


(define usage-string
  "Usage:\n
    scam [-i]                  Enter interactive mode
    scam -o FILE [OPTS] FILE   Build an executable from SRC
    scam -e EXPR               Eval and print value of expression
    scam [-x] FILE ARGS...     Compile and execute FILE
    scam -v / --version        Show version
    scam -h                    Show this message

Options:

  --no-trace      Omit tracing functionality from the executable
  --quiet         Do not display progress messages
  --out-dir DIR   Specify directory for intermediate files
  --              Stop processing options
")

;; The following are options are subject to change:
;;
;;  --no-syms  Strip symbols information.  This will result in a smaller file
;;             that behaves the same, unless it makes compilation functions.
;;  --boot     Selects "bootstrap" mode, in which the run-time and compile-time
;;             implied dependencies are read from sources, not bundles.


(define (perror fmt ...values)
  ;; printn goes to stderr
  (printn "scam: " (vsprintf (concat fmt "\n") values))
  ;; this value can be returned from main to indicate error
  1)


(define `version "1.3.1")


(define (set-obj-dir out-dir out-file)
  (define `given-dir
      (or out-dir
          (if out-file
              (concat (dir out-file) ".scam/")
              ".scam/")))
  ;; ensure it ends with "/"
  (set *obj-dir* (patsubst "%//" "%/" (concat given-dir "/"))))


;; Construct & eval rules that will build an execute a program.  These rules
;; will be executed after `main` returns.
;;
(define (build-and-run file-and-args omap)
  (build (concat *obj-dir* (basename (notdir (first file-and-args))))
         (first file-and-args)
         (append { run: (rest file-and-args) }
                 omap)))


(define (main argv)
  (define `opt-names
    "-o= -e= -v --version -h -x -i --no-trace --quiet --out-dir= --no-syms --boot")

  (let ((omap (getopts argv opt-names)))
    (define `(opt name) (dict-get name omap))

    (define `names (opt "*"))      ; non-option arguments
    (define `errors (opt "!"))     ; errors encountered by getopts
    (set *is-quiet* (opt "quiet"))
    (set-obj-dir (last (opt "out-dir")) (last (opt "o")))

    (cond
     (errors
      (for e errors
           (case e
             ((MissingArg opt) (perror "'%s' is missing an argument" opt))
             ((BadOption arg) (perror "'%s' is not a recognized option" arg))
             (else (perror "[internal error]"))))
      (perror "try 'scam -h' for help"))

     ((opt "h")
      (print usage-string))

     ((opt "o")
      (if (word 2 names)
          (perror "to many input files were given with `-o`")
          (build (last (opt "o")) names omap)))

     ((opt "e")
      (for expr (opt "e")
           (repl-rep expr nil))
      nil)

     ((or (opt "v")
          (opt "version"))
      (print "SCAM version " version))

     ((opt "x")
      (if (not names)
          (perror "no FILE was given with '-x'")
          (build-and-run names omap)))

     ((not names) ;; handles valid `-i` case as well
      (print "SCAM v" version " interactive mode. Type '?' for help.")
      (repl))

     ((opt "i")
      (perror "extraneous arguments were provided with -i"))

     (else
      (build-and-run names omap)))))
