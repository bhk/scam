;;----------------------------------------------------------------
;; scam.scm: implements `main` for the SCAM compiler/interpreter.
;;----------------------------------------------------------------

(require "core.scm")
(require "repl.scm")
(require "getopts.scm")
(require "compile.scm")
(require "gen.scm")
(require "io.scm")


(define usage-string
  "Usage:\n
    scam [-i]                  Enter interactive mode
    scam -o FILE [OPTS] FILE   Build an executable from SRC
    scam -e EXPR               Eval and print value of expression
    scam [-x] FILE ARGS...     Compile and execute FILE
    scam -v / --version        Show version
    scam -h                    Show this message

Options:

  --quiet         Do not display progress messages
  --obj-dir DIR   Specify directory for intermediate files
  --              Stop processing options
")

;; The following are options are subject to change:
;;
;;  --boot     Selects "bootstrap" mode, in which the run-time and compile-time
;;             implied dependencies are read from sources, not bundles.


(define (perror fmt ...values)
  (fprintf 2 (concat "scam: " fmt "\n") values)
  ;; this value can be returned from main to indicate error
  1)


(define `version "1.5x")


(define (main argv)
  (define `opt-names
    "-o= -e= -v --version -h -x -i --quiet --obj-dir= --boot")

  (let ((omap (getopts argv opt-names)))
    (define `(opt name)
      (dict-get name omap))

    (define `names (opt "*"))      ; non-option arguments
    (define `errors (opt "!"))     ; errors encountered by getopts

    ;; These globals govern compilation
    (set *is-boot* (opt "boot"))
    (define is-quiet (opt "quiet"))
    (define obj-dir
      (or (last (opt "obj-dir"))
          (if (opt "o")
              (concat (dir (last (opt "o"))) ".scam/"))))

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
          (perror "too many input files were given with `-o`")
          (build-program (first names) (last (opt "o")) obj-dir is-quiet)))

     ((opt "e")
      ;; eval with the REPL's initial env & output formatting
      (for expr (opt "e")
           (if (repl-ep expr obj-dir is-quiet)
               (error "Error")))
      nil)

     ((or (opt "v")
          (opt "version"))
      (print "SCAM version " version))

     ((opt "x")
      (if (not names)
          (perror "no FILE was given with '-x'")
          (run-program (first names) (rest names) obj-dir is-quiet)))

     ((not names) ;; handles valid `-i` case as well
      (print "SCAM v" version " interactive mode. Type '?' for help.")
      (repl obj-dir))

     ((opt "i")
      (perror "extraneous arguments were provided with -i"))

     (else
      (run-program (first names) (rest names) obj-dir is-quiet)))))
