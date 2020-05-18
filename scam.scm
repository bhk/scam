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
    scam FILE ARGS...          Compile and execute FILE
    scam -o FILE [OPTS] FILE   Build an executable from SRC
    scam -e EXPR               Print the value of expression EXPR
    scam -v / --version        Show version
    scam -h / --help           Show this message

Options:

  --quiet           Do not display progress messages
  --build-dir DIR   Specify directory for intermediate files
  --                Stop processing options
")

;; The following are options are subject to change:
;;
;;  --boot     Selects "bootstrap" mode, in which the run-time and compile-time
;;             implied dependencies are read from sources, not bundles.


(define (perror fmt ...values)
  (fprintf 2 (.. "scam: " fmt "\n") values)
  ;; this value can be returned from main to indicate error
  1)


(define `version "2.0.3")


(define (main argv)
  (define `opt-names
    "-o= -e= -v --version -h --help -i --quiet --build-dir= --boot")

  (let ((omap (getopts argv opt-names)))
    (define `(opt name)
      (dict-get name omap))

    (define `names (opt "*"))
    (define `errors (opt "!"))
    (define `is-quiet (opt "quiet"))

    ;; These globals govern compilation
    (set *is-boot* (opt "boot"))

    (define build-dir
      (or (last (opt "build-dir"))
          (addsuffix ".scam/" (dir (last (opt "o"))))
          (native-var "SCAM_BUILD_DIR")
          (.. (native-var "HOME") "/.scam/")))

    (or

     (when errors
       (for (e errors)
         (case e
           ((MissingArg opt) (perror "`%s` is missing an argument" opt))
           ((BadOption arg) (perror "`%s` is not a recognized option" arg))
           (else (perror "[internal error]"))))
       (perror "try `scam -h` for help"))

     (vec-or
      (for (expr (opt "e"))
        (if (repl-ep expr build-dir is-quiet)
            1)))

     (when (or (opt "h")
               (opt "help"))
       (print usage-string)
       0)

     (when (opt "o")
       (if (word 2 names)
           (perror "too many input files were given with `-o`")
           (or (build-program (first names) (last (opt "o")) build-dir is-quiet)
               0)))

     (when (or (opt "v")
               (opt "version"))
       (print "SCAM version " version)
       0)

     (when names
       (or (run-program (first names) (rest names) build-dir is-quiet)
           0))

     (when (or (opt "i")
               (not (or names (opt "e"))))
      (print "SCAM v" version " interactive mode. Type `?` for help.")
      (repl build-dir)
      0))))
