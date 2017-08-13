;;----------------------------------------------------------------
;; scam.scm: implements `main` for the SCAM compiler/interpreter.
;;----------------------------------------------------------------

(require "core")
(require "repl")
(require "build")
(require "getopts")
(require "gen")

;; Supported command-line options are documented in `usage` function, below.
;; The following private options are used only when compiling the compiler:
;;
;;  --symbols : Retain symbol information when building an executable.  This
;;              is used when building the interpreter/compiler.
;;  --boot    : Selects "bootstrap" mode, in which the run-time and compile-time
;;              implied dependencies are read from sources, not bundles.
;;  --rt FILE : specifies a source file to be used as the runtime for the
;;              generated executable.
;;  --ct FILE : specifies a source file that defines compile-time macros.

(define (usage ?fmt ...values)
  (if fmt
      (print "scam: " (vsprintf fmt values)))
  (print "Usage:\n
    scam [-i]              : enter interactive mode
    scam -o EXE FILE...    : build an executable from SRC
    scam -e EXPR           : eval and print value of expression
    scam -r MAK            : load and execute executable file MAK
    scam [-x] FILE ARG...  : compile and execute FILE
    scam -v                : show version

Options:

  --no-trace : Omit tracing functionality.  This will produce a slightly
               smaller executable.
")
  (if fmt 1))


(define `version
  "1.1")


(define (opt-err opt)
  (usage "Unrecognized command option '%s'" opt))


(define (main argv)
  (define `opt-names
    "-e= -h -i -r= -o= --symbols --boot --no-trace -x=... -v")

  (let ((o (getopts argv opt-names opt-err)))
    (define `files (nth 1 o))
    (define `opts (nth 2 o))
    (define `(opt name) (dict-get name opts))

    (define `(exec argv)
      (define `user-main (gen-global-name "main" nil))
      (if (eq? user-main (global-name main))
          (begin
            (print "scam: -x not supported; namespace collision")
            1)
          (begin
            (repl-file (first argv))
            (run-hooks "load")
            (call user-main (rest argv)))))

    (cond
     ((opt "v")
      (print "SCAM version " version))

     ((opt "o")
      (build (opt "o") files opts))

     ((opt "h")
      (usage))

     ((opt "e")
      (repl-rep (opt "e") nil))

     ((opt "r")
      (eval (concat "include " (opt "r"))))

     ((or (opt "x") files)
      (exec (or (opt "x") files)))

     (else       (repl)))))
