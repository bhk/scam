;; scam.scm

(require "core")
(require "repl")
(require "build")
(require "getopts")
(if nil
    (require "trace"))  ;; include just for bundling purposes


(define (usage ...)
  (if *args*
      (print "scam: " (vsprintf *args*)))
  (print "Usage:\n
    scam [-i]              : enter interactive mode
    scam -o EXE FILE...    : build an executable from SRC
    scam -e EXPR           : eval and print value of expression
    scam [-x] FILE         : compile and execute FILE

Options:

  --symbols : when building an executable, retain symbol information.  This
              is useful when building an interpreter or compiler.
")
  (if ... 1))


(define (opt-err opt)
  (usage "Unrecognized command option '%s'" opt))


(define (main argv)
  (let ((o (getopts argv "-e= -h -i -o= -x= --symbols" opt-err)))
    (define `files (nth 1 o))
    (define `opts (nth 2 o))
    (define `(opt name) (hash-get name opts))
    (define `runfile (or (opt "-x") (first files)))
    (define `(exec file argv)
      (set main "")
      (repl-file file)
      (main argv))

    (cond
     ((opt "-o")  (build (opt "-o") files (opt "--symbols")))

     ((opt "-h")  (usage))

     ((opt "-e")  (repl-rep (opt "-e")))

     ((opt "-x")  (lambda () (exec (opt "-x") files)))

     (files       (exec (first files) (rest files)))

     (else        (repl)))))
