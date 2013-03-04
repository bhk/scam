;; scam.scm

(require "core")
(require "repl")
(require "build")


(define (usage ...)
  (if *args*
      (print "scam: " (vsprintf *args*)))
  (print "Usage:\n
    scam [-i]              : enter interactive mode
    scam -o EXE FILE...    : build an executable from SRC
    scam -e EXPR           : eval and print value of expression
    scam [-x] FILE         : compile and execute FILE
")
  (if ... 1))



(define (main argv)
  (define `(opt? pat)
    (filter (concat "-" pat) (word 1 argv)))

  (cond
   ((or (not argv)
        (opt? "i")) (repl))

   ((opt? "h") (usage))

   ((opt? "o") (if (not (word 3 argv))
                   (usage "error: '-o' must be followed by EXE and at least one source file")
                   (build (nth 2 argv) (rrest argv))))

   ((opt? "e") (repl-rep (nth 2 argv)))

   ((opt? "x") (repl-file (nth 2 argv)))
   
   ((opt? "%") (usage "Unrecognized option: %q\n" (nth 1 argv)))

   (else (repl-file (first argv)))))

