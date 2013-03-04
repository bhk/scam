(require "scamc" &private)

(declare I)
(declare O)

(let-global
 ((I (concat (dir *file*) "scamc-q-tmp"))
  (O (concat I ".min")))

 (write-file I "(define (f x) (subst 1 11 x))\n")

 (main)

 (let ((out (read-file O)))
   (expect 1 (see "f = $(subst 1,11,$1)\n" out))
   (expect 1 (see "# Exports: f" out)))

 (shell (concat "rm " O)))

;; we've already run main; prevent SCAMC from being run again
(define (main) "")

