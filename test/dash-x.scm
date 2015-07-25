;; "scam -x SOURCE" test file
;;
;; - Requires bundled files.
;; - Is given "file" arguments properly in argv.
;; - When a number is returned, it is treated as the exit code.
;;

(require "core")
(require "num")

(define (main argv)
  ;; return exit code (check content and lenth of words
  (print (concat-vec (append (^ (nth 1 argv) 2)
                             argv)
                     ":"))
  0)
