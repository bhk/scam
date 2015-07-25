;; Test "scam -o EXE SOURCE" behavior
;;

(require "core")        ; provided by bundled file
(require "subdir/dup")  ; in a directory relative to this one

(define (main argv)
  ;; print to stdout
  (print "result=" (dup (first argv)) ":" (concat-vec (rest argv) ":")))
