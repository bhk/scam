(require "core")

(define (f ...args)
  (if (rest args)
      (._. (apply f (rest args)) (first args))
      args))

(define (g vec)
  (if (rest vec)
      (? g (rest vec))
      vec))

(define (main args)
  (cond ((eq? "g" (first args))
         (g [1 2 3]))
        (else
         (print (f 1 2 3))))
  0)
