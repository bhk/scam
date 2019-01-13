(require "core")
(require "math")

;;----------------------------------------------------------------
;; Phi (the Golden Ratio) = (1 + √5)/2
;;
;; Starting at any two consecutive Fibonacci numbers a & b, we know the
;; subsequent elements will be: a+b, a+2b, 2a+3b, 3a+5b, 5a+8b, 8a+13b, ...
;; The i'th subsequent element is given by Fi*a+F(i+1)*b.  With any a=Fn and
;; b=F(n+1) we can compute two larger consecutive Fibonacci numbers:
;;
;;   F(2n+1) = F(n+1 + n) = Fn*a + F(n+1)*b = a² + b²
;;   F(2n+2) = F(n+1 + n+1) = F(n+1)*a + F(n+2)*b = b*a + (a+b)*b = 2a*b + b²
;;
;; Repeating this will double the number of digits per iteration.
;; Remarkably, dividing any two consecutive N+1-digit Fibonacci numbers
;; gives us ϕ with 2*N digits of precision.
;; ----------------------------------------------------------------


;; Generate a pair of successive Fibonacci numbers greater than min, and
;; then return the ratio.
;;
(define (get-fibs a b min)
  (if (> a min)
      [a b]
      (foreach b^2 (* b b)
        (get-fibs (+ (^ a 2) b^2) (+ (* 2 (* a b)) b^2) min))))


(define (calc-phi digits)
  (let ((pair (get-fibs 433494437 701408733 (concat "1e" (/ digits 2)))))
    (/ (nth 2 pair) (first pair) digits)))


(define (main argv)
  (print (calc-phi (or (first argv) 80))))
