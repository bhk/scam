(require "core")
(require "math")

;;----------------------------------------------------------------
;; Pi
;;----------------------------------------------------------------

(define `(negate n)
  (subst "--" nil (concat "-" n)))


(define (arccot-loop digits p^2 x u 2k+1)
  (if (< (abs u) (concat "1e-" digits))
      (/ x 1 digits)
      (let ((u2 (/ (negate u) p^2 digits)))
        (define `next-x (+ x (/ u2 2k+1 digits)))
        (arccot-loop digits p^2 next-x u2 (+ 2k+1 2)))))


;; arccot(p) = arctan(1/p) = ∑ (-1)^k / ( (2k+1) p^(2k+1) )   [k>=0]
;;
(define (arccot p digits)
  (let ((ip (/ 1 p digits)))
    (arccot-loop digits (* p p) ip ip 3)))


;; PI/4 = 4*arccot(5) - arccot(239)
;;
(define (calc-pi digits)
  (* 4 (- (* 4 (arccot 5 digits)) (arccot 239 digits))))


;;----------------------------------------------------------------
;; Phi (the Golden Ratio)
;;----------------------------------------------------------------

;; Generate a pair of successive Fibonacci numbers greater than min, and
;; then return the ratio.
(define (get-fibs a b min)
  (if (> a min)
      [a b]
      (foreach b^2 (* b b)
        (get-fibs (+ (^ a 2) b^2) (+ (* 2 (* a b)) b^2) min))))


(define (calc-phi digits)
  (let ((pair (get-fibs 433494437 701408733 (concat "1e" (/ digits 2)))))
    (/ (nth 2 pair) (first pair) digits)))



;;----------------------------------------------------------------
;; main
;;----------------------------------------------------------------

;;   pi   100 digits in 15 seconds
;;  phi  1000 digits in  5 seconds
;;
(define (main argv)
  (define `fn
    (or (dict-get (first argv) {pi: calc-pi,
                                phi: calc-phi})
        (lambda () (print "usage: numbers CONST [DIGITS]"))))
  (print (fn (or (nth 2 argv) 10))))
