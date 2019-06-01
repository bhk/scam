;; Sample implementation of `log` and `exp` functions using
;; arbitrary precision floating point primitives.

(require "core")
(require "math")

;;----------------------------------------------------------------
;; log-small: calculate log(x) for values of x very close to 1.
;;----------------------------------------------------------------


;; atanh-loop(u,v,m) = ∑ [n ≥ 0] x * aⁿ / (2n + b)
;;
(define (atanh-loop x a b place total)
  (if (filter "0 NaN" x)
      total
      (atanh-loop (*~ x a place) a (+ b 2) place (+ total (/ x b place)))))


;; arctanh(x) = ∑ [n ≥ 0] x²ⁿ⁺¹ / (2n+1)
;;            = ∑ [n ≥ 0] x * x²ⁿ / (2n+1)
;;
;; PLACE is the decimal place of the least siginificant digit (e.g. -16).
;;
(define `(arctanh-seq x-in place)
  (foreach x x-in
    (atanh-loop x (*~ x x place) 1 place 0)))


;; Compute log(x) for values of X close to 1.
;;
;; DIGITS = number of significant digits to the right of the decimal place.
;;
(define (log-small x digits)
  (define `place
    (.. "-" digits))

  ;; log(x) = 2 * arctanh ((x-1)/(x+1))
  (*~ 2 (arctanh-seq (/ (- x 1) (+ x 1) place) place) place))


(expect -0.693147180559945 (log-small 0.5 16))


;;----------------------------------------------------------------
;; log-fr: Efficiently calculate log(x) for 0.1 ≤ x ≤ 1
;;
;; We first find `a` closest to 1 where:
;;   a = x*Mᵖ
;;   x = a / Mᵖ
;;   log(x) = log(a) - log(M)*p
;;
;; We then compute log(a) and log(M) log-small.
;;----------------------------------------------------------------


(define `scale-m 1.02)
(define scale-max (/ (+ 1 scale-m) 2 5))    ;; ~= (1+m)/2


(define (m-powers-loop x max)
  (if (< x max)
      (._. x (m-powers-loop (* x scale-m) max))))

;; [M M² M³ ...] up to but not including Mⁿ > scale-max*10
(define m-powers
  (strip (m-powers-loop scale-m (* 10 scale-max))))

(define m-powers-r
  (foreach x m-powers (/ scale-max x 4)))


;; X = a number
;; VALUES = a list of numbers in descending order
;; Result = number of entries in VALUES greater than X
;;
(define (count-gt x values ?valuesgt)
  (define `(words/2 lst)
    (words (subst "0 0" 0 (patsubst "%" 0 lst))))

  (if (eq? values valuesgt)
      (words valuesgt)
      (let ((mid (words/2 (._. values valuesgt))))
        (if (> (word mid values) x)
            (count-gt x values (wordlist 1 mid values))
            (count-gt x (wordlist 2 mid (._. 0 values)) valuesgt)))))


(expect 0 (count-gt 10 "5 4 3 2 1"))
(expect 2 (count-gt 3 "5 4 3 2 1"))
(expect 5 (count-gt 0 "5 4 3 2 1"))


;; We "scale" X to a value, A, that is more efficient for log-small by
;; calculating A = X * Mⁿ where A and M are close to 1.
;;
;; 0.1 <= X < 1.1
;;
;; Since 1 <= Mⁿ < ~10, the precision of log(A) must be one more digit than
;; prec.  Since the slope of log(A) is close to 1 at A, A must also have one
;; more digit of precision.
;;
(define (log-fr x digits)
  (let ((p (count-gt x m-powers-r)))
    (define `mᵖ (if (eq? 0 p) 1 (word p m-powers)))
    (define `a (* x mᵖ))

    (- (log-small a (1+ digits))
       (* p (log-small scale-m (+ digits 2))))))


(expect (round (log-small 0.5 19) 16)
        (round (log-fr 0.5 19) 16))


;;----------------------------------------------------------------
;; log
;;----------------------------------------------------------------


(define (log-10 digits)
  (0- (log-fr 0.1 digits)))

(expect 2.302585 (round (log-10 7) 7))


;; Compute logarithm for any X.
;;
;; For X<=0, the result is NaN.
;;
(define (log x ?digits-in)
  ;; log(m * 10ᵉ) = log(m) + e*log(10)
  (define `result
    (let ((m.e (frexp10 x))
          (digits (or digits-in 16)))
      (define `m (word 1 m.e))
      (define `e (word 2 m.e))
      (define `log10-e (word 2 (frexp10 e)))

      (if (filter-out "0 -%" m)
          (round (+ (log-fr m (1+ digits))
                    (* e (log-10 (max 1 (+ digits log10-e)))))
                 (.. "-" digits)))))
  (or result "NaN"))


(expect 4.8978397999509 (log 134 13))
(expect "NaN" (log 0 16))
(expect "NaN" (log -1 16))


;;----------------------------------------------------------------
;; exp
;;----------------------------------------------------------------


;; exp(x) = ∑ [k ≥ 0] xᵏ / k!
;;        = ∑ [k ≥ 0] t(k),  t(0) = 1,  t(k+1) = t(k) * x / (k+1)
;;
;; This converges for all X, but is fastest when X is close to zero.
;;
(define (exp-loop t k+1 total x prec)
  (if (filter "0 NaN" t)
      total
      (exp-loop (/ (*~ t x prec) k+1 prec)
                (+ k+1 1)
                (+ total t)
                x prec)))


(define (exp-small x digits)
  (exp-loop 1 1 0 x (.. "-" digits)))


(expect 2.7182818284590452 (exp-small 1 16))


;; Calculate eˣ.
;;
;; DIGITS = numer of significant digits (left or right of the decimal).
;;
;; We use this identity:
;;   exp(log(z) + p*log(10)) = z * 10ᵖ
;;
(define (exp x ?digits-in)
  (let ((digits (1+ (or digits-in 16))))
    (let ((L10 (log-10 digits)))
      (let ((p (// x L10)))
        (let& ((lz (- x (* p L10))))
          ;; log(Z) ∈ [0,~2.3]  Z ∈ [1,~10]
          (round (* (exp-small lz digits) (.. "1e" p))
                 digits-in))))))


(expect 59874.1417 (exp 11 9))
(expect 0.1234 (exp (log 0.1234 23) 23))

;;================================================================


(define (main argv)
  (define `fnmap
    {log: log, exp: exp})

  (define `fn
    (or (dict-get (nth 1 argv) fnmap)
        (lambda () "[function not found]")))

  (if argv
      (printf "(%s) → %q" argv (apply fn (rest argv)))
      (print "Usage: logexp FUNCTION ARG..."))
  0)
