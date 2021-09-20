;; Sample implementation of trigonometric functions using arbitrary
;; precision floating point primitives.

(require "core")
(require "math")


;;----------------------------------------------------------------
;; pi
;;----------------------------------------------------------------


(define (atan-loop u v total m place)
  (if (filter "0 NaN%" u)
      total
      (atan-loop (*~ m u place)
                 (+ v 2)
                 (+ total (/ u v place))
                 m place)))


;; atan(x) = ∑ [n ≥ 0] (-1)ⁿ * x²ⁿ⁺¹ / (2n+1)  , n ≥ 0
;;
(define (appx-atan x place)
  (atan-loop x 1 0 (0- (*~ x x place)) place))


;; Calculate PI to within 10^PLACE.
;;
(define (calc-pi place)
  ;; π/4 = 4*acot(5) - acot(239)
  (/ (* 4 (- (* 4 (appx-atan 0.2 place))
             (appx-atan (/ 1 239 place) place)))
     1
     place))


;; Calculate PI to DIGITS significant digits, caching results.
;;
(define (pi ?digits)
  (declare pi-digits)
  (declare pi-value)

  (define `N
    (or digits 16))

  (if (> N pi-digits)
      (begin
        (set pi-value (calc-pi (.. "-" (+ N 5))))
        (set pi-digits N)))
  (round pi-value N))


(expect (pi 5) 3.1416)
(expect (pi) 3.141592653589793)
(let-global ((calc-pi "undef"))
  ;; test cached results
  (expect (pi 16) 3.141592653589793)
  (expect (pi 5) 3.1416))


;;----------------------------------------------------------------
;; sin & cos
;;----------------------------------------------------------------


(define (sin-loop v t total m place)
  (if (filter "0 NaN%" t)
      total
      (sin-loop (+ v 2)
                (/ (/ (*~ t m place) v place) (+ v 1) place)
                (+ total t)
                m place)))


(define (log10i x)
  (- (word 2 (frexp10 x)) 1))


(declare (appx-cos x place))


;; sin(x) = ∑ (-1)^n * x^(2n+1) / (2n+1)!   , n ≥ 0
;;        = ∑ t(n) where:
;;               t(0) = x
;;               t(n+1) = t(n) * -x² / (v * (v+1))
;;               v = 2n+2
;;               m = -x²
;;
(define (appx-sin x place)
  ;; When calculating X%PI for large X we need PI with many digits
  (define `PI
    (pi (+ (log10i x) (abs place))))

  (if (filter "-%" x)
      (0- (appx-sin (0- x) place))
      (if (< x 0.8)
          (sin-loop 2 x 0 (0- (*~ x x place)) place)
          (if (> x 6.2832)
              (appx-sin (mod x (* PI 2)) place)
              (if (> x 3.1416)
                  (0- (appx-sin (- x PI) place))
                  (if (> x 1.5707)
                      (appx-sin (- PI x) place)
                      (appx-cos (- (/ PI 2 place) x) place)))))))


;; cos(x) = ∑ (-1)^n * x^(2n) / (2n)!   , n ≥ 0
;;        = ∑ t(n) where:
;;               t(0) = 1
;;               t(n+1) = t(n) * -x² / (v * (v+1))
;;               v = 2n+1
;;               m = -x²
;; PLACE = "-NN"
;;
(define (appx-cos x place)
  ;; When calculating X%PI for large X we need PI with many digits
  (define `PI
    (pi (+ (log10i x) (abs place))))

  (if (filter "-%" x)
      (appx-cos (0- x) place)
      (if (< x 0.8)
          (sin-loop 1 1 0 (0- (*~ x x place)) place)
          (if (> x 6.2832)
              (appx-cos (mod (abs x) (* PI 2)) place)
              (if (> x 3.1416)
                  (appx-cos (- (* PI 2) x) place)
                  (if (> x 1.5708)
                      (0- (appx-cos (- PI x) place))
                      (appx-sin (- (/ PI 2 place) x) place)))))))



;; Call approximating function after validating digits-in.
;;
(define (appx fn x digits-in)
  (foreach (loop-digits (+ 2 (or digits-in 16)))
    (or (filter "NaN" loop-digits)
        (round (fn x (.. "-" loop-digits))
               (.. "-" (or digits-in 16))))))


(define (sin x ?digits)
  &public
  (appx appx-sin x digits))


(define (cos x ?digits)
  &public
  (appx appx-cos x digits))


(define (atan x ?digits)
  &public
  (appx appx-atan x digits))


(expect "NaN" (sin nil))
(expect (sin 0.1 27) 0.099833416646828152306814198)
(expect (sin 0.00001 27) 0.000009999999999833333333334)
(expect (cos 0.1 27) 0.995004165278025766095561988)


(define `(check-range value min-value max-value)
  (expect min-value (min value min-value))
  (expect max-value (max value max-value)))

(define ~pi 3.1415926)
(define pi/2+e (+ (/ ~pi 2) 0.1))
(define pi/2-e (- (/ ~pi 2) 0.1))
(define pi+e (+ ~pi 0.1))
(define pi-e (- ~pi 0.1))
(define pi*3/2+e (* ~pi 1.51))
(define pi*3/2-e (* ~pi 1.49))
(define pi*2+e (* ~pi 2.01))
(define pi*2-e (* ~pi 1.99))

;; Validate domains used to calculate COS
(check-range (cos -0.1) 0.99 1)
(check-range (cos 0.1) 0.99 1)
(check-range (cos pi/2-e) 0 0.1)
(check-range (cos pi/2+e) -0.1 0)
(check-range (cos pi-e) -1 -0.99)
(check-range (cos pi+e) -1 -0.99)
(check-range (cos pi*3/2-e) -0.1 0)
(check-range (cos pi*3/2+e) 0 0.1)
(check-range (cos pi*2-e) 0.99 1)
(check-range (cos pi*2+e) 0.99 1)
(check-range (cos (* (pi) 1000)) 0.99 1)
(check-range (cos (* (pi) 1001)) -1 -0.99)

;; Validate domains used to calculate SIN
(check-range (sin -0.1) -0.1 0)
(check-range (sin 0.1) 0 0.1)
(check-range (sin pi/2-e) 0.99 1)
(check-range (sin pi/2+e) 0.99 1)
(check-range (sin pi-e) 0 0.1)
(check-range (sin pi+e) -0.1 0)
(check-range (sin pi*3/2-e) -1 -0.99)
(check-range (sin pi*3/2+e) -1 -0.99)
(check-range (sin pi*2-e) -0.1 0)
(check-range (sin pi*2+e) 0 0.1)
(check-range (sin (* (pi) 1000)) -0.1 0.1)


;;================================================================

(define (main argv)
  (define `fnmap
    { sin: sin, cos: cos, atan: atan, pi: pi})

  (define `fn
    (or (dict-get (nth 1 argv) fnmap)
        (lambda () "[function not found]")))

  (if argv
      (printf "(%s) → %q" argv (apply fn (rest argv)))
      (print "Usage: trig FUNCTION ARG..."))
  0)
