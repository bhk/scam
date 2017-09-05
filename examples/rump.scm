;;----------------------------------------------------------------
;; https://link.springer.com/article/10.1023/A%3A1015569431383
;;
;; Rump’s example is to compute the expression:
;;
;;   ƒ = 333.75b⁶ + a²(11a²b² − b⁶ − 121b⁴ − 2) + 5.5b⁸ + a/(2b)
;;
;; with a = 77617 and b = 33096. On an IBM S/370 main frame he computed ƒ
;; using single, double, and extended-precision arithmetic, to produce the
;; results:
;;
;;    Single precision: ƒ = 1.172603...
;;    Double precision: ƒ = 1.1726039400531...
;;    Extended precision: ƒ = 1.172603940053178...
;;
;; This suggests a reliable result of approximately 1.172603 or even
;; 1.1726039400532. In fact, however, the correct result (within one unit of
;; the last digit) is
;;
;;   ƒ = −0.827396059946821368141165095479816...
;;
;; Even the sign was wrong.
;;
;; ... For example, the following rearrangement of Rump’s expression
;; produces his original effect on IEEE-754 computers:
;;
;;    ƒ = (333.75 − a²)b⁶ + a²(11a²b² − 121b⁴ − 2) + 5.5b⁸ + a/2b
;;
;; Using 32-bit, 64-bit, and 128-bit round-to-nearest IEEE-754 arithmetic,
;; this produces:
;;
;;    32-bit:  ƒ = 1.172604,
;;    64-bit:  ƒ = 1.1726039400531786,
;;    128-bit: ƒ = 1.1726039400531786318588349045201838.
;;
;; Even this reformulated expression is not completely stable. Wild
;; variations in results may occur for alternative rounding modes, rounding
;; precisions, arithmetic formats (such as 96-bit words), or exponential
;; evaluation methods.
;;
;; --------------------------------
;;
;; SCAM's arbitrary-precision floating point arithmetic produces (perfectly)
;; as many digits of precision as desired.  Here are the first 200:
;;
;;  -0.827396059946821368141165095479816291999033115784384819917814841672
;;  709693014261542180323906212231085327532028039642252840222383369591491
;;  4189025864152767706067198452985255015711868503746676335508822818...

(require "core")
(require "num")

(define (^2 x) (* x x))
(define (^3 x) (* x (^2 x)))
(define (^4 x) (^2 (^2 x)))
(define (^6 x) (^2 (^3 x)))
(define (^8 x) (^2 (^4 x)))

(define (f a b)
  (sum [ (* (- 333.75 (^2 a)) (^6 b))
         (* (^2 a) (- (- (* 11 (* (^2 a) (^2 b))) (* 121 (^4 b))) 2))
         (* 5.5 (^8 b))
         (/ a (* 2 b) 200) ]))

(print (f 77617 33096))
