(require "core")
(require "../math2.scm" &private)
(require "../math.scm")

;;----------------------------------------------------------------
;; Math Accuracy
;;
;; Here we test approximating functions for accuracy.
;;
;; Only a couple of functions provide exact rounding ("to the nearest
;; digit"): `/` and `round`.  These are not covered by this test, because
;; their operation is more easily validated.
;;
;; Functions like the transcendental functions have irrational results that
;; cannot be completely evaluated, and functions like `*~` cannot compute
;; exact results without defeating their main goal which is to avoid
;; unnecessary calculation.
;;
;; For these functions, the guarantee we provide is generally "within one
;; unit of the last digit of precision" -- or "1U", in our internal
;; nomenclature.  This means that the preliminary result (the input to the
;; rounding operation) must be within 0.5U of the actual value, because
;; rounding itself will add up to 0.5U of error.  Each extra digit of
;; calculation will divide this preliminary error by 10, but at additional
;; cost, and it will never reach zero.
;;
;; There are three main way we test them:
;;
;; 1. We test the functions against themselves.  A low-precision answer
;;    should agree, within the 1U bound, of a higher-precision answer.
;;
;; 2. Against each other:  `ln` should invert `exp`, and vice-versa.
;;
;; 3. Against reference values.  Just in case some confluence of systemic
;;    errors afflicts our implementation, reference values keep us in check.
;;    These should provide basic classical "coverage" of code paths.
;;
;; f-ln, f-log, f-exp
;;
;;----------------------------------------------------------------


;; Refernence values



;; Increase/decrease precision PREC by N positions
(define `(prec+ prec n)
  (if (filter "+% -%" prec)
      (subst "+-" "-" (.. "+" (- (subst "+" nil prec) n)))
      (max 1 (+ prec n))))


(expect 5 (prec+ 3 2))
(expect "+1" (prec+ "+3" 2))
(expect "-2" (prec+ "+3" 5))
(expect "+4" (prec+ "+3" -1))
(expect "-5" (prec+ "-3" 2))
(expect "-2" (prec+ "-3" -1))


(define `(expect± max-delta a b)
  (let ((a a)
        (b b)
        (max-delta max-delta))
    (let ((Δ (abs (- a b))))
      (if (or (filter "N%" Δ)
              (> Δ max-delta))
          (begin
            (print "delta: " Δ " > " max-delta)
            (expect a b))))))

;; (log X)  [and f-log indirectly]

(expect "NaN" (log nil))
(expect "NaN" (log 0))
(expect "NaN" (log -1))
(expect 0 (log 1))
(expect -0.0010005 (log 0.999 nil 5))
(expect 0.0009995 (log 1.001 nil 4))
(expect 2.302585092994046 (log 10))
(expect 2302.585092994046 (log 1e1000))
(expect 1.1 (log 3 nil 2))
(expect± 1e-6 0.69314718 (log 2 nil 6))
(expect± 1e-4 2.197224 (log 9 nil 5))
(expect 115.129 (log 1e50 nil 6))
;; PREC = place
(expect 115.129 (log 1e50 nil "-3"))
(expect 1151.29 (log 1e500 nil "-2"))
(expect 1200 (log 1e500 nil "+2"))
(expect 1151300 (log 1e500000 nil "+2"))
;; PREC = significant when close to zero
(expect± 1e-22 (log 1.000001) (log 1.000001 nil 20))

;; log: precision

;; Obtain values prior to final rounding operation.
;;
(define `(round-to prec expr)
  (let-global ((result-pod (prec-to-pod prec)))
    expr))

(define (log+ x b p)
  (round-to (prec+ p 5) (log x b p)))

(define (logx+ x p)
  (log+ x nil p))

;; The below tests are overly conservative checks, but on shorter lengths.
(expect± 0.12e-14 -0.23572233352106987386 (logx+ 0.79 14))
(expect± 0.13e-13 -0.33855440282553749817 (logx+ 0.7128 13))
(expect± 1e-15 0.593326845277734378803 (logx+ 1.81 14))
(expect± 0.14e-8 1153.3594092564958 (logx+ 7.9e+500 12))

;; (log X BASE)

(expect "NaN" (log 10 1))
(expect 0 (log 1 10))
(expect "NaN" (log 0 10))
;; IEEE says log(X,0)= 0 ... mathematically incorrect, and of dubious value
;; in calculations given the discontinuity. E.g. log(10,SMALLEST) ≈ -0.0032.
(expect "NaN" (log 10 0))
(expect "NaN" (log 10 -2))
(expect "NaN" (log -2 10))
(expect 10 (log 1024 2))
;; PREC=DIGITS (with small result)
(expect 4.3433574e-10 (log 1.00001 1e9999 8))  ;; 4.34335744019720658e-10
;; PREC=PLACE, big X, small BASE
(expect 230029400.93 (log 1e999 1.00001 "-2"))  ;; 230029400.929444668
(expect 200000000 (log 1e999 1.00001 "+8"))
(expect 0 (log 1e999 1.00001 "+9"))
;; PREC=PLACE, small X, big BASE
(expect 4.34e-10 (log 1.00001 1e9999 "-12"))    ;; 4.34335744019720658e-10
(expect 0 (log 1.00001 1e9999 "+1"))    ;; 4.34335744019720658e-10
(expect -985.33206962 (log 0.99 1.0000102 11))
(expect -990 (log 0.99 1.0000102 "+1"))
(expect -1000 (log 0.99 1.0000102 "+2"))

;; exp

(expect 3 (exp 1 1))
(expect 2.7 (exp 1 2))
(expect 2.718 (exp 1 4))
(expect 0 (exp 1 "+1"))
(expect 3 (exp 1 "+0"))
(expect 3 (exp 1 "-0"))
(expect 2.7 (exp 1 "-1"))
(expect 2.718 (exp 1 "-3"))
(expect 59874.1417 (exp 11 9))
(expect 59874.142 (exp 11 "-3"))
(expect 59874.1417 (exp 11 "-4"))
(expect 59900 (exp 11 "+2"))
(expect 1.1051709 (exp 0.1 8))
(expect 0.90483742 (exp -0.1 8))

(expect± 1e-16 9.98915495506478668 (exp 2.3015 17))
(expect± 0.1e-16 9.98915495506478668 (round-to 21 (exp 2.3015 17)))
(expect± 0.1e-16 2.11700001661267467 (round-to 21 (exp 0.75 17)))

;; large exponent
(expect± 1e79 7.225973768e+86 (round-to 12 (exp 200 8)))
(expect± 1e-93 1.383896527e-87 (round-to 12 (exp -200 8)))

;; pow

(expect (pow 2 2) 4)
(expect (pow 2 0) 1)
(expect (pow 0 3) 0)
(expect (pow 0 0) "NaN")
(expect (pow 2 -1 5) 0.5)
(expect (pow -2 2 5) "NaN")
;; PREC = place
(expect 6.72749995e+420 (pow 11e20 20 "+412"))
(expect 1e+421 (pow 11e20 20 "+421"))
(expect 0 (pow 11e20 20 "+422"))
(expect 6.72749995e-380 (pow 11e-20 20 "-388"))
;; PREC = digits
(expect 2.85976221e-2757241303992110 (pow 0.53 1e16 10))
(expect 141421356.2 (pow 2e16 0.5 10))

;; sin & cos

;; Validate handling of different ranges of sinc & cos
(define ~π 3.14159)
(define (validate-sincos fn pattern)
  (for (n (range 1 (words pattern)))
    (define `θ (* ~π (/ (- n 1) 6)))
    (expect (fn θ -2) (nth n pattern))))

(validate-sincos
 sin [0 0.5 0.87 1 0.87 0.5 0 -0.5 -0.87 -1 -0.87 -0.5 0 0.5 0.87 1])
(validate-sincos
 cos [1 0.87 0.5 0 -0.5 -0.87 -1 -0.87 -0.5 0 0.5 0.87 1 0.87 0.5 0])

;; validate precision: place
(expect 0 (sin 0.5 "+1"))
(expect 1 (cos 0 "+0"))
(expect 0 (cos 0 "+1"))

(define (sin+ x prec)
  (round-to (prec+ prec 4) (sin x prec)))

(define (cos+ x prec)
  (round-to (prec+ prec 4) (cos x prec)))

;; test above/below each boundary (0.8, 2.36, 5.5, 2π)
(expect± 0.1e-12 (sin+ 0.79 -12) 0.7103532724176078)
(expect± 0.1e-12 (cos+ 0.79 -12) 0.7038453156522361)
(expect± 0.1e-12 (sin+ 0.82 -12) 0.7311458297268959)
(expect± 0.1e-12 (cos+ 0.82 -12) 0.6822212072876136)
(expect± 0.1e-12 (sin+ 2.35 -12) 0.7114733527908444)
(expect± 0.1e-12 (cos+ 2.35 -12) -0.7027130767735539)
(expect± 0.1e-12 (sin+ 2.37 -12) 0.6972777382599378)
(expect± 0.1e-12 (cos+ 2.37 -12) -0.7168010572865428)
(expect± 0.1e-12 (sin+ 5.49 -12) -0.7125916284799616)
(expect± 0.1e-12 (cos+ 5.49 -12) 0.7015790554315859)
(expect± 0.1e-12 (sin+ 5.51 -12) -0.6984184692162134)
(expect± 0.1e-12 (cos+ 5.51 -12) 0.7156896267640611)
(expect± 0.1e-12 (sin+ 6.29 -12) 0.006814640074770140)
(expect± 0.1e-12 (cos+ 6.29 -12) 0.9999767800707431)
;; large numbers
(expect± 0.1e-12 (sin+ 314159265358979 -12) -0.3182152351447918)

;; validate precision: digits
(expect± 0.1e-22 (sin+ 3.14159265358979 8) 3.2384626433832795e-15)
(expect± 0.1e-17 (sin+ 3.14159 12) 0.0000026535897932353484)

;; get-pi

(expect 3.14 (get-pi 3))
(expect 3.14 (get-pi "-2"))
(expect 3 (get-pi 1))
(expect 3 (get-pi "+0"))
(expect 0 (get-pi "+1"))
(expect 3.14159265359 (get-pi 13))  ;; trim trailing zero
(expect "NaN" (get-pi "blah"))
