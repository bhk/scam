(require "core.scm")
(require "math0.scm" &private)
(require "math1.scm" &private)
(require "math2.scm" &private)
;; Note: math0-q and math1-q are not ordinarily run as unit tests because
;; they are always required with "&private".  This file *does* get run as a
;; unit test, so here we ensure that math0.scm & math1.scm are tested before
;; math.scm.  Also, we pull in some test utilities.
(require "math0-q.scm" &private)
(require "math1-q.scm" &private)
(require "math2-q.scm" &private)
(require "math.scm" &private)

;;--------------------------------
;; Operators
;;--------------------------------

;; Ensure spaces are treated as invalid characters in a binop
;;
(define `(check-binop-spaces fn)
  (expect NaN (fn "01" " 01"))
  (expect NaN (fn "01" "01 "))
  (expect NaN (fn "01" "- 01"))
  (expect NaN (fn " 01" "01"))
  (expect NaN (fn "01 " "01"))
  (expect NaN (fn "- 01" "01")))

;; non-integers?

(foreach n ["-" "--1" " -1" "- 1" "0-1" "1-0" "?"]
         (assert (non-integers? n "01"))
         (assert (non-integers? "01" n)))
(assert (not (non-integers? "-0" "01")))

;; non-naturals?

(assert (non-naturals? (U 1) (U -1)))
(assert (not (non-naturals? (U 1) (U 2))))

;; raw-add

(expect (U 3.2) (raw-add (U 1.2) (U 2)))
(expect (U 3) (raw-add (U 1) (U 2)))
(expect (U 0) (raw-add (U -1) (U 1)))
(check-binop-spaces raw-add)

;; raw-sub

(expect (U -0.8) (raw-sub (U 1.2) (U 2)))
(expect (U 0) (raw-sub (U -1) (U -1)))
(expect (U -1) (raw-sub (U 1) (U 2)))
(check-binop-spaces raw-sub)

;; raw-mul

(expect (U 2.4) (raw-mul (U 1.2) (U 2)))
(expect (U 63)  (raw-mul (U -21) (U -3)))
(expect (U -63) (raw-mul (U 21) (U -3)))
(expect (U -63) (raw-mul (U -21) (U 3)))
(expect (U 0)   (raw-mul (U -21) (U 0)))
(expect (U 63)  (raw-mul (U 21) (U 3)))
(check-binop-spaces raw-mul)

;; raw-fdiv

(expect (U 1) (raw-fdiv (U 3.71) (U 3)))
(expect (U 4) (raw-fdiv (U 14) (U 3)))
(expect NaN (raw-fdiv (U 1) 0))
(check-binop-spaces raw-fdiv)

;; raw-mod

(expect (U 0.71) (raw-mod (U 3.71) (U 3)))
(expect (U 7.1) (raw-mod (U 37.1) (U 30)))
(expect (U 2) (raw-mod (U 14) (U 3)))
(expect NaN (raw-mod (U 1) 0))
(check-binop-spaces raw-mod)

;; raw-round

(expect "NaN" (raw-round nil 0 DIV-NEAREST))
(expect 120 (raw-round 123 -01 DIV-NEAREST))
(expect 130 (raw-round 123 -01 DIV-CEILING))

;; raw-cmp & math-cmp

(assert (findstring 1 (math-cmp 12.3 1.32)))
(assert (findstring 1 (math-cmp 2 -3)))
(assert (findstring 1 (math-cmp 2 1)))

;; raw-pwr

(expect NaN (raw-pwr (U 2) nil))
(expect NaN (raw-pwr (U 2) (U -2)))
(expect (U 1) (raw-pwr (U 1e3) (U 0)))
(expect (U 1) (raw-pwr (U -1e3) (U 0)))
(expect (U 4294967296) (raw-pwr (U 2) (U 32)))

;;--------------------------------
;; Misc.
;;--------------------------------

;; 0-

(expect -1 (0- 1))
(expect 1 (0- -1))

;; abs

(expect 1.0 (abs -1.0))
(expect 1.0 (abs 1.0))

;; max

(expect 3.0 (max 1.0 3.0))
(expect 3.0 (max 3.0 nil))
(expect 3.0 (max 3.0 "NaN"))

;; min

(expect 1.0 (min 1.0 3.0))
(expect nil (min 3.0 nil))
(expect "NaN" (min 3.0 "NaN"))

;; sum

(expect 1 (sum 1.0))
(expect 3.1 (sum 1 2.1))
(expect 7.1 (sum [ [1 2.1] ] 4))

;;--------------------------------
;; range
;;--------------------------------

;; uv-trim

(expect (uv-trim "0" "0" "1 2 3") "1 2 3")
(expect (uv-trim "01" "0" "1 2 3") "2 3")
(expect (uv-trim "01" "01" "1 2 3") "2")
(expect (uv-trim "011" "01" "1 2 3") "")

;; uv-range

(expect "1 2 3" (strip (uv-range (UV 1) (UV 3))))
(expect "0 1 2 3" (strip (uv-range (UV 0) (UV 3))))
(expect "8 9 10" (strip (uv-range (UV 8) (UV 10))))

;; range

(expect "1 2 3" (range 1 3))
(expect "0 1 2 3" (range 0 3))
(expect "0 1 2 3" (range -0 3))
(expect "-2 -1 0 1" (range -2 1))
(expect "-2 -1 0" (range -2 0))
(expect "-2 -1 0" (range -2 -0))
(expect "0" (range 0 0))
(expect "0" (range -0 -0))
(expect "0" (range -0 0))
(expect "0" (range 0 -0))
(expect "8 9 10" (range 8 10))
(expect "98 99 100" (range 98 100))
(expect nil (range "NaN" 100))
(expect nil (range nil 100))
(expect nil (range 1.0 2.0))  ;; not yet supported
(expect nil (range 1 1e2))  ;; not yet supported

;;--------------------------------
;; format-fixed
;;--------------------------------

;; format-fixed

(expect (format-fixed nil nil nil) "nan")
(expect (format-fixed nil 5 nil) "  nan")
(expect (format-fixed 1 "x" nil) "[invalid_MIN-WIDTH]")
(expect (format-fixed 1 nil "x") "[invalid_PRECISION]")
(expect (format-fixed 1e5 nil nil) "100000")
(expect (format-fixed 1e5 nil 2) "100000.00")
(expect (format-fixed 1 3 nil) "  1")
(expect (format-fixed 0 3 nil) "  0")
(expect (format-fixed 1e5 10 2) " 100000.00")
;; rounding
(expect (format-fixed 1.99999 3 3) "2.000")
(expect (format-fixed 0.99999 3 3) "1.000")
(expect (format-fixed 9.9999 3 3) "10.000")
(expect (format-fixed 0.0001e4 nil nil) "1")
(expect (format-fixed 1e-999999999 nil 3) "0.000")


;;--------------------------------
;; num-lex, num-sort
;;--------------------------------

;; lex-exp

(define `(elt n)
  (u2d (lex-exp (d2u n))))

(expect "1"     (elt 1))
(expect "8"     (elt 8))
(expect "909"   (elt 9))
(expect "910"   (elt 10))
(expect "989"   (elt 89))
(expect "99090" (elt 90))
(expect "09"   (elt 0))
(expect "08"   (elt -1))
(expect "01"   (elt -8))
(expect "0090" (elt -9))

;; num-lex

(expect 0 (num-lex 0))
(expect 11 (num-lex 1))
(expect 11 (num-lex 01))
(expect 18 (num-lex 8))
(expect 1911 (num-lex 9.11))
(expect "091" (num-lex 0.1))
(expect "099" (num-lex 0.9))
(expect "0991" (num-lex 0.91))
(expect "-900:" (num-lex -0.9))
(expect "-9008:" (num-lex -0.91))

;; num-sort

(define `sort-test-vec
  { -2: "a", -0.91: "b", -0.01: "c", 0.9: "d", 2: "e", 10: "f"})
(expect sort-test-vec (num-sort (sort sort-test-vec)))

;; /

(expect 1.2 (/ 3.6 3 4))
(expect NaN (/ 1.2 3 "x"))
(expect 6.67 (/ 20 3 3))
(expect 6.6667 (/ 20 3 "-4"))

;; *~

;; not (prec and x and y)
(expect NaN (*~ 1212 1001 "x"))
(expect NaN (*~ "x" 1001 2))
(expect NaN (*~ 1001 "x" 2))
;; PREC=digits, most-significant-digit>0
(expect 147000 (*~ 333 444 3))
;; PREC=digits, most-significant-digit==0
(expect 73900 (*~ 333 222 3))
;; PREC=place,  place much left of result
(expect 0 (*~ 333 222 "+7"))
(expect 0 (*~ 333 222 "+5"))
;; PREC=place,  place in result
(expect 73926 (*~ 333 222 "+0"))
(expect 73920 (*~ 333 222 "+1"))
(expect 70000 (*~ 333 222 "+4"))
(expect 73926 (*~ 333 222 -1))

;; `binop` functions

(foreach
    op [+ - * // mod]
    (expect NaN (+ NaN 1))
    (expect NaN (+ nil 1))
    (expect NaN (+ 1 nil))
    (expect NaN (+ "-" 1))
    (expect NaN (+ "--" 1))
    (expect NaN (+ "- 0" 1))
    (expect NaN (+ "1 " 1)))


(expect 5 (+ 3 2))
(expect 4 (- 13 9))
(expect 6 (* 3 2))
(expect 4 (// 9 2))
(expect 2 (mod 29 9))
(expect 9.261 (^ 2.1 3))

;; floor

(expect 3 (floor 3.001))
(expect -4 (floor -3.001))

;; ceil

(expect 4 (ceil 3.001))
(expect -3 (ceil -3.001))

;; trunc

(expect 3 (trunc 3.001))
(expect -3 (trunc -3.001))

;; round

(expect "NaN" (round "x"))
(expect 120 (round 123 2))
(expect 123 (round 123.456))
(expect 123.46 (round 123.456 -2))
(expect 123.45 (round 123.456 -2 "-"))
(expect 123.45 (round 123.456 -2 "|"))
(expect 123.46 (round 123.451 -2 "+"))
(expect -123.46 (round -123.456 -2 "-"))
(expect -123.45 (round -123.456 -2 "|"))
(expect -123.45 (round -123.456 -2 "+"))

;; relational operators
;; <, >, =, !=, <=, >=

(expect 1   (< 1 2))
(expect nil (< 2 2))
(expect nil (< 3 2))

(expect nil (> 1 2))
(expect nil (> 2 2))
(expect 1   (> 3 2))

(expect nil (= 1 2))
(expect 1   (= 2 2))
(expect nil (= 3 2))

(expect 1   (!= 1 2))
(expect nil (!= 2 2))
(expect 1   (!= 3 2))

(expect 1   (<= 1 2))
(expect 1   (<= 2 2))
(expect nil (<= 3 2))

(expect nil (>= 1 2))
(expect 1   (>= 2 2))
(expect 1   (>= 3 2))


;;--------------------------------
;; Stress Tests
;;--------------------------------

(define naturals
  [ 1 2 3 9 10 23 99 199 299 10000001 99999999
    21356736517905213567365179051 ])

(define decimals
  (append naturals
          (foreach s ".1 .9999 .0000001 .0000555531"
                   (addsuffix s naturals))))

(define num-args
  (append 0
          decimals
          (addprefix "-" decimals)
          (foreach e "e1 e-3 e5 e20 e-100"
                   (addsuffix e [2 5.000001 9.9999999]))))


(define (num-eq? x y)
  (or (eq? x y)
      (= x y)))


;; Validate arithmetic identities
;;
(define (stress-test x-args y-args)
  (foreach x x-args
           (expect 1 (<= (floor x) x))
           (expect 1 (>= (ceil x) x)))

  (foreach
      x x-args
      (print "x = " x)
      (foreach
          y (filter-out "0 -0" y-args)

          (define `(check-eq a b)
            (if (not (num-eq? a b))
                (begin
                  (print "x = " x ", y = " y)
                  (expect a b))))

          (check-eq x (+ (- x y) y))
          (check-eq x (+ (* y (// x y)) (mod x y)))
          (check-eq x (/ (* x y) y 16))
          nil)))


(define (main argv)
  (if argv
      (stress-test num-args num-args))

  ;; success
  nil)


;;--------------------------------
;; Transcendentals
;;--------------------------------


;; Increase/decrease precision PREC by N positions
(define `(prec+ prec n)
  (if (filter "+% -%" prec)
      (subst "+-" "-" (concat "+" (- (subst "+" nil prec) n)))
      (max 1 (+ prec n))))


(expect 5 (prec+ 3 2))
(expect "+1" (prec+ "+3" 2))
(expect "-2" (prec+ "+3" 5))
(expect "+4" (prec+ "+3" -1))
(expect "-5" (prec+ "-3" 2))
(expect "-2" (prec+ "-3" -1))


(define `(expect± max-delta a b)
  (let ((a a)
        (b b))
    (let ((Δ (abs (- a b))))
      (if (or (filter "N%" Δ)
              (> Δ max-delta))
          (begin
            (print "delta: " Δ " > " max-delta)
            (expect a b))))))

;; (log X)

(expect "NaN" (log nil))
(expect "NaN" (log 0))
(expect "NaN" (log -1))
(expect 0 (log 1))
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
(expect± 1e-15 -0.00999999999999994303038 (logx+ 1.010050167084168 14))
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
(expect -4.3433574e-10 (log 1.00001 1e9999 8))  ;; -4.34335744019720658e-10
;; PREC=PLACE, big X, small BASE
(expect -230029400.93 (log 1e999 1.00001 "-2"))  ;; -230029400.929444668
(expect -200000000 (log 1e999 1.00001 "+8"))
(expect 0 (log 1e999 1.00001 "+9"))
;; PREC=PLACE, small X, big BASE
(expect -4.34e-10 (log 1.00001 1e9999 "-12"))    ;; -4.34335744019720658e-10
(expect 0 (log 1.00001 1e9999 "+1"))    ;; -4.34335744019720658e-10
(expect 985.33206962 (log 0.99 1.0000102 11))
(expect 990 (log 0.99 1.0000102 "+1"))
(expect 1000 (log 0.99 1.0000102 "+2"))

;; exp

(expect "NaN" (exp nil 1))
(expect "NaN" (exp "X" 1))

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
  (for n (range 1 (words pattern))
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
(expect "NaN:PREC" (get-pi "blah"))

;; atan2

(expect "NaN" (atan2 0 0))
(expect "NaN" (atan2 "x" 1))
(expect "NaN:PREC" (atan2 0 0 "x"))
(expect (atan2 0 2) 0)
(expect (atan2 0 -2 6) (get-pi 6))

(define (atan2+ y x prec)
  (round-to (prec+ prec 4) (atan2 y x prec)))

(expect± 0.1e-6 (atan2+ 2 2 6) 0.78539816339745)
(expect± 0.1e-5 (atan2+ 2 0 6) 1.5707963267949)
(expect± 0.1e-5 (atan2+ -2 0 6) -1.5707963267949)

(expect± 0.12e-6 (atan2+ 2 3 6) 0.58800260354757)
(expect± 0.1e-6 (atan2+ 3 2 6) 0.98279372324733)
(expect± 0.1e-5 (atan2+ 2 -3 6) 2.5535900500422)
(expect± 0.1e-5 (atan2+ 3 -2 6) 2.1587989303425)
(expect± 0.12e-6 (atan2+ -2 3 6) -0.58800260354757)
(expect± 0.1e-6 (atan2+ -3 2 6) -0.98279372324733)
(expect± 0.1e-5 (atan2+ -2 -3 6) -2.5535900500422)
(expect± 0.1e-5 (atan2+ -3 -2 6) -2.1587989303425)

(expect± 0.1e-15 (atan2+ 1 1000 12) 0.00099999966666687)
(expect± 0.1e-11 (atan2+ 1000 1 12) 1.5697963271282)

;; atan

(expect "NaN" (atan "x"))
(expect "NaN" (atan nil))
(expect "NaN:PREC" (atan 1 "x"))

(define (atan+ x prec)
  (round-to (prec+ prec 4) (atan x prec)))

(expect± 0.1e-5 (atan+ 0.1 6) 0.099668652491162)
(expect± 0.1e-5 (atan+ 0.9 6) 0.73281510178651)
(expect± 0.1e-5 (atan+ 4 6) 1.325817663668)
