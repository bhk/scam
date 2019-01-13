(require "core.scm")
(require "mcore.scm" &private)
(require "math.scm" &private)
(require "mcore-testutils.scm" &private)


;; uf-lt?

(expect 1 (uf-lt? (UV 19) (UV 30)))
(expect nil (uf-lt? (UV 12) (UV 12)))

;; u-lt?

(expect 1 (u-lt? U1 U9))
(expect nil (u-lt? U1 U1))

;; u>0?

(expect nil (u>0? 0))
(expect 11 (u>0? "011"))
(expect nil (u>0? "-01"))

;; u<0?

(expect nil (u<0? 0))
(expect nil (u<0? -0))
(assert (u<0? -01))

;;----------------------------------------------------------------
;; FP operations
;;----------------------------------------------------------------


;; fp>0?

(expect nil (fp>0? "0 + 0"))
(expect nil (fp>0? "0 - 0 01"))
(assert (fp>0? "0 + 01"))
(assert (fp>0? "0 + 0 0 0 011"))

;; fp<0?
(assert (fp<0? "0 - 0 01"))
(expect nil (fp<0? "01 - 0"))

;; fp!=0?

(expect nil (fp!=0? "0 + 0"))
(assert (fp!=0? "0 - 0 01"))
(assert (fp!=0? "0 + 0 0 0 011"))

;; pad

(expect "0 0 " (pad "0011"))
(expect "0 0 0 0 0 0 0 0 0 0 0 " (pad "00101"))

;; fp-negate

(expect "01 - 0" (fp-negate "01 + 0"))
(expect "01 + 0" (fp-negate "01 - 0"))
(expect "-01 + 0" (fp-negate "-01 - 0"))

;; fp-add

;; same sign; pad empty
(expect (FP 3) (fp-norm (fp-add (FP 1) (FP 2))))
;; same sign; pad non-empty
(expect (FP 12) (fp-norm (fp-add (FP 10) (FP 2))))
(expect (FP 10.21) (fp-norm (fp-add (FP 10) (FP 0.21))))
;; different sign; pad non-empty
(expect (FP -7) (fp-norm (fp-add (FP -10) (FP 3))))
(expect nil (fp-add (FP 1) nil))
(expect nil (fp-add nil (FP 1)))

;; fp-sub

(expect (DFP 2 08) (fp-sub (FP 10) (FP 2)))
(expect (FP 12) (fp-norm (fp-sub (FP 10) (FP -2))))
(expect nil (fp-sub (FP 1) nil))
(expect nil (fp-sub nil (FP 1)))

;; sign-mul

(expect "-" (sign-mul "+" "-"))
(expect "-" (sign-mul "-" "+"))
(expect "+" (sign-mul "+" "+"))
(expect "+" (sign-mul "-" "-"))

;; fp-mul

(expect (FP -124) (fp-mul (FP 31) (FP -4)))
(expect nil (fp-mul nil (FP 4)))
(expect nil (fp-mul (FP 4) nil))

;; fp-div

(define `NEAR 1)
(define `DOWN nil)
(define (fp-div-N a b p r)
  (fp-norm (fp-div a b p r)))

(define (fp-div-d a b p r)
  (fp2d (fp-norm (fp-div a b p r))))

;; cond: (not "AF and BF begin with non-zero digit")
;; B nil
(expect nil (fp-div (FP 1) nil 8 NEAR))
(expect nil (fp-div (FP 0) nil 8 NEAR))
;; B zero
(expect nil (fp-div (FP 1) "011 + 0" 4 NEAR))
;; B non-normalized
(expect 2 (fp-div-d (FP 4) "011 + 0 011" 4 NEAR))
;; A nil
(expect nil (fp-div nil (FP 1) 8 NEAR))
;; A zero
(expect "0 + 0" (fp-div "011 + 0" (FP 1) 4 NEAR))
;; A non-normalized
(expect 2 (fp-div-d "011 + 0 011" (FP 1) 4 NEAR))
;; cond: prec
;; af<bf vs. af>=bf
(expect (FP 0.4) (fp-div (FP 2) (FP 5) 1 NEAR))
(expect (FP 1.2) (fp-div (FP 6) (FP 5) 2 NEAR))
;; prec: PLACE vs. DIGITS
;; mode: NEAREST vs. TRUNCATE vs. CEILING
(expect 6      (fp-div-d (FP 20)  (FP 3) 1      DOWN))
(expect 6.66   (fp-div-d (FP 20)  (FP 3) 3      DOWN))
(expect 6.67   (fp-div-d (FP 20)  (FP 3) 3      NEAR))
(expect 6.66   (fp-div-d (FP 20)  (FP 3) (U 2)  DOWN))
(expect 6.6    (fp-div-d (FP 20)  (FP 3) (U 1)  DOWN))
(expect 7      (fp-div-d (FP 20)  (FP 3) (U 0)  NEAR))
(expect 10     (fp-div-d (FP 20)  (FP 3) (U -1) NEAR))
(expect 0      (fp-div-d (FP 20)  (FP 3) (U -1) DOWN))
(expect "0 + 0"  (fp-div (FP 20)  (FP 3) (U -2) DOWN))
(expect -100   (fp-div-d (FP -20) (FP 3) (U -2) DOWN))  ;; CEILING
(expect -1000  (fp-div-d (FP -20) (FP 3) (U -3) DOWN))  ;; CEILING
;; round(AF/BF) == 1
(expect 100    (fp-div-d (FP 199) (FP 2) 2      NEAR))
;; cond: [else]
(expect nil    (fp-div (FP 3) (FP 2) nil DOWN))

;; fp-mod

(expect nil (fp-mod nil (FP 1)))
(expect nil (fp-mod (FP 1) nil))
(expect nil (fp-mod (FP 1) "01 + 0"))
;; normalization
(expect (FP 2) (fp-mod (FP 8) "011 + 0 0111"))
(expect (FP 2) (fp-mod "011 + 0 011111111" "01 + 0111"))
;; signs differ
(expect (FP 2) (fp-mod (FP 5) (FP 3)))
(expect (FP -1) (fp-mod (FP 5) (FP -3)))
(expect (FP -2) (fp-mod (FP -5) (FP -3)))
(expect (FP 1) (fp-mod (FP -5) (FP 3)))
;; ae < be
(expect (FP 3) (fp-mod (FP 23) (FP 4)))
;; else
(expect 0.72 (fp2d (fp-mod (FP 45) (FP 1.23))))
(expect 2.88 (fp2d (fp-mod (FP 12) (FP 4.56))))

;; fp-round

;; significant digits
(expect (FP 12e1) (fp-round "0111 + 01 011 0111" 2 DIV-NEAREST))
(expect (FP 0.0012) (fp-round "-011 + 01 011 0111" 2 DIV-NEAREST))
;; decimal place
(expect (FP 123) (fp-round "0111 + 01 011 0111 01111 0111 01" 0 DIV-NEAREST))
(expect (FP 12e1) (fp-round "0111 + 01 011 0111 01111 0111 01" -01 DIV-NEAREST))
(expect (FP 123.4) (fp-round "0111 + 01 011 0111 01111 0111 01" 01 DIV-NEAREST))
;; not normalized
(expect (FP 12e1) (fp-round "01111 + 0 01 011 0111" 2 DIV-NEAREST))
(expect "0 + 0" (fp-round "01111 + 0" 2 DIV-NEAREST))
;; exponent with large magnitude
(expect "0 + 0" (fp-round (FP 1e-999999999) 0111 DIV-NEAREST))

;; fp-cmp

(define (fp-cmp-rubric ret)
  (cond
   ((not ret) "=")
   ((findstring "1" ret) ">")
   (else "<")))


;; positive
(expect ">" (fp-cmp-rubric (fp-cmp (FP 20) (FP 5))))
(expect "<" (fp-cmp-rubric (fp-cmp (FP 20) (FP 50))))
(expect "=" (fp-cmp-rubric (fp-cmp (FP 20) (FP 20))))
;; nil/NaN
(expect ">" (fp-cmp-rubric (fp-cmp (FP -1) nil)))
(expect "<" (fp-cmp-rubric (fp-cmp nil (FP -1))))
(expect "=" (fp-cmp-rubric (fp-cmp nil nil)))
;; A=0
(expect "<" (fp-cmp-rubric (fp-cmp (FP 0) (FP 1))))
(expect "=" (fp-cmp-rubric (fp-cmp (FP 0) (FP -0))))
(expect ">" (fp-cmp-rubric (fp-cmp (FP 0) (FP -1))))
;; B=0
(expect ">" (fp-cmp-rubric (fp-cmp (FP 1) (FP 0))))
(expect "<" (fp-cmp-rubric (fp-cmp (FP -1) (FP 0))))
;; A<0
(expect "<" (fp-cmp-rubric (fp-cmp (FP -1) (FP 0))))
(expect ">" (fp-cmp-rubric (fp-cmp (FP -1) (FP -2))))
;; else
(expect ">" (fp-cmp-rubric (fp-cmp (FP 1) (FP -2))))

;; fp-sq

(expect 400 (fp2d (fp-sq (FP 20))))

;; fp-pwr

(expect 1     (fp2d (fp-pwr (FP 2) (U 0))))
(expect 1     (fp2d (fp-pwr (FP -2) (U 0))))
(expect 2     (fp2d (fp-pwr (FP 2) (U 1))))
(expect 4     (fp2d (fp-pwr (FP -2) (U 2))))
(expect -2048 (fp2d (fp-pwr (FP -2) (U 11))))

;;----------------------------------------------------------------
;; Binop
;;----------------------------------------------------------------

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

;; prec-to-pod

(expect 16 (prec-to-pod nil))
;; +N -N
(expect "011" (prec-to-pod "-2"))
(expect "-011" (prec-to-pod "+2"))
(expect "-0" (prec-to-pod "+0"))
(expect nil (prec-to-pod " -1"))
(expect nil (prec-to-pod "-1-"))
(expect (U "-91") (prec-to-pod "+91"))
(expect (U "91") (prec-to-pod "-91"))
;; word-index?
(expect 1 (prec-to-pod 1))
(expect 1 (prec-to-pod "001"))
(expect 91 (prec-to-pod 91))
(expect 40 (prec-to-pod 40))
(expect nil (prec-to-pod 0))
(expect nil (prec-to-pod " 01"))

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

;;----------------------------------------------------------------
;; Misc.
;;----------------------------------------------------------------

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

;;----------------------------------------------------------------
;; range
;;----------------------------------------------------------------

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

;;----------------------------------------------------------------
;; format-fixed
;;----------------------------------------------------------------

;; uf-fix

(expect (UV 12300) (uf-fix (UV 123) 5))
(expect (UV 123) (uf-fix (UV 123) 3))
(expect (UV 12) (uf-fix (UV 123) 2))
(expect nil (uf-fix (UV 123) 0))
(expect nil (uf-fix (UV 123) nil))

;; zero-pad

(expect (UV 00123) (zero-pad (UV 123) 5))
(expect (UV 123) (zero-pad (UV 123) 3))
(expect (UV 123) (zero-pad (UV 123) 2))
(expect (UV 123) (zero-pad (UV 123) 0))
(expect (UV 123) (zero-pad (UV 123) nil))

;; ltrimz

(expect (U "  123") (ltrimz (U "00123")))
(expect (U " nan") (ltrimz (U "0nan")))
(expect (U " -1") (ltrimz (U "0-1")))
(expect (U "  0") (ltrimz (U "000")))

;; fp-fix

(define (ff xd wd pd)
  (fp-fix (FP xd) wd pd (d2u pd)))

;; left of decimal: exp>0
(expect (ff 123.45 nil nil) "123")        ;; wordlist
(expect (ff 1e2 nil nil)    "100")        ;; rpad left of decimal
;; left of decimal: exp<=0
(expect (ff 1.23e-2 nil nil) "0")
;; right of decimal: exp>0
(expect (ff 1e2 nil 2)      "100.00")     ;; padding to right
(expect (ff 123.456 nil 2)  "123.45")     ;; trim digits to right
(expect (ff 1e-99999999 nil 2)  "0.00")   ;; big -E
;; right of decimal: exp<=0
(expect (ff 1.23e-2 nil nil) "0")
(expect (ff 1.23e-2 nil 3)   "0.012")
(expect (ff 1.23e-1 nil 3)   "0.123")
(expect (ff 1.23e-2 nil 5)   "0.01230")
;; sign
(expect (ff -1.23 5 2) "-1.23")
(expect (ff -1.23 7 2) "  -1.23")
;; w>len
(expect (ff 1.23 5 2)     " 1.23")
(expect (ff 1.23 5 nil)   "    1")
;; extra zeros
(expect (ff "0001" nil nil)   "1")

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


;;----------------------------------------------------------------
;; num-lex, num-sort
;;----------------------------------------------------------------

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
