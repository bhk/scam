(require "core.scm")
(require "math0.scm" &private)

;;--------------------------------
;; Utilities
;;--------------------------------

(define `U18 (concat U9 T9))
(define `U27 (concat U18 T9))

(define (repeat-words lst len)
  (if (subst 0 nil len)
      (if (word len lst)
          (wordlist 1 len lst)
          (repeat-words (concat lst " " lst) len))))

;; Decimal digits to U:  (U 123) --> "01 011 0111"
;;
(define (U d)
  (subst "A" U10 "I" U18 "R" U27 (d2u d)))

;; Decimal digits to UV:  (UV "123") --> "01 011 0111"
;;
(define (UV d)
  (u2uv (U d)))

;; Digit sizes to UV:  (UD 1 2) --> "01 011"
;;
(define (UD ...values)
  (for n values
       (concat 0 (smash (repeat-words "1 1 1 1 1 1" n)))))


;;--------------------------------
;; Tests
;;--------------------------------

;; u2d & d2u

(foreach n "0 1 2 3 4 5 6 7 8 9"
         (define `num (concat 0 n 01 n 12 n 23 n 34 n 45 n 56 n 67 n 78 n 9 n 9))
         (expect num (u2d (d2u num))))

(expect 761 (u2d (d2u 761)))

;; n>0?

(expect nil (n>0? 0))
(expect nil (n>0? "-0"))
(expect 11 (n>0? "011"))
(expect nil (n>0? "-01"))
(expect nil (n>0? "-3"))
(expect 3 (n>0? "3"))

;; u<0?

(expect nil (u<0? 0))
(expect nil (u<0? -0))
(assert (u<0? -01))

;; 0-

(expect -123 (0- 123))
(expect 123 (0- -123))

;; uf-get-lz

(expect 0 (uf-get-lz 0))
(expect nil (uf-get-lz (UV 123)))
(expect 0 (uf-get-lz (UV 0123)))
(expect "0 0" (uf-get-lz (UV 00123)))

;; uf-trim-lz

(expect nil (uf-trim-lz 0))
(expect (UV 123) (uf-trim-lz (UV 123)))
(expect (UV 123) (uf-trim-lz (UV 0123)))
(expect (UV 123) (uf-trim-lz (UV 00123)))

;; u-carry

;; overflow should produce a mal-formed result
(expect (concat 1 (U "00")) (u-carry (U "9A")))
(for n [nil 9 99 999 9999 99999 999999]
     (expect (U (concat "1" (subst 9 0 n) "0"))
             (u-carry (U (concat "0" n "A")))))
(expect (U 10007) (u-carry (U "0998R")))
(expect (U 1207) (u-carry (U "0AIR")))

;; uf-carry

(expect (UV  99) (uf-carry (UV 99)))
(expect (UV  10) (uf-carry (UV "0A")))
(expect (UV 100) (uf-carry (UV "09A")))
;; overflow produces modulo-1 result
(expect (UV   1) (uf-carry (concat (UV 9) "11")))

;; uf-add

(expect (UV 23) (uf-add (UV 1) (UV 13)))
(expect (UV 10) (uf-add (UV 00) (UV 10)))
(expect (UV 02) (uf-add (UV 89) (UV 13)))
(expect (UV 00) (uf-add (UV 99) (UV 01)))
(expect (UV 10) (uf-add (UV 99) (UV 11)))
(expect (UV 1000) (uf-add (UV 0990) (UV 001)))

;; u-norm-uns

(expect (U 0)      (u-norm-uns (U 0)))
(expect (U 0)      (u-norm-uns (U 00)))
(expect (U 1)      (u-norm-uns (U 1)))
(expect (U 10)     (u-norm-uns (U 10)))
(expect (U 1)      (u-norm-uns (U 01)))
(expect (U 100300) (u-norm-uns (U 00100300)))

;; uv-complement-digits

(expect (UV 8765432109) (uv-complement-digits (UV 1234567890)))

;; u-sub-unsigned

(expect (U 13) (u-sub-unsigned (U 995) (U 982)))
(expect (U -13) (u-sub-unsigned (U 982) (U 995)))
(expect (U 0) (u-sub-unsigned (U 99) (U 99)))

;; u-add-unsigned

(expect (U 123) (u-add-unsigned (U 110) (U 13)))
(expect (U 0) (u-add-unsigned (U 0) (U 0)))
(expect (U 123) (u-add-unsigned (U 20) (U 00103)))

;; u-add

(expect (U -23) (u-add (U -20) (U -3)))
(expect (U 10) (u-add (U -20) (U 30)))
(expect (U 17) (u-add (U 20) (U -3)))
(expect (U 0) (u-add (U -0) (U -0)))
(expect (U 5) (u-add (U 2) (U 3)))

(expect (U 0) (u-add (U 2) (U -2)))
(expect (U 0) (u-add (U -2) (U 2)))

;; u-sub

(expect (U 3) (u-sub (U 2) (U -1)))

;; uf-sub

(expect (UV 88) (uf-sub (UV 01) (UV 13)))
(expect (UV 90) (uf-sub (UV 00) (UV 10)))
(expect (UV 68) (uf-sub (UV 89) (UV 21)))

;; cmp-reduce

(for n "0 1 2 3 4 5 6 7 8 9"
     (define `ones (smash (wordlist 1 n "1 1 1 1 1 1 1 1 1")))
     (define `neg (subst 1 "~" ones))
     (expect nil (cmp-reduce (concat ones 0 neg)))
     (expect "~" (cmp-reduce (concat ones 0 neg "~")))
     (expect "1" (cmp-reduce (concat 1 ones 0 neg))))

;; u-cmp & u-cmp-unsigned

(define (cmp-rubric ret)
  (cond
   ((filter "1%" ret) ">")
   ((filter "~%" ret) "<")
   ((not ret) "=")
   (else (concat "?: " ret))))

(define (u-cmp-redux a b)
  (cmp-rubric (u-cmp (U a) (U b))))

;; non-negative cases (u-cmp-unsigned)
(expect ">" (u-cmp-redux 10 2))
(expect "<" (u-cmp-redux 2 10))
(expect ">" (u-cmp-redux 0140 139))
(expect "<" (u-cmp-redux 0139 140))
(expect "=" (u-cmp-redux 00345 345))
;; u-cmp coverage
(expect "<" (u-cmp-redux -3 -2))
(expect "<" (u-cmp-redux -3 2))
(expect ">" (u-cmp-redux 2 -3))
(expect "=" (u-cmp-redux -0 -0))

;; u-lt?

(expect 1 (u-lt? U1 U9))
(expect nil (u-lt? U1 U1))

;; uf-cmp

(define (uf-cmp-redux a b)
  (cmp-rubric (uf-cmp (UV a) (UV b))))

(expect "<" (uf-cmp-redux "123" "124"))
(expect "=" (uf-cmp-redux "124" "124"))
(expect ">" (uf-cmp-redux "1245" "124"))
(expect "<" (uf-cmp-redux "1235" "124"))

;; uf-lt?

(expect 1 (uf-lt? (UV 19) (UV 30)))
(expect nil (uf-lt? (UV 12) (UV 12)))

;; uf-sign-sub

;; A<B
(expect (UV -79) (uf-sign-sub (UV 1) (UV 89) nil))  ;; 0.10 - 0.89 = -0.79
(expect (UV "+79") (uf-sign-sub (UV 1) (UV 89) "-"))
;; A>B
(expect (UV "+211") (uf-sign-sub (UV 34) (UV 129) nil))
(expect (UV "-1") (uf-sign-sub (UV 2) (UV 1) "-"))
;; A==B
(expect (UV "+ 0 0") (uf-sign-sub (UV 89) (UV 89) nil))
(expect (UV "- 0 0") (uf-sign-sub (UV 89) (UV 89) "-"))

;; u-1

(expect (U 0) (u-1 (U 1)))
(expect (U 9) (u-1 (U 10)))
(expect (U -1) (u-1 (U 0)))
(expect (U -2) (u-1 (U -1)))
(expect (U -100) (u-1 (U -99)))

;; u+1

(expect (U 10) (u+1 U9))
(expect U1 (u+1 0))
(expect 0 (u+1 -1))
(expect (U -8) (u+1 (U -9)))
(expect (U -9) (u+1 -10))

;; u-carry-all

(define T30 "111111111111111111111111111111")
(expect (U -120) (u-carry-all (concat "-0" (subst 1 1111 T30))))

;; u-add-ones

(expect (U 1) (u-add-ones 0 "1"))
(expect (U 9) (u-add-ones 0 T9))
(expect (U 10) (u-add-ones 0 (concat T9 1)))
(expect (U 2) (u-add-ones "01" "1"))
(expect (U 10) (u-add-ones U9 "1"))
(expect 0 (u-add-ones "-01" "1"))
(expect (U -8) (u-add-ones (U -9) "1"))
(expect (U 28) (u-add-ones (U -2) T30))
(expect (U 38) (u-add-ones (U 8) T30))


;;----------------------------------------------------------------
;; Multiplication
;;----------------------------------------------------------------

;; uf-carry_...

(expect (UV "1A9") (uf-carry_19_10 (UD 0 19 19)))
(expect (UD 0 11 27 11 25) (uf-carry_45_27 (UD 0 9 45 29 45)))
(expect (UD 1 7 9 1) (uf-carry_81_10 (UD 0 9 81 81)))

;; uf-add-x

(expect (UV 20113) (uf-add-x (UV 10012) (UV 10101)))

;; merge-and-carry

(expect (UV 04001) (merge-and-carry "0 0010011001 00000 0 010"))

;; longer?

(expect 9 (longer? "9 9 9" "9 9"))
(expect nil (longer? "9 9" "9 9"))
(expect 1 (longer? 1 nil))
(expect nil (longer? nil nil))

;; uf*digit

(expect (UV 12) (uf*digit (UV 3) (UV 4)))
(expect (UV 042) (uf*digit (UV 14) (UV 3)))
(expect (UV 1002) (uf*digit (UV 334) (UV 3)))

;; uf*CONSTANT

(expect (UV 012345678901) (uf*0.1 (UV 12345678901)))
(expect (UV 024691357802) (uf*0.2 (UV 12345678901)))
(expect (UV 049382715604) (uf*0.4 (UV 12345678901)))
(expect (UV 061728394505) (uf*0.5 (UV 12345678901)))
(expect (UV "037036A36703") (uf*0.3_10 (UV 12345678901)))
(expect (UV 086419752307) (uf*0.7_10 (UV 12345678901)))
(expect (UV 111111110109) (uf*0.9_10 (UV 12345678901)))

;; uf-mul-fixed

(define (uf-mul-fixed-fn a b)
  (uf-mul-fixed a b))
(fix-native-var (native-name uf-mul-fixed-fn))

(define (umf a b)
  (u2d (smash (uf-carry (uf-mul-fixed-fn (UV a) (UV b))))))

(expect 0246 (umf 123 2))
(expect 02583 (umf 123 21))
(expect 039483 (umf 123 321))
(expect 30247 (umf 7 4321))
(expect 380247 (umf 7 54321))
(expect 4580247 (umf 7 654321))
(expect 53580247 (umf 7 7654321))
(expect 613580247 (umf 7 87654321))

;; uf-mul

;; This is a simple reference implementation for uf-mul.
(define (uf-mul-ref a b)
  (if (word 2 b)
      (uf-add (uf*digit a (word 1 b))
              (>>1 (uf-mul-ref a (rest b))))
      (uf*digit a b)))

(define (uf-mul-check a b)
  (expect (uf-mul-ref a b) (uf-mul a b)))

(define (uf-of-len len)
  (d2u (wordlist 1 len "1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 1 2 4 5 6 7 8 9")))

(foreach len-b "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19"
         (foreach len-a [1 2 len-b (1+ (1+ len-b))]
                  (uf-mul-check (uf-of-len len-a) (uf-of-len len-b))
                  (uf-mul-check (uf-of-len len-a) (concat (uf-of-len len-b)
                                                          " 0 0 0 0 0 0"))))

;;------------------------------------------------------------------------
;; Division
;;------------------------------------------------------------------------

;; guess-digit

(expect (UV 6) (guess-digit (UV 024) "1111"))
(expect (UV 6) (guess-digit (UV 072) "111111111111"))   ;; bhi = 12
(expect (UV 5) (guess-digit (UV 071) "111111111111"))   ;; bhi = 12
(expect (UV 9) (guess-digit (UV 121) "1111111111111"))  ;; bhi = 13
(expect (UV 0) (guess-digit nil "1"))
;; max is 9
(expect (UV 9) (guess-digit (UV 121) "111111111111"))
;; Numbers are left-aligned
(expect (UV 5) (guess-digit (UV 030) "111111"))

;; uf/2

(expect "0 011111" (uf/2 "01 0"))
(expect "0 0111111" (uf/2 "01 011"))
(expect "01" (uf/2 "01"))

;; uf*3

(expect (UV 370370367) (uf*3 (UV 123456789)))

;; uf*0.25

(expect (UV 03086419725) (uf*0.25 (UV 123456789)))

;; uf*1.5

(expect (UV 1851851835) (uf*1.5 (UV 123456789)))

;; power-seq-1
(expect (UV 1371)                           ;; 0.1234 * 1.111
        (power-seq-1 (UV 1234) "0" 2 3))    ;; MIN=4 => 3+len(B) = 3 digits

(expect (UV 13711111)                       ;; 0.1234 * 1.1111111
        (power-seq-1 (UV 1234) "0" 3 3))    ;; MIN=5 => 4+len(B) = 4 digits


;; uf-round

(expect (UV 00) (uf-round 1 DIV-NEAREST (UV 05)))
(expect (UV 01) (uf-round 1 DIV-CEILING (UV 05)))
(expect (UV 01) (uf-round 1 DIV-NEAREST (UV 051)))
(expect (UV 00) (uf-round 1 DIV-TRUNCATE (UV 051)))

;; recip2

(expect (UV 9) (recip2 (UV 11)))
(expect (UV 9) (recip2 (UV 11)))
(expect (UV 76923) (recip2 (UV 13)))
(expect (UV 12987) (recip2 (UV 77)))
(expect (UV 1) (recip2 (UV 99)))

;; uf-div

;; trailing zero
(expect (uf-div (UV 123) (UV 30) 4 DIV-NEAREST)
        (uf-div (UV 123) (UV 3) 4 DIV-NEAREST))
;; zero-length result?
(expect (UV 0) (uf-div (UV 2) (UV 4) 0 DIV-TRUNCATE))
(expect (UV 2) (uf-div (UV 2) (UV 4) 0 DIV-REMAINDER))
;; zero dividend?
(expect 0 (uf-div (UV 0) (UV 4) 4 DIV-TRUNCATE))
;; div-long case
(expect (UV 06942) (uf-div (UV 3) (UV 4321) 4 DIV-TRUNCATE))
(expect (UV 3618) (uf-div (UV 3) (UV 4321) 4 DIV-REMAINDER))
(expect (UV 08) (uf-div (UV 16665) (UV 2222) 1 DIV-NEAREST))   ;; 7.5 --> 8
(expect (UV 06) (uf-div (UV 14443) (UV 2222) 1 DIV-NEAREST))   ;; 6.5 --> 6
(expect (UV 07) (uf-div (UV 1444301) (UV 2222) 1 DIV-NEAREST)) ;; 6.5x --> 7
(expect (UV 10) (uf-div (UV 21109) (UV 2222) 1 DIV-NEAREST))   ;; 9.5 --> 10
(expect (UV 06) (uf-div (UV 13332) (UV 2222) 1 DIV-CEILING))
(expect (UV 07) (uf-div (UV 1333201) (UV 2222) 1 DIV-CEILING))
;; uf-remainder case
(expect (UV 29) (uf-div (UV 3) (UV 41) 3 DIV-REMAINDER))
;; div3 case
(expect (UV 06944) (uf-div (UV 3) (UV 432) 4 DIV-TRUNCATE))
(expect (UV 08) (uf-div (UV 1665) (UV 222) 1 DIV-NEAREST))   ;; 7.5 --> 8
(expect (UV 06) (uf-div (UV 1443) (UV 222) 1 DIV-NEAREST))   ;; 6.5 --> 6
(expect (UV 07) (uf-div (UV 144301) (UV 222) 1 DIV-NEAREST)) ;; 6.5x --> 7
(expect (UV 10) (uf-div (UV 2109) (UV 222) 1 DIV-NEAREST))   ;; 9.5 --> 10
(expect (UV 06) (uf-div (UV 1332) (UV 222) 1 DIV-CEILING))
(expect (UV 07) (uf-div (UV 133201) (UV 222) 1 DIV-CEILING))
;; div2 case
(expect (UV 06976) (uf-div (UV 3) (UV 43) 4 DIV-TRUNCATE))
(expect (UV 08) (uf-div (UV 165) (UV 22) 1 DIV-NEAREST))     ;; 7.5 --> 8
(expect (UV 06) (uf-div (UV 143) (UV 22) 1 DIV-NEAREST))     ;; 6.5 --> 6
(expect (UV 07) (uf-div (UV 14301) (UV 22) 1 DIV-NEAREST))   ;; 6.5x --> 7
(expect (UV 10) (uf-div (UV 209) (UV 22) 1 DIV-NEAREST))     ;; 9.5 --> 10
(expect (UV 06) (uf-div (UV 132) (UV 22) 1 DIV-CEILING))
(expect (UV 07) (uf-div (UV 13201) (UV 22) 1 DIV-CEILING))
;; div1 case
(expect (UV 04285) (uf-div (UV 3) (UV 7) 4 DIV-TRUNCATE))
(expect (UV 08) (uf-div (UV 15) (UV 2) 1 DIV-NEAREST))       ;; 7.5 --> 8
(expect (UV 06) (uf-div (UV 13) (UV 2) 1 DIV-NEAREST))       ;; 6.5 --> 6
(expect (UV 07) (uf-div (UV 13001) (UV 2) 1 DIV-NEAREST))    ;; 6.5x --> 7
(expect (UV 10) (uf-div (UV 19) (UV 2) 1 DIV-NEAREST))       ;; 9.5 --> 10
(expect (UV 06) (uf-div (UV 12) (UV 2) 1 DIV-CEILING))
(expect (UV 07) (uf-div (UV 1201) (UV 2) 1 DIV-CEILING))
