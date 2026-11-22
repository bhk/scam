(require "core.scm")
(require "math0.scm")
(require "math0-q.scm" &private)
(require "math1.scm" &private)

;;--------------------------------
;; Utilities
;;--------------------------------

(define (FP n)
  (u2fp (U n)))

;; Construct an FP number from decimal without normalizing
;;
(define (DFP exp m)
  (make-fp (U exp)
           (or (findstring "-" m) "+")
           (UV (subst "-" nil m))))


;;--------------------------------
;; Tests
;;--------------------------------

;; non-digit

(assert (non-digit? " "))
(assert (non-digit? 2))
(expect nil (non-digit? "01"))

;; u-begins, u-rm-prefix, u-rm-suffix

(assert (u-begins? "011" "0111"))
(expect "  abc " (u-rm-prefix " 0 0 0 " " 0 0 0   abc "))
(expect "  abc " (u-rm-suffix " 0 0 0 " "  abc  0 0 0 "))

;; fp-norm

(expect "01 + 01"    (fp-norm "011 + 0 01"))
(expect "-0111 + 01" (fp-norm "-011 + 0 01"))
(expect "0 + 01"     (fp-norm "01 + 0 01"))
(expect "-011 + 01"  (fp-norm "-01 + 0 01"))
(expect "-0111 - 01" (fp-norm "01 - 0 0 0 0 01"))
(expect "-0100 - 01" (fp-norm (.. "01 - " (repeat-words 0 101) " 01")))

;; u2fp

;; cond: (not (non-digit? u))
(expect (FP nil) nil)
(expect (FP 1) "01 + 01")
(expect (FP 11) "011 + 01 01")
(expect (FP 01) "01 + 01")
(expect (FP 00) "0 + 0")
;; (word 10 (spread u))
(expect (FP (.. 1 (smash (repeat-words 0 200))))
        (.. "011001 + 01 " (repeat-words 0 200)))
(expect (FP (.. "0." (smash (repeat-words 0 200)) "1"))
        (.. "-01100 + 01"))
;; cond: (findstring "E" (subst "e" "E" u))
(expect (FP "1E1") "011 + 01")
(expect (FP "1e1") "011 + 01")
(expect (FP "1e1 ") nil)
(expect (FP "1e 1") nil)
(expect (FP "1E") nil)
(expect (FP "E1") nil)
(expect (FP "1e+1") "011 + 01")
(expect (FP "1e++1") nil)
(expect (FP "1e+21") "011011 + 01")
(expect (FP "1e-1") "0 + 01")
(expect (FP "1e+-1") nil)
(expect (FP "1ee1") nil)
(expect (FP "1e+e3") nil)
;; cond: (u-begins? "-0")
(expect (FP "-1") "01 - 01")
(expect (FP "--1") nil)
;; cond: (findstring "." u)
(expect (FP "1.") nil)
(expect (FP "1.x") nil)
(expect (FP ".1") nil)
(expect (FP "1.2") "01 + 01 011")
(expect (FP "1.2.2") nil)
;; misc.
(expect (FP "+1") nil)
(expect (FP -12.34e-3) "-01 - 01 011 0111 01111")
;; disallow extraneous spaces
(for (n [1 -1 "1.2" "1e3" "1.2e3" "1.2e+3" "1.2e-3" ])
  (expect nil (FP (.. " " n)))
  (expect nil (FP (.. n " "))))

;; fp2d & fp2u

;; cond: (findstring ... "0")
(expect 0.2 (fp2d "01 + 0 011"))
(expect 0.02 (fp2d "0 + 0 011"))
(expect 20 (fp2d "0111 + 0 011"))
(expect 0 (fp2d "0111 + 0 0 0"))
;; cond: (findstring 1 frac)
;; if: EXP in -5..21
(expect 0.000001 (fp2d (FP 1e-6)))
(expect 100000000000000000000 (fp2d (FP 1e20)))
(expect 123 (fp2d (make-fp (U 3) "+" (UV 123))))
(expect 120 (fp2d (make-fp (U 3) "+" (UV 12000))))
(expect 123.4 (fp2d (make-fp (U 3) "+" (UV 1234))))
;; if EXP not in -5..21
(expect 1e-7 (fp2d (FP 1e-7)))
(expect 9.99e-7 (fp2d (FP 9.99e-7)))
(expect 1e+21 (fp2d (FP 1e21)))
(expect 1e+99 (fp2d (make-fp (U 100) "+" (UV 1))))
(expect 1.23e+100 (fp2d (make-fp (U 101) "+" (UV 123000))))
;; cond: fp & else
(expect "NaN" (fp2d nil))
(expect 0 (fp2d "-011 + 0 0 0 0"))
(expect 0 (fp2d "-011 - 0 0 0 0"))


;;--------------------------------
;; tally
;;--------------------------------


(expect "111" (tally (UV 3)))
(expect T10 (tally (UV 10)))
(expect 123 (words (subst 1 " 1" (tally (UV 123)))))


;; u-zeros

(expect " 0 0 0" (u-zeros "0111"))
(expect " 0" (u-zeros "0001"))
(expect nil (u-zeros "-0001"))
(expect " 0 0 0 0 0 0 0 0 0 0" (u-zeros "010"))

;;--------------------------------
;; PREC and POD
;;--------------------------------

;; prec-to-pod

(expect 16 (prec-to-pod nil))
;; +N, -N, 0
(expect "011" (prec-to-pod "-2"))
(expect "-011" (prec-to-pod "+2"))
(expect "-0" (prec-to-pod "+0"))
(expect -0 (prec-to-pod "-0"))
(expect (U "-91") (prec-to-pod "+91"))
(expect (U "91") (prec-to-pod "-91"))
(expect 0 (prec-to-pod "0"))
(expect 0 (prec-to-pod "00"))
;; word-index?
(expect 2 (prec-to-pod 2))
(expect 2 (prec-to-pod "002"))
(expect 91 (prec-to-pod 91))
(expect 40 (prec-to-pod 40))
;; garbage
(expect nil (prec-to-pod "+-0"))
(expect nil (prec-to-pod "-+0"))
(expect nil (prec-to-pod " 0"))
(expect nil (prec-to-pod "0 "))
(expect nil (prec-to-pod "4 "))
(expect nil (prec-to-pod "+"))
(expect nil (prec-to-pod "0+"))
(expect nil (prec-to-pod "0-"))
(expect nil (prec-to-pod " -1"))
(expect nil (prec-to-pod "-1-"))


;;--------------------------------
;; FP operations
;;--------------------------------

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

;; f+

;; same sign; pad empty
(expect (FP 3) (fp-norm (f+ (FP 1) (FP 2))))
;; same sign; pad non-empty
(expect (FP 12) (fp-norm (f+ (FP 10) (FP 2))))
(expect (FP 10.21) (fp-norm (f+ (FP 10) (FP 0.21))))
;; different sign; pad non-empty
(expect (FP -7) (fp-norm (f+ (FP -10) (FP 3))))
(expect nil (f+ (FP 1) nil))
(expect nil (f+ nil (FP 1)))

;; f-

(expect (DFP 2 08) (f- (FP 10) (FP 2)))
(expect (FP 12) (fp-norm (f- (FP 10) (FP -2))))
(expect nil (f- (FP 1) nil))
(expect nil (f- nil (FP 1)))

;; sign-mul

(expect "-" (sign-mul "+" "-"))
(expect "-" (sign-mul "-" "+"))
(expect "+" (sign-mul "+" "+"))
(expect "+" (sign-mul "-" "-"))

;; f*

(expect (FP -124) (f* (FP 31) (FP -4)))
(expect nil (f* nil (FP 4)))
(expect nil (f* (FP 4) nil))

;; f*~

(define (d*~ x y p)
  (fp2d (f*~ (d2fp x) (d2fp y) (prec-to-pod p))))

;; 12 * 12 = 0144    MSD from uv-mul is zero
;; 24 * 24 = 0576    MSD zero, but next digit rounds to MSD
;; 32 * 32 = 1024    MSD non-zero
;; 99 * 99 = 9801    MSD non-zero, rounds up to next place

;; PREC as place

(expect 144 (d*~ 12 12 "+0"))
(expect 140 (d*~ 12 12 "+1"))
(expect 100 (d*~ 12 12 "+2"))
(expect 0 (d*~ 12 12 "+3"))
(expect 0 (d*~ 12 12 "+4"))

(expect 600 (d*~ 24 24 "+2"))
(expect 1000 (d*~ 24 24 "+3"))
(expect 0 (d*~ 24 24 "+4"))

(expect 1000 (d*~ 32 32 "+3"))
(expect 0 (d*~ 32 32 "+4"))

(expect 9800 (d*~ 99 99 "+2"))
(expect 10000 (d*~ 99 99 "+3"))
(expect 10000 (d*~ 99 99 "+4"))
(expect 0 (d*~ 99 99 "+5"))

;; PREC as digits

(expect 100 (d*~ 12 12 1))    ;; MSD 0
(expect 600 (d*~ 24 24 1))    ;; MSD zero, rounds up
(expect 1000 (d*~ 32 32 1))   ;; MSD non-zero
(expect 10000 (d*~ 99 99 1))  ;; MSD rounds to next place

;; u*~

(expect (U 73930)  (u*~ (U 333) (U 222) (prec-to-pod "+1")))

;; f/

(define (df/ a b p)
  (fp2d (f/ (d2fp a) (d2fp b) (prec-to-pod p) ROUND-NEAR)))

(define (df/down a b p)
  (fp2d (f/ (d2fp a) (d2fp b) (prec-to-pod p) ROUND-FLOOR)))

;; cond: (not "AF and BF begin with non-zero digit")
;; B nil
(expect nil (f/ 1 nil 8 ROUND-NEAR))
(expect nil (f/ 0 nil 8 ROUND-NEAR))
;; B zero
(expect nil (f/ (FP 1) "011 + 0" 4 ROUND-NEAR))
(expect nil (f/ (FP 1) "011 + 0" 4 ROUND-NEAR))
;; B non-normalized
(expect 2 (fp2d (f/ (FP 4) "011 + 0 011" 4 ROUND-NEAR)))
;; A nil
(expect "NaN" (df/ nil 1 8))
;; A zero
(expect 0 (df/ 0 1 4))
;; A non-normalized
(expect 2 (fp2d (f/ "011 + 0 011" (FP 1) 4 ROUND-NEAR)))
;; signs
(expect 6.67   (df/ 20 3 3))
(expect -6.67  (df/ 20 -3 3))
(expect 6.67   (df/ -20 -3 3))
(expect 6.7    (df/  20 3 -1))
;; cond: prec
;; af<bf vs. af>=bf
(expect 0.4 (df/ 2 5 1))
(expect 1.2 (df/ 6 5 2))
;; prec: PLACE vs. DIGITS
(expect 6.67   (df/ 20 3 3))
(expect 6.67   (df/ 20 3 -2))
(expect 7      (df/ 20 3 -0))
(expect 10     (df/ 20 3 "+1"))
(expect 6.66   (df/down 20 3 -2))
(expect 6      (df/down 20 3 -0))
(expect -6.67  (df/down 20 -3 -2))
(expect 0      (df/down 20 3 "+1"))
(expect -100   (df/down 20 -3 "+2"))
;; round(AF/BF) == 1
(expect 100    (df/ 199 2 2))
;; cond: [else]
(expect nil    (f/ (FP 3) (FP 2) nil ROUND-FLOOR))

;; f-mod

(expect nil (f-mod nil (FP 1)))
(expect nil (f-mod (FP 1) nil))
(expect nil (f-mod (FP 1) "01 + 0"))
;; normalization
(expect (FP 2) (f-mod (FP 8) "011 + 0 0111"))
(expect (FP 2) (f-mod "011 + 0 011111111" "01 + 0111"))
;; signs differ
(expect (FP 2) (f-mod (FP 5) (FP 3)))
(expect (FP -1) (f-mod (FP 5) (FP -3)))
(expect (FP -2) (f-mod (FP -5) (FP -3)))
(expect (FP 1) (f-mod (FP -5) (FP 3)))
;; ae < be
(expect (FP 3) (f-mod (FP 23) (FP 4)))
;; else
(expect 0.72 (fp2d (f-mod (FP 45) (FP 1.23))))
(expect 2.88 (fp2d (f-mod (FP 12) (FP 4.56))))

;; fp-round

;; significant digits
(expect (FP 12e1) (fp-round "0111 + 01 011 0111" 2 ROUND-NEAR))
(expect (FP 0.0012) (fp-round "-011 + 01 011 0111" 2 ROUND-NEAR))
;; decimal place
(expect (FP 123) (fp-round "0111 + 01 011 0111 01111 0111 01" 0 ROUND-NEAR))
(expect (FP 12e1) (fp-round "0111 + 01 011 0111 01111 0111 01" -01 ROUND-NEAR))
(expect (FP 123.4) (fp-round "0111 + 01 011 0111 01111 0111 01" 01 ROUND-NEAR))
;; not normalized
(expect (FP 12e1) (fp-round "01111 + 0 01 011 0111" 2 ROUND-NEAR))
(expect "0 + 0" (fp-round "01111 + 0" 2 ROUND-NEAR))
;; exponent with large magnitude
(expect "0 + 0" (fp-round (FP 1e-999999999) 0111 ROUND-NEAR))
;; rounding modes
(define `FP1.5 "01 + 01 011111")
(define `FP-1.5 "01 - 01 011111")
(define `FP1 "01 + 01")
(define `FP-1 "01 - 01")
(define `FP2 "01 + 011")
(define `FP-2 "01 - 011")
(expect FP2 (fp-round FP1.5 1 ROUND-NEAR))
(expect FP-2 (fp-round FP-1.5 1 ROUND-NEAR))
(expect FP1 (fp-round FP1.5 1 ROUND-TRUNC))
(expect FP-1 (fp-round FP-1.5 1 ROUND-TRUNC))
(expect FP2 (fp-round FP1.5 1 ROUND-AWAY))
(expect FP-2 (fp-round FP-1.5 1 ROUND-AWAY))
;; sign-sensitive rounding modes
(expect FP2 (fp-round FP1.5 1 ROUND-CEIL))
(expect FP-1 (fp-round FP-1.5 1 ROUND-CEIL))
(expect FP1 (fp-round FP1.5 1 ROUND-FLOOR))
(expect FP-2 (fp-round FP-1.5 1 ROUND-FLOOR))

;; f-cmp

(define (f-cmp-rubric ret)
  (cond
   ((not ret) "=")
   ((findstring "1" ret) ">")
   (else "<")))


;; positive
(expect ">" (f-cmp-rubric (f-cmp (FP 20) (FP 5))))
(expect "<" (f-cmp-rubric (f-cmp (FP 20) (FP 50))))
(expect "=" (f-cmp-rubric (f-cmp (FP 20) (FP 20))))
;; nil/NaN
(expect ">" (f-cmp-rubric (f-cmp (FP -1) nil)))
(expect "<" (f-cmp-rubric (f-cmp nil (FP -1))))
(expect "=" (f-cmp-rubric (f-cmp nil nil)))
;; A=0
(expect "<" (f-cmp-rubric (f-cmp (FP 0) (FP 1))))
(expect "=" (f-cmp-rubric (f-cmp (FP 0) (FP -0))))
(expect ">" (f-cmp-rubric (f-cmp (FP 0) (FP -1))))
;; B=0
(expect ">" (f-cmp-rubric (f-cmp (FP 1) (FP 0))))
(expect "<" (f-cmp-rubric (f-cmp (FP -1) (FP 0))))
;; A<0
(expect "<" (f-cmp-rubric (f-cmp (FP -1) (FP 0))))
(expect ">" (f-cmp-rubric (f-cmp (FP -1) (FP -2))))
;; else
(expect ">" (f-cmp-rubric (f-cmp (FP 1) (FP -2))))

;; fp-sq

(expect 400 (fp2d (fp-sq (FP 20))))

;; fp-u-pwr

(expect 1     (fp2d (fp-u-pwr (FP 2) (U 0))))
(expect 1     (fp2d (fp-u-pwr (FP -2) (U 0))))
(expect 2     (fp2d (fp-u-pwr (FP 2) (U 1))))
(expect 4     (fp2d (fp-u-pwr (FP -2) (U 2))))
(expect -2048 (fp2d (fp-u-pwr (FP -2) (U 11))))

;;--------------------------------
;; fp-fix
;;--------------------------------

;; uf-fix

(expect (UV 12300) (uf-fix (UV 123) 5))
(expect (UV 123) (uf-fix (UV 123) 3))
(expect (UV 12) (uf-fix (UV 123) 2))
(expect nil (uf-fix (UV 123) 0))
(expect nil (uf-fix (UV 123) nil))

;; fp-fix

(define (ff xd wd pd)
  (fp-fix (FP xd) wd pd (d2u pd)))


;; left of decimal: exp>0
(expect (ff 123.45 nil nil) "123")        ;; wordlist
(expect (ff 1e2 nil nil)    "100")        ;; rpad left of decimal
(expect (ff 123.45 nil 0) "123")        ;; wordlist
;; left of decimal: exp<=0
(expect (ff 1.23e-2 nil nil) "0")
(expect (ff 123e-1 nil nil)  "12")
;; right of decimal: exp>0
(expect (ff 1e2 nil 2)      "100.00")     ;; padding to right
(expect (ff 123.456 nil 2)  "123.45")     ;; trim digits to right
(expect (ff 1e-99999999 nil 2)  "0.00")   ;; big -E
(expect (ff 1e-4 nil 4)  "0.0001")
(expect (ff 1e-3 nil 4)  "0.0010")
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
;; extraneous zeros
(expect (ff "0001" nil nil)   "1")
