(require "core.scm")
(require "math0.scm" &private)
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
(expect "-0100 - 01" (fp-norm (concat "01 - " (repeat-words 0 101) " 01")))

;; u2fp

;; cond: (not (non-digit? u))
(expect (FP nil) nil)
(expect (FP 1) "01 + 01")
(expect (FP 11) "011 + 01 01")
(expect (FP 01) "01 + 01")
(expect (FP 00) "0 + 0")
;; (word 10 (spread u))
(expect (FP (concat 1 (smash (repeat-words 0 200))))
        (concat "011001 + 01 " (repeat-words 0 200)))
(expect (FP (concat "0." (smash (repeat-words 0 200)) "1"))
        (concat "-01100 + 01"))
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
(for n [1 -1 "1.2" "1e3" "1.2e3" "1.2e+3" "1.2e-3" ]
     (expect nil (FP (concat " " n)))
     (expect nil (FP (concat n " "))))

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
;; PREC and POD
;;--------------------------------

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

;;--------------------------------
;; fp-fix
;;--------------------------------

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
