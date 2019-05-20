(require "core.scm")
(require "math0.scm" &private)
(require "math1.scm" &private)
(require "math0-q.scm" &private)
(require "math1-q.scm" &private)
(require "math2.scm" &private)

;;----------------------------------------------------------------
;; Test utils
;;----------------------------------------------------------------

;; Expect OUT to match REF except perhaps for PLUS-OR-MINUS in the digit after
;; the last digit in REF.
;;
(define (uf-match ref plus-or-minus out)
  (let ((delta (uf-carry (+_+ (patsubst "%" 0 ref) plus-or-minus))))
    (if (or (uf-lt? (uf-add ref delta) out)
            (uf-lt? out (uf-sub ref delta)))
        (begin
          (print "ref = " (u2d (smash ref)))
          (print "out = " (u2d (smash out))))
        1)))

(define `(uf±1 ref out)
  (assert (uf-match ref U10 out)))

(define `(uf±0.5 ref out)
  (assert (uf-match ref U5 out)))

(uf±1 (UV 1234) (UV 1235))
(uf±1 (UV 1234) (UV 1233))
(uf±0.5 (UV 1234) (UV 1234))
(uf±0.5 (UV 1234) (UV 12345))
(uf±0.5 (UV 1234) (UV 12335))


;;----------------------------------------------------------------
;; Tests
;;----------------------------------------------------------------


;; wsub

(expect "1 2 3 4" (wsub "1 2 3 4" nil))
(expect "2 3 4" (wsub "1 2 3 4" "a"))
(expect nil (wsub "1 2 3 4" "1 2 3 4"))

;; u-zeros

(expect " 0 0 0" (u-zeros "0111"))
(expect " 0" (u-zeros "0001"))
(expect nil (u-zeros "-0001"))
(expect " 0 0 0 0 0 0 0 0 0 0" (u-zeros "010"))

;; round-uf-const

(define `K2 "0 0")
(expect (round-uf-const nil K2) nil)
(expect (round-uf-const (UV 0) K2) nil)
(expect (round-uf-const (UV 01) K2) nil)
(expect (round-uf-const (UV 013) K2) (UV 01))
(expect (round-uf-const (UV 016) K2) (UV 02))
(expect (round-uf-const (UV 014) K2) nil)
(expect (round-uf-const (UV 015) K2) nil)
(expect (round-uf-const (UV 050) K2) (UV 05))
(expect (round-uf-const (UV 0149) K2) nil)
(expect (round-uf-const (UV 0150) K2) nil)
(expect (round-uf-const (UV 01499) K2) nil)
(expect (round-uf-const (UV 01500) K2) nil)
(expect (round-uf-const (UV 01498) K2) (UV 01))
(expect (round-uf-const (UV 01501) K2) (UV 02))
(expect (round-uf-const (UV 0105) K2) (UV 01))
(expect (round-uf-const (UV 0194) K2) (UV 02))

;; get-uf-const

(define (guc-func n-count)
  (UV 0123499999999998))

(define (guc n)
  (get-uf-const (native-name guc-func) (nzeros n)))

(declare guc-func/v)
(declare guc-func/c)

;; Initially compute N+5 digits
(expect (guc 3) (UV 012))
(expect 8 (words guc-func/c))
(expect (UV 01235000) guc-func/v)
;; This allows us to compute 5 digits
(expect (guc 5) (UV 01235))
(expect 8 (words guc-func/c))
;; To know the first three digits, we need digits beyond the last 9
(expect (guc 4) (UV 0123))
(expect 18 (words guc-func/c))
(expect (guc 5) (UV 01235))
(expect 18 (words guc-func/c))

;; uf-divu

(expect (UV 1234) (uf-divu (UV 24681212) (U 2) 4))
(expect (UV 01020) (uf-divu (UV 24481212) (U 24) 4))
(expect (UV 001003) (uf-divu (UV 24481212) (U 244) 4))

;; uf-atanh

(uf±1 (UV 0010000003333335333334762)
      (uf-atanh (UV 001) (nzeros 25)))
(uf±1 (UV 0123406264175351930433597)
      (uf-atanh (UV 01234) (nzeros 25)))
(uf±1 (UV 7126565760712450802820306)
      (uf-atanh (UV 61234) (nzeros 26)))
(expect 0 (uf-atanh "0" (nzeros 10)))
(uf±1 (UV 01) (uf-atanh (UV 0008) "0 0 0"))

;; uf-log-pos, uf-log-neg

;; N digits usually yields N-1 valid digits to the right of the decimal.
(expect (UV 0) (uf-log-pos (UV 0) (nzeros 19)))
(uf±1 (UV 173953307123438)
      (uf-log-pos (UV 19) (nzeros 16)))  ;; near max value
(uf±1 (UV 012422519986057)
      (uf-log-neg (UV 987654321) (nzeros 15)))
(uf±1 (UV 19723216952971)
      (uf-log-neg (UV 821) (nzeros 15)))  ;; near min value
(uf±1 (UV 00001012094878302507)
      (uf-log-pos (UV 000010121) (nzeros 20)))

;; const-log-M

(expect (UV 0198026273) (const-log-M (nzeros 10)))

;; psrch

(expect 0 (psrch (UV 30) (d2u [30 03 02 01]) nil))
(expect 1 (psrch (UV 29) (d2u [30 03 02 01]) nil))
(expect 3 (psrch (UV 01) (d2u [30 03 02 01]) nil))
(expect 4 (psrch (UV 001) (d2u [30 03 02 01]) nil))

;; m-powers

(expect 116 (words cached-m-powers))
(expect 233 (words (subst 0 " 0" (lastword cached-m-powers))))

;; uf-log-fr

(uf±1 (UV 20923241675108496126)
      (uf-log-fr (UV 1234) (nzeros 20)))
(uf±1 (UV 023)
      (uf-log-fr (UV 79) "0 0"))
(uf±1 (UV 02)
      (uf-log-fr (UV 79) "0"))

;; log-size

;; (lst FLOAT VEC) : Ensure that result of (log-size FLOAT) is in VEC
(define `(lst x sizes)
  (let ((o (log-size (FP x))))
    (expect o (or (filter (U sizes) o)
                  (U sizes)))))

;; Vectors holds [EXACT-RESULT ACCEPTABLE]
(lst 1.5e+21714724095 11) ;; 0.500e11
(lst 1.7e+4386374267 [11 10]) ;; 0.101e11
(lst 1.0441001e+43386 5) ;; 0.999e5
(lst 5.2977952e+21714 5) ;; 0.500e5
(lst 2.3673759e+4386 [5 4]) ;; 0.101e5
(lst 3.99828929e+4338 4) ;; 0.999e4
(lst 2.96762838e+2171 4) ;; 0.500e4
(lst 4.3393704e+438 [4 3]) ;; 0.101e4
(lst 7.247486605e+433 3) ;; 0.999e3
(lst 1.403592218e+217 3) ;; 0.500e3
(lst 7.307059979e+43 [3 2]) ;; 0.101e3
(lst 2.432308974e+43 2) ;; 0.999e2
(lst 5.1847055286e+21 2) ;; 0.500e2
(lst 24343.009424 [2 1]) ;; 0.101e2
(lst 21807.2987982 1) ;; 0.999e1
(lst 148.413159103 1) ;; 0.500e1
(lst 2.74560101502 [1 0]) ;; 0.101e1
(lst 2.715564905319 0) ;; 0.999e0
(lst 1.6487212707 0) ;; 0.500e0
(lst 1.106276641763 [0 -1]) ;; 0.101e0
(lst 1.1050604065095 -1) ;; 0.999e-1
(lst 1.051271096376 -1) ;; 0.500e-1
(lst 1.0101511771513 -1) ;; 0.101e-1
(lst 1.010040066633 -2) ;; 0.999e-2
(lst 1.0050125208594 -2) ;; 0.500e-2
(lst 1.00101051022176 -2) ;; 0.101e-2
(lst 1.000999499166709 -3) ;; 0.999e-3
(lst 1.000500125020836 -3) ;; 0.500e-3
(lst 1.000101005100672 -3) ;; 0.101e-3
(lst 1.0000999049901712 -4) ;; 0.999e-4
(lst 1.0000500012500208 -4) ;; 0.500e-4
(lst 1.0000101000510052 -4) ;; 0.101e-4
(lst 1.00000999004990022 -5) ;; 0.999e-5
(lst 1.00000500001250002 -5) ;; 0.500e-5
(lst 1.00000101000051005 -5) ;; 0.101e-5
(lst 1.00000000000999000000005 -11) ;; 0.999e-11
(lst 1.00000000000500000000001 -11) ;; 0.500e-11
(lst 1.00000000000101 -11) ;; 0.101e-11
(lst 0.99999899000051 -5)  ;; -0.101e-5
(lst 0.9999950000125 -5)  ;; -0.5e-5
(lst 0.9999900100499 -5)  ;; -0.999e-5
(lst 0.999989900051 -4)  ;; -0.101e-4
(lst 0.99995000124998 -4)  ;; -0.5e-4
(lst 0.99990010498984 -4)  ;; -0.999e-4
(lst 0.99989900510033 -3)  ;; -0.101e-3
(lst 0.99950012497917 -3)  ;; -0.5e-3
(lst 0.99900149883437 -3)  ;; -0.999e-3
(lst 0.99899050987833 -2)  ;; -0.101e-2
(lst 0.99501247919268 -2)  ;; -0.5e-2
(lst 0.99005973429701 -2)  ;; -0.999e-2
(lst 0.98995083371588 -1)  ;; -0.101e-1
(lst 0.95122942450071 -1)  ;; -0.5e-1
(lst 0.9049279063021 -1)  ;; -0.999e-1
(lst 0.90393303288586 [0 -1])  ;; -0.101e0
(lst 0.60653065971263 0)  ;; -0.5e0
(lst 0.36824750461366 0)  ;; -0.999e0
(lst 0.36421897957152 [1 0])  ;; -0.101e1
(lst 0.0067379469990855 1)  ;; -0.5e1
(lst 4.5856206642207e-05 1)  ;; -0.999e1
(lst 4.1079555225301e-05 [2 1])  ;; -0.101e2
(lst 1.9287498479639e-22 2)  ;; -0.5e2
(lst 4.1113197817301e-44 2)  ;; -0.999e2
(lst 1.3685394711739e-44 [3 2])  ;; -0.101e3
(lst 7.124576407e-218 3) ;; -0.500e3
(lst 1.379788683e-434 3) ;; -0.999e3
(lst 2.30448177e-439 [4 3]) ;; -0.101e4
(lst 3.36969415e-2172 4) ;; -0.500e4
(lst 2.50106965e-4339 4) ;; -0.999e4
(lst 4.2240862e-4387 [5 4]) ;; -0.101e5
(lst 1.8875777e-21715 5) ;; -0.500e5
(lst 9.5776258e-43387 5) ;; -0.999e5
(lst 6e-4386374268 [11 10]) ;; -0.101e11
(lst 6.9e-21714724096 11) ;; -0.500e11
(lst 7.3e-43386018743 11) ;; -0.999e11

(expect 0 (log-size 1))  ;; Any value is acceptable.

;; adjust-digit-count

(expect "0 0 1 1" (adjust-digit-count "0 0" "-011"))
(expect "" (adjust-digit-count "0 0" "011"))
(expect "0" (adjust-digit-count "0 0" "01"))
(expect "0" (adjust-digit-count "0 0" "01"))


;;--------------------------------
;; Exp
;;--------------------------------

;; uf-exp-small

(define `(uf-exp-small-round u digits)
  (rest (uf-round digits DIV-NEAREST (uf-exp-small u (nzeros digits)))))
(expect (UV 1648721270700128)
        (uf-exp-small-round (UV 5) 16))
(expect (UV 24596031111569)
        (uf-exp-small-round (UV 9) 14))


;; uf-exp-med

(uf±1 (UV 02718) (uf-exp-med U1 (nzeros 6)))
(uf±1 (UV 01000) (uf-exp-med 0 (nzeros 6)))
(uf±1 (UV 059874141715197) (uf-exp-med (UV 17896596280238) (nzeros 15)))
(uf±1 (UV 099999990700596) (uf-exp-med (UV 2302585) (nzeros 15)))

(define `(exp-AvsB n dd)
  (uf±1 (wordlist 1 n (>>1 (uf-exp-small (UV dd) (+_+ "0 0" (nzeros n)))))
       (uf-exp-med (>>1 (UV dd)) (nzeros n))))

(exp-AvsB 16 01)
(exp-AvsB 16 00122)
(exp-AvsB 16 0001)
(exp-AvsB 16 02)
(exp-AvsB 16 09)

;; cached-m-powers

(expect (smash um/10) (word 1 cached-m-powers))
(expect (smash (rest (uf-mul um/10 um/10)))
        (word 2 cached-m-powers))
(expect 01
        (word 1 (uf-mul (u2uv (lastword cached-m-powers)) um/10)))

;; fp2su

(expect 1000 (u2d (fp2su (FP 1e3))))
(expect -1000 (u2d (fp2su (FP -1e3))))
(expect 1234 (u2d (fp2su (FP 1.2345e3))))
(expect 0 (u2d (fp2su (FP -1.2345e-3))))

;; fp2uf

(expect (UV 00123) (fp2uf (FP 1.23) (U 3)))
(expect (UV 23) (fp2uf (FP 12.3) U1))
(expect (UV 0) (fp2uf (FP 1e99) U1))
(expect (UV 0) (fp2uf (FP 1.234e4294967297) U1))
