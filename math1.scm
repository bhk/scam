;;----------------------------------------------------------------
;; math1: FP encoding and operations
;;----------------------------------------------------------------

(require "core.scm")
(require "math0.scm")


;; Construct an FP number
;;
(define `(make-fp exp sgn frac)
  &public
  (._. exp sgn frac))


(define `(fp.xpo n) &public (word 1 n))        ;; exponent (U-encoded)
(define `(fp.sign n) &public (word 2 n))       ;; sign
(define `(fp.uf n) &public (nth-rest 3 n))     ;; fraction (UF-encoded)
(define `(fp.msd n) &public (word 3 n))        ;; most significant digit of UF
(define `(fp.uf<<1 n) &public (nth-rest 4 n))  ;; remainder of digits of UF


(define `NaN &public "NaN")

;; math.scm re-exports this
(define `(abs x)
  &public
  (patsubst "-%" "%" x))


(define `(non-digit? u)
  &public
  (subst 0 "" 1 "" u))


;; U-strings cannot contain 2; they have been converted to "011".
(define `(u-begins? prefix u)
  (findstring (.. 2 prefix) (.. 2 u)))


(define `(u-rm-prefix prefix u)
  (subst (.. 2 prefix) "" (.. 2 u)))


;; Remove SUFFIX from the end of U if U ends with SUFFIX.  Otherwise,
;; concatenate "2" to U.
;;
(define `(u-rm-suffix suffix u)
  (subst (.. suffix 2) "" (.. u 2)))


;; Normalize N (ensure that UF >= 0.1 unless N is zero).
;;
(define (fp-norm x)
  &public
  (if (filter 0 (fp.msd x))
      (foreach (zeros (word 1 (subst "001" "0 01" (smash (fp.uf x)))))
        (define `zeros-tally
          (subst 0 1 zeros))

        (define `norm-uf
          (subst (.. 9 (spread zeros)) nil (._. "9" (fp.uf x))))

        (.. (u-sub-m (fp.xpo x) (u-add-ones "0" zeros-tally))
            " " (fp.sign x) (or norm-uf " 0")))
      x))


(declare (u2fp u ?exp ?sgn ?ufrac))


(define (u2fp-exp u)
  (foreach (s (lastword (subst "E" " E" u)))
    (foreach (en (subst "E" "" (subst "E+" "E" (subst "+-" "X" s))))
      (if (non-digit? (patsubst "-%" "%" en))
          nil
          ;; If U ends with "E"+EN, then remove it cleanly.
          ;; Otherwise, leave behind clearly invalid characters.
          (u2fp (subst "E" "X" (u-rm-suffix s u))
                en)))))


;; Validate U-encoded string and convert to normalized FP format.
;; Return NIL if the number is not properly formatted.
;;
;; Syntax for valid numbers:   "-"? DIGITS ("." DIGITS)? ([Ee] SIGN? DIGITS)?
;;
(define (u2fp u ?n-exp ?n-sign ?n-frac)
  &public
  (cond
   ;; simple integer
   ((not (non-digit? u))
    (if u
        (if (or n-exp (word 10 (spread u)) (filter "00%" (.. u "0")))
            (fp-norm (.. (u-add-ones (or n-exp 0) (subst 1 nil 0 1 u))
                             " " (or n-sign "+")
                             (spread u) n-frac))
            (.. "0" (subst 1 nil 0 1 u)
                    " " (or n-sign "+") (spread u) n-frac))))

   ;; Remove exponent
   ((findstring "E" (subst "e" "E" u))
    (u2fp-exp (subst "e" "E" u)))

   ;; Strip (valid) sign prefix: valid-signed("-" ++ N) <=> valid-unsigned(N)
   ((u-begins? "-0" u)
    (u2fp (u-rm-prefix "-" u) n-exp "-"))

   ;; Extract fractional part
   ((findstring "." u)
    ;; validate frac and leave any garbage in u
    (foreach (f (lastword (subst "." " " u)))
      (if (non-digit? f)
          nil
          (u2fp (subst "." "x" (u-rm-suffix (.. "." f) u))
                n-exp
                n-sign
                (spread f)))))
   ;; else nil
   ))


;; Convert FP to a U-encoded decimal floating point string.  Digits
;; are denoted in unary form (0, 01, 011, ...) but a decimal point and
;; exponent may also appear.
;;
(define (fp2u fp)
  &public
  (define `exp (fp.xpo fp))
  (define `sgn (fp.sign fp))
  (define `frac (fp.uf fp))
  (define `frac-msd (fp.msd fp))
  (define `frac*10 (fp.uf<<1 fp))

  (cond
   ((findstring 1 frac-msd)
    ;; We use fixed point when 1e-6 <= FP < 1e21; "E" notiation otherwise.
    ;; FP is in this range when -5 <= EXP <= 21.  We reduce EXP to a string
    ;; with which we can quickly check that range.  We also use the string
    ;; for padding 0's on the left/right in the fixed point representation.

    (define `uv-result
      (foreach (et (or (subst "10" "091" "10" "91" 0 nil exp)
                       "-"))
        (if (findstring et "-11111 91911 91111111111")
            ;; Fixed point  (-5 <= EXP <= 21)
            (if (filter "-%" et)
                ;; -5...0 => EXP is number of zeros left of first digit
                (.. "0. " (subst 1 "0 " "-" nil et) frac)
                ;; 1..21 => EXP is number of digits left of decimal
                (subst "10" "1" "00" "0"
                       (join frac (.. (subst 9 T9 1 " 0" et) "."))))
            ;; "E" notation
            (.. frac-msd ". " frac*10
                ;; Two spaces before "e" allow preceding redundant
                ;; zeros to be trimmed; "." after the exponent
                ;; preserves its trailing zeros.
                "  e" (subst "+-" "-" (.. "+" (u-1 exp))) "."))))

    (.. (findstring "-" sgn)
            ;; Remove trailing 0's after "." and trailing "." after digits
            ;; (and possible preceding "e+/+XX").  Remove spaces.
            (subst ". 0" ".0" ". " nil " " nil
                   (.. (filter-out "%0" (subst "0 " "0" uv-result))
                       " "))))

   ;; normalized?
   ((findstring 1 frac)
    (fp2u (make-fp (u-1 exp) sgn frac*10)))

   (fp 0)

   (else NaN)))


;;--------------------------------
;; PREC and POD
;;--------------------------------

;; Validate and convert PREC (public precision specification) to POD
;; (internal precision specification).
;;
;;   PREC                      POD
;;   ---------                 -------
;;   +N, -N, 0 =>  place   =>  -U, U      (negated sign, U-encoded)
;;   1-9+      =>  digits  =>  1-9+       (decimal)
;;   nil       =>  digits  =>  16
;;   other     => invalid  =>  nil
;;
(define (prec-to-pod prec)
  &public
  (if (not prec)
      16
      (if (findstring prec (word 1 prec))
          (or (filter "0 -0 1 8 16" prec)
              (foreach (u (d2u (patsubst "-%" "%" (subst "+" "-" prec))))
                (if (not (subst 0 nil 1 nil u))
                    (if (filter "-% +%" prec)
                        ;; place
                        (.. (if (findstring "+" prec) "-") u)
                        ;; digits
                        (if (filter "0%" prec)
                            (prec-to-pod (patsubst "0%" "%" prec))
                            prec))))))))


;; True if POD (Place-or-Digits) contains a "place" value: the number of
;; digits to the right of the decimal place, U-encoded.
;;
(define `(pod-is-place? pod)
  &public
  (filter "0% -0%" pod))


;;--------------------------------
;; FP operations
;;--------------------------------

(define `FP0 &public "0 + 0")
(define `FP1 &public "01 + 01")

(define `(fp-negate n)
  &public
  (subst " +" " !" " -" " +" "!" "-" n))


(define `(fp>0? fx)
  &public
  (findstring "+ 01" (filter-out 0 fx)))


(define `(fp<0? fx)
  &public
  (findstring "- 01" (filter-out 0 fx)))


(define `(fp!=0? fx)
  &public
  (findstring 1 (fp.uf fx)))


(define `(fp-abs fx)
  &public
  (subst " -" " +" fx))


(define `(d2fp dx)
  &public
  (u2fp (d2u dx)))


(define `(fp2d fx)
  &public
  (u2d (fp2u fx)))


;; Construct FP number given frac>>1.
;;
(define (make-fp<<1 exp sgn frac>>1)
  (if (filter 0 (word 1 frac>>1))
      (make-fp exp sgn (or (rest frac>>1) 0))
      (make-fp (u+1 exp) sgn frac>>1)))


;; Convert a multi-digit UV number to a single string of 1's where the count
;; of 1's equals the value of the multi-digit number.
;;
(define (tally uv)
  &public
  (if (word 2 uv)
      (tally (.. (subst 1 T10 (word 1 uv)) (rest uv)))
      (subst 0 nil uv)))


;; Return a list with U words, all "0".  UX may have digits larger than 9.
;;
(define (u-zeros u)
  &public
  (if (filter-out "-%" u)
      (subst 1 " 0"
             (if (findstring "10" u)
                 (tally (subst "0" " 0" u))
                 (subst 0 nil u)))))


(define `(pad ui)
  (subst 1 "0 " (tally (spread ui))))


;; Note: AE must be >= BE
(define (fp-add-x ae as af be bs bf)
  (define `b-pad
    (and (filter-out ae be)
         (pad (u-sub-m ae be))))

  (if (findstring as bs)
      ;; sign is same => add
      (make-fp<<1 ae as (uf-add (>>1 af) (>>1 (.. b-pad bf))))

      ;; difference
      (._. ae (uf-sign-sub af (.. b-pad bf) (findstring "-" as)))))


;; Add A to B.
;;
(define (f+ a b)
  &public
  (define `ae (fp.xpo a))
  (define `as (fp.sign a))
  (define `af (fp.uf a))
  (define `be (fp.xpo b))
  (define `bs (fp.sign b))
  (define `bf (fp.uf b))

  (and a b
       (if (u-lt? ae be)
           (fp-add-x be bs bf ae as af)
           (fp-add-x ae as af be bs bf))))


;; Subtract B from A.
;;
(define `(fp-sub-m a b)
  &public
  (f+ a (fp-negate b)))


(define (f- a b)
  &public
  (fp-sub-m a b))


(define `(sign-mul sign-a sign-b)
  (if (findstring sign-a sign-b) "+" "-"))


;; Multiply A by B.
;;
(define (f* a b)
  &public
  (define `exp (u+ (fp.xpo a) (fp.xpo b)))
  (define `sgn (sign-mul (fp.sign a) (fp.sign b)))
  (define `m (uv-mul (fp.uf a) (fp.uf b)))
  (and a b (make-fp exp sgn m)))


;; f*~: Compute X*Y to the precision given by POD (see prec-to-pod).
;; Called via (prec-op "mulp" ...)
;;
;; X & Y must be normalized (no leading zeros in UF).
;;
(define (f*~ x y pod)
  &public
  (and
   pod x y
   (foreach (exp (u+ (fp.xpo x) (fp.xpo y)))
     (foreach (num-digits (if (pod-is-place? pod)
                              (u2d (u+ exp pod))
                              pod))
       ;; get up to NUM_DIGITS+1 digits of A
       (define `(in-digits a)
         (.. (word 3 a)
             (if (word 4 a)
                 (.. " " (wordlist 1 num-digits (nth-rest 4 a))))))

       (if (filter "-% 0" num-digits)
           ;; Rounding place is to the left of EXP => 0
           FP0
           ;; Non-zero num-digits
           (let ((exp exp)
                 (sgn (sign-mul (fp.sign x) (fp.sign y)))
                 (uf (uv-mul (in-digits x) (in-digits y)))
                 (pod pod)
                 (num-digits num-digits))

             ;; If pod is given in terms of *significant* digits and the first result
             ;; digit is zero, use NUM-DIGITS digits *after* the first digit.
             (define `skip-zero
               (and (filter-out "-0% 0%" pod)
                    (filter 0 (word 1 uf))
                    (addprefix " " (word 2 (nth-rest num-digits uf)))))

             (make-fp exp sgn
                      (.. (wordlist 1 num-digits uf) skip-zero))))))))


;; Divide X by Y.
;;
;; X & Y must be normalized (no leading zeros in UF).
;; POD = see prec-to-pod
;; MODE = rounding mode.
;;
;; Results are not normalized.
;;
;; The numerical value of X/Y is given simply:
;;
;;    Q = X/Y = X.UF/X.UF * 10^(X.EXP-Y.EXP) * X.SGN * Y.SGN
;;
;; But there are some complications:
;;
;;  1. uf-div requires X.UF < Y.UF, so we divide X.UF by 10 and increment
;;     X.EXP when necessary.
;;  2. In order to count *significant* digits in QF, we need X.UF >= Y.UF/10,
;;     which ensures the first digit of Q.UF is non-zero.
;;  3. Rounding may result in Q.UF==1 overflowing the UF value result.  The
;;     rounding functions return Q.UF/10 to avoid overflow.
;;
(define (f/ x y pod mode)
  &public
  (define `x.uf (fp.uf x))
  (define `y.uf (fp.uf y))
  (define `q.exp (u-sub-m (fp.xpo x) (fp.xpo y)))
  (define `q.sgn (sign-mul (fp.sign x) (fp.sign y)))

  (cond
   ;; Normalized and non-zero?
   ((not (findstring 101 (.. (fp.msd x) (fp.msd y))))
    (if (findstring 1 y.uf)
        (if (findstring 1 x.uf)
            (f/ (fp-norm x) (fp-norm y) pod mode)
            (if x FP0 nil))
        nil))

   ;; Now: X.UF >= 0.1,  Y.UF >= 0.1
   (pod
    (let ((q.exp_ q.exp)
          (q.sgn q.sgn)
          (x.uf_ x.uf)
          (y.uf y.uf)
          (pod pod)
          (uns-mode (uns-round-mode mode q.sgn))
          (xlt? (uf-lt? x.uf y.uf)))

      ;; ensure X < Y when calling uf-div
      (define `q.exp (if xlt? q.exp_ (u+1 q.exp_)))
      (define `x.uf (if xlt? x.uf (>>1 x.uf)))

      (foreach (num-digits (if (pod-is-place? pod)
                               (u2d (u+ q.exp pod))
                               pod))

        (if (filter "-%" num-digits)
            ;; Rounding place is to the left of Q.EXP
            ;; result = 0 unless mode=AWAY
            (if (filter ROUND-AWAY uns-mode)
                (make-fp (u-sub-m "01" pod) q.sgn "01")
                FP0)

            ;; Non-negative num-digits
            (make-fp<<1 q.exp q.sgn (uf-div x.uf y.uf num-digits uns-mode))))))))


;; called from `op`
(define (u/ x y pod mode)
  (fp2u (f/ (u2fp x) (u2fp y) pod mode)))


;; Return the remainder after flooring division:  R = A - B*floor(A/B)
;;
;; For positive A, B:
;;    R = A - B*floor(A/B)
;;    A/B = AF/BF << (AE-BE)
;;    floor(A/B) = floor(AF/BF << (AE-BE))
;;    (uf-div AF BF N=(AE-BE) ROUND-REMAINDER) -->
;;       REM = (AF - BF*Qt) << N
;;        Qt = floor(AF/BF << N) >> N
;;           = floor(A/B) >> N
;;       REM = (AF - BF*floor(A/B) >> AE << BE) << AE >> BE
;;           = (A - B*floor(A/B)) >> BE
;;
(define (f-mod a b)
  &public
  (cond
   ;; Normalized and non-zero?
   ((not (findstring 101 (.. (fp.msd a) (fp.msd b))))
    (if (findstring 1 (fp.uf b))
        (if (findstring 1 (fp.uf a))
            (f-mod (fp-norm a) (fp-norm b))
            (if a FP0 nil))
        nil))

   ;; signs differ?
   ((not (findstring (fp.sign a) (fp.sign b)))
    (let ((m (f-mod a (fp-negate b)))
          (b b))
      (if (findstring 1 (fp.uf m))
          (f+ b m)
          m)))

   ((u-lt? (fp.xpo a) (fp.xpo b))
    a)

   (else
    (make-fp (fp.xpo b)
             (fp.sign a)
             (uf-div (>>1 (fp.uf a))
                     (fp.uf b)
                     (u2d (u-sub-m (u+1 (fp.xpo a)) (fp.xpo b)))
                     ROUND-REMAINDER)))))



;; called from `op`
(define (u-mod x y)
  (if (findstring "-" (.. x y))
      (fp2u (f-mod (u2fp x) (u2fp y)))
      (or (u-div-uns x y ROUND-REMAINDER)
          NaN)))



;; Round X to specified precision
;;
;; mode = one of {ROUND-NEAR/TRUNC/AWAY/CEIL/FLOOR}
(define (fp-round x pod mode)
  &public
  ;; Convert signed rounding mode to unsigned mode
  (define `uns-mode
    (uns-round-mode mode (fp.sign x)))

  (cond
   ((findstring 1 (fp.msd x))
    ;; return NIL if POD is NIL
    (foreach (num-digits (if (pod-is-place? pod)
                             (u2d (u+ (fp.xpo x) pod))
                             pod))
      (foreach (uns-mode uns-mode)
        (if (filter "-%" num-digits)
            ;; Rounding place is to the left of EXP => 0 unless mode=CEILING
            (if (filter ROUND-CEIL uns-mode)
                (make-fp (u-sub-m "01" pod) (fp.sign x) "01")
                FP0)

            (make-fp<<1 (fp.xpo x)
                        (fp.sign x)
                        (if (filter 0 num-digits)
                            (rest (uf-round-at 1 uns-mode (>>1 (fp.uf x))))
                            (uf-round-at num-digits uns-mode (fp.uf x))))))))

   ((findstring 1 (fp.uf x))
    (fp-round (fp-norm x) pod mode))

   (x
    ;; return NIL when X is NIL
    FP0)))


;; Truncate FX (towards zero).
;;
(define (fp-trunc fx)
  &public
  (if (n>0? (fp.xpo fx))
      ;; Avoid using EXP as a word index if it's too large
      (if (word 9 (spread (fp.xpo fx)))
          fx
          (wordlist 1 (u2d (u-add-ones (fp.xpo fx) 11)) fx))
      (if fx FP0)))


;; Raise N to the next integer equal to or larger (away from zero).
;;
(define (fp-mag-ceiling n)
  (if (n>0? (fp.xpo n))
      (let ((tr (fp-trunc n))
            (n n))
        (if (findstring 1 (subst tr nil n))
            ;; round magnitude up
            (make-fp<<1 (fp.xpo tr)
                        (fp.sign tr)
                        (uf-carry (>>1 (.. (fp.uf tr) 1))))
            tr))
      (if (findstring 1 (fp.uf n))
          (make-fp "01" (fp.sign n) "01")
          (if n FP0))))


(define (fp-floor n)
  &public
  (cond
   ((findstring " -" n) (fp-mag-ceiling n))
   (n (fp-trunc n))))


(define (fp-ceil n)
  &public
  (cond
   ((findstring " +" n) (fp-mag-ceiling n))
   (n (fp-trunc n))))


;; ASSERT: A and B are normalized (fp.uf >= 0.1)
;;
(define (f-cmp a b)
  &public
  (cond
   ;; Both are positive
   ((findstring (findstring "+ 01" a) b)
    (or (u-cmp (fp.xpo a) (fp.xpo b))
        (uf-cmp (fp.uf a) (fp.uf b))))

   ;; Treat NaN as less than everything else
   ((not (and a b))
    (if a 1 (if b "~")))

   ;; A=0 or B=0 ?
   ((findstring "00" (.. (fp.msd a) (fp.msd b) 0))
    (cond
     ;; A!=0, B=0
     ((findstring 1 (fp.msd a))
      (if (filter "-" (fp.sign a)) "~" 1))
     ;; A=0, B!=0
     ((findstring 1 (fp.msd b))
      (if (filter "-" (fp.sign b)) 1 "~"))
     ;; A=B=0
     (else nil)))

   ((fp<0? a)
    (if (fp<0? b)
        (f-cmp (fp-negate b) (fp-negate a))
        "~"))

   (else 1)))


(define `(fp-lt? fa fb)
  &public
  (findstring 1 (f-cmp fb fa)))


;; Return X*X.
;;
(define (fp-sq x)
  (f* x x))


;; Raise X to the power of N.
;;
;; X = FP number
;; N = U-encoded non-negative integer
;;
(define (fp-u-pwr x n)
  &public
  (foreach (n/2 (subst "11" 2 "10" "022222" n))

    (if (findstring 2 n/2)      ;; n > 1?
        (if (findstring 1 n/2)  ;; n is odd?
            (f* (fp-sq (fp-norm (fp-u-pwr x (subst 1 nil 2 1 n/2)))) x)
            (fp-sq (fp-norm (fp-u-pwr x (subst 2 1 n/2)))))
        (if (findstring 1 n)
            x
            FP1))))


;;--------------------------------
;; fp-fix
;;--------------------------------


;; Return LEN digits of UF, extending with zeros on the right as necessary.
;; LEN is a non-negative decimal integer.  If LEN=0 or nil, empty string is
;; returned.
;;
(define (uf-fix uf len)
  &public
  (if (n>0? len)
      (wordlist 1 len
                (._. uf (if (word len uf) nil (repeat-words 0 len))))))


;; Pad LST on left with repetitions of W until length is LEN or greater.
;;
(define (lpad lst w len)
  (._. (nth-rest (words (._. lst 0)) (repeat-words w len))
      lst))


;; X = number to format as fixed-point
;; DW = minimum width of field [decimal]
;; DP = number of digits after the decimal point [decimal]
;; UP = (d2u DP)
;;
(define (fp-fix x dw dp up)
  &public
  (define `uf (fp.uf x))
  (define `ue (fp.xpo x))

  (define `digits
    (foreach (de (u2d ue))
      (define `(umin ua ub)
        (if (u-lt? ua ub) ua ub))

      ;; FRAC = digits to the right of the decimal point
      (define `frac
        (if (n>0? de)
            (nth-rest de (fp.uf<<1 x))
            ;; Pad with -E zeros on left... but watch out for VERY big -E.
            (._. (u-zeros (umin (abs ue) up))
                 uf)))

      (.. (or (uf-fix uf de) 0)                    ; left of decimal
          (if (n>0? dp)
              (.. " . " (uf-fix frac dp))))))      ; right of decimal

  (define `text
    (if x
        (._. (filter "-" x) digits)
        "N a N"))

  (u2d (concat-vec (lpad text [" "] dw))))
