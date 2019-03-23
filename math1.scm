;;----------------------------------------------------------------
;; math1: FP encoding and operations
;;----------------------------------------------------------------

(require "core.scm")
(require "math0.scm" &private)


;; Construct an FP number
;;
(define `(make-fp exp sgn frac)
  (concat exp " " sgn " " frac))


(define `(fp.xpo n) (word 1 n))
(define `(fp.sign n) (word 2 n))
(define `(fp.uf n) (nth-rest 3 n))


(define `NaN "NaN")

;; In math.scm this is exported as &public
(define `(abs x)
  (patsubst "-%" "%" x))


(define `(non-digit? u)
  (subst 0 "" 1 "" u))


;; U-strings cannot contain 2; they have been converted to "011".
(define `(u-begins? prefix u)
  (findstring (concat 2 prefix) (concat 2 u)))


(define `(u-rm-prefix prefix u)
  (subst (concat 2 prefix) "" (concat 2 u)))


;; Remove SUFFIX from the end of U if U ends with SUFFIX.  Otherwise,
;; concatenate "2" to U.
;;
(define `(u-rm-suffix suffix u)
  (subst (concat suffix 2) "" (concat u 2)))


;; Normalize N (ensure that UF >= 0.1 unless N is zero).
;;
(define (fp-norm x)
  (if (filter 0 (word 3 x))
      (foreach
          zeros (word 1 (subst "001" "0 01" (smash (fp.uf x))))

          (define `zeros-tally
            (subst 0 1 zeros))

          (define `norm-uf
            (subst (concat 9 (spread zeros)) nil (concat "9 " (fp.uf x))))

        (concat (u-sub (fp.xpo x) (u-add-ones "0" zeros-tally))
                " " (fp.sign x) (or norm-uf " 0")))
      x))


(declare (u2fp u ?exp ?sgn ?ufrac))


(define (u2fp-exp u)
  (foreach
      s (lastword (subst "E" " E" u))
      (foreach
          en (subst "E" "" (subst "E+" "E" (subst "+-" "X" s)))
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
  (cond
   ;; simple integer
   ((not (non-digit? u))
    (if u
        (if (or n-exp (word 10 (spread u)) (filter "00%" (concat u "0")))
            (fp-norm (concat (u-add-ones (or n-exp 0) (subst 1 nil 0 1 u))
                             " " (or n-sign "+")
                             (spread u) n-frac))
            (concat "0" (subst 1 nil 0 1 u)
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
    (foreach f (lastword (subst "." " " u))
             (if (non-digit? f)
                 nil
                 (u2fp (subst "." "x" (u-rm-suffix (concat "." f) u))
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
  (define `exp (word 1 fp))
  (define `sgn (word 2 fp))
  (define `frac (nth-rest 3 fp))
  (define `frac-msd (word 3 fp))
  (define `frac*10 (nth-rest 4 fp))

  (cond
   ((findstring 1 frac-msd)
    ;; We use fixed point when 1e-6 <= FP < 1e21; "E" notiation otherwise.
    ;; FP is in this range when -5 <= EXP <= 21.  We reduce EXP to a string
    ;; with which we can quickly check that range.  We also use the string
    ;; for padding 0's on the left/right in the fixed point representation.

    (define `uv-result
      (foreach
          et (or (subst "10" "091" "10" "91" 0 nil exp)
                 "-")

          (if (findstring et "-11111 91911 91111111111")
              ;; Fixed point  (-5 <= EXP <= 21)
              (if (filter "-%" et)
                  ;; -5...0 => EXP is number of zeros left of first digit
                  (concat "0. " (subst 1 "0 " "-" nil et) frac)
                  ;; 1..21 => EXP is number of digits left of decimal
                  (subst "10" "1" "00" "0"
                         (join frac (concat (subst 9 T9 1 " 0" et) "."))))
              ;; "E" notation
              (concat frac-msd ". " frac*10
                      ;; Two spaces before "e" allow preceding redundant
                      ;; zeros to be trimmed; "." after the exponent
                      ;; preserves its trailing zeros.
                      "  e" (subst "+-" "-" (concat "+" (u-1 exp))) "."))))


    (concat (findstring "-" sgn)
            ;; Remove trailing 0's after "." and trailing "." after digits
            ;; (and possible preceding "e+/+XX").  Remove spaces.
            (subst ". 0" ".0" ". " nil " " nil
                   (concat (filter-out "%0" (subst "0 " "0" uv-result))
                           " "))))

   ;; normalized?
   ((findstring 1 frac)
    (fp2u (concat (u-1 exp) " " sgn " " frac*10)))

   (fp 0)

   (else NaN)))


;;--------------------------------
;; PREC and POD
;;--------------------------------

;; Validate PREC (providing default when nil) and return POD value.
;;
;; PREC = positive integer => number of digits of precision
;;        "+NNN", "-NNN", "0" => place of lest significant digit
;; Result = place-or-digits (POD) encoding:
;;   Begins with "0" or "-0" => place, negated, and in signed U-encoded form
;;   Begins with 1-9 => number of significant digits, in decimal
;;   nil => mal-formed PREC error
;;
(define (prec-to-pod prec)
  (if prec
      (if (filter "+% -%" prec)
          ;; If PREC contains spaces, return nil.
          (foreach w (d2u (findstring prec (word 1 prec)))
                   (if (filter-out "+ -" (subst 0 nil 1 nil w))
                       ;; Contains non-digit, non-sign characters
                       nil
                       (subst "-" nil "+" "-" w)))
          (if (word-index? prec)
              (if (filter "0%" prec)
                  (prec-to-pod (patsubst "0%" "%" prec))
                  prec)))
      ;; default
      16))


;; True if POD (Place-or-Digits) contains a "place" value: the number of
;; digits to the right of the decimal place, U-encoded.
;;
(define `(pod-is-place? pod)
  (filter "0% -0%" pod))


;;--------------------------------
;; FP operations
;;--------------------------------

(define `FP0 "0 + 0")
(define `FP1 "01 + 01")

(define `(fp-negate n)
  (subst " +" " !" " -" " +" "!" "-" n))


(define `(fp>0? fx)
  (findstring "+ 01" (wordlist 2 3 (filter-out 0 (concat "." fx)))))


(define `(fp<0? fx)
  (findstring "- 01" (wordlist 2 3 (filter-out 0 (concat "." fx)))))


(define `(fp!=0? fx)
  (findstring 1 (fp.uf fx)))


(define `(fp-abs fx)
  (subst " -" " +" fx))


(define `(d2fp dx)
  (u2fp (d2u dx)))


(define `(fp2d fx)
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
(define (tally w)
  (if (word 2 w)
      (tally (concat (subst 1 T10 (word 1 w)) (rest w)))
      (subst 0 nil w)))


(define `(pad ui)
  (subst 1 "0 " (tally (spread ui))))


;; Note: AE must be >= BE
(define (fp-add-x ae as af be bs bf)
  (define `b-pad
    (and (filter-out ae be)
         (pad (u-sub ae be))))

  (if (findstring as bs)
      ;; sign is same => add
      (make-fp<<1 ae as (uf-add (>>1 af) (>>1 (concat b-pad bf))))

      ;; difference
      (+_+ ae (uf-sign-sub af (concat b-pad bf) (findstring "-" as)))))


;; Add A to B.
;;
(define (fp-add a b)
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
(define `(fp-sub a b)
  (fp-add a (fp-negate b)))


(define `(sign-mul sign-a sign-b)
  (if (findstring sign-a sign-b) "+" "-"))


;; Multiply A by B.
;;
(define (fp-mul a b)
  (define `exp (u-add (fp.xpo a) (fp.xpo b)))
  (define `sgn (sign-mul (fp.sign a) (fp.sign b)))
  (define `m (uf-mul (fp.uf a) (fp.uf b)))
  (and a b (make-fp exp sgn m)))


;; fp-mulp: Compute X*Y to the precision given by POD (see fp-div).
;;
;; X & Y must be normalized (no leading zeros in UF).
;;
(define (fp-mulp-x exp sgn uf pod num-digits)
  ;; If pod is given in terms of *significant* digits and the first result
  ;; digit is zero, use NUM-DIGITS digits *after* the first digit.
  (define `skip-zero
    (and (filter-out "-0% 0%" pod)
         (filter 0 (word 1 uf))))

  (make-fp exp sgn
           (concat (wordlist 1 num-digits uf)
                   (if skip-zero
                       (addprefix " " (word 2 (nth-rest num-digits uf)))))))


(define (fp-mulp x y pod)
  (if (and pod x y)
      (foreach
          exp (u-add (fp.xpo x) (fp.xpo y))
          (foreach
              num-digits (if (pod-is-place? pod)
                             (u2d (u-add exp pod))
                             pod)

              ;; get up to NUM_DIGITS+1 digits of A
              (define `(in-digits a)
                (concat (word 3 a)
                        (if (word 4 a)
                            (concat " " (wordlist 1 num-digits (nth-rest 4 a))))))

              (if (filter "-% 0" num-digits)
                  ;; Rounding place is to the left of EXP => 0
                  FP0
                  ;; Non-zero num-digits
                  (fp-mulp-x exp
                             (sign-mul (fp.sign x) (fp.sign y))
                             (uf-mul (in-digits x) (in-digits y))
                             pod
                             num-digits))))))


;; Divide X by Y.
;;
;; X & Y must be normalized (no leading zeros in UF).
;; POD = see prec-to-pod
;; ROUND = nearest (non-nil) or down (nil).  [When POD=0, this means round
;;         down to the largest integer <= the quotient.]
;;
;; Results are not normalized.
;;
;; Notes:
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

(define (fp-div-x q.exp q.sgn x.uf y.uf round pod)
  ;; Now:   Y.UF > X.UF > Y.UF/10

  (define `mode
    (if round
        DIV-NEAREST
        (if (filter "+" q.sgn)
            DIV-TRUNCATE
            DIV-CEILING)))

  (foreach
      num-digits (if (pod-is-place? pod)
                     (u2d (u-add q.exp pod))
                     pod)

      (if (filter "-%" num-digits)
          ;; Rounding place is to the left of Q.EXP => 0 unless mode=CEILING
          (if (filter DIV-CEILING mode)
              (make-fp (u-sub "01" pod) q.sgn "01")
              FP0)

          ;; Non-negative num-digits
          (make-fp<<1 q.exp q.sgn (uf-div x.uf y.uf num-digits mode)))))


(define (fp-div x y pod round)
  (define `x.uf (fp.uf x))
  (define `y.uf (fp.uf y))
  (define `q.exp (u-sub (fp.xpo x) (fp.xpo y)))
  (define `q.exp+1 (u-sub (u+1 (fp.xpo x)) (fp.xpo y)))
  (define `q.sgn (sign-mul (fp.sign x) (fp.sign y)))

  (cond
   ;; Normalized and non-zero?
   ((not (findstring 101 (concat (word 3 x) (word 3 y))))
    (if (findstring 1 y.uf)
        (if (findstring 1 x.uf)
            (fp-div (fp-norm x) (fp-norm y) pod round)
            (if x FP0 nil))
        nil))

   ;; Now: X.UF >= 0.1
   ;; Now: X.UF >= 0.1
   (pod
    (if (uf-lt? x.uf y.uf)
        ;; X.UF < Y.UF
        (fp-div-x q.exp q.sgn x.uf y.uf round pod)
        ;; uf-div requires X.UF < Y.UF
        (fp-div-x q.exp+1 q.sgn (>>1 x.uf) y.uf round pod)))))


;; Return the remainder after flooring division:  R = A - B*floor(A/B)
;;
;; For positive A, B:
;;    R = A - B*floor(A/B)
;;    A/B = AF/BF << (AE-BE)
;;    floor(A/B) = floor(AF/BF << (AE-BE))
;;    (uf-div AF BF N=(AE-BE) DIV-REMAINDER) -->
;;       REM = (AF - BF*Qt) << N
;;        Qt = floor(AF/BF << N) >> N
;;           = floor(A/B) >> N
;;       REM = (AF - BF*floor(A/B) >> AE << BE) << AE >> BE
;;           = (A - B*floor(A/B)) >> BE
;;
(define (fp-mod a b)
  (cond
   ;; Normalized and non-zero?
   ((not (findstring 101 (concat (word 3 a) (word 3 b))))
    (if (findstring 1 (fp.uf b))
        (if (findstring 1 (fp.uf a))
            (fp-mod (fp-norm a) (fp-norm b))
            (if a FP0 nil))
        nil))

   ;; signs differ?
   ((not (findstring (fp.sign a) (fp.sign b)))
    (let ((m (fp-mod a (fp-negate b)))
          (b b))
      (if (findstring 1 (fp.uf m))
          (fp-add b m)
          m)))

   ((u-lt? (fp.xpo a) (fp.xpo b))
    a)

   (else
    (make-fp (fp.xpo b)
             (fp.sign a)
             (uf-div (>>1 (fp.uf a))
                     (fp.uf b)
                     (u2d (u-sub (u+1 (fp.xpo a)) (fp.xpo b)))
                     DIV-REMAINDER)))))


;; Round X to specified precision
;;
(define (fp-round x pod dir)
  ;; Convert signed rounding direction to unsigned direction
  (define `udir
    (or (filter [DIV-NEAREST DIV-TRUNCATE] dir)
        ;; FLOOR or CEILING
        (if (findstring (fp.sign x) (if (filter DIV-CEILING dir) "+" "-"))
            DIV-CEILING
            DIV-TRUNCATE)))

  (cond
   ((findstring 1 (word 3 x))
    ;; return NIL if POD is NIL
    (foreach
        num-digits (if (pod-is-place? pod)
                       (u2d (u-add (fp.xpo x) pod))
                       pod)
        (if (filter "-%" num-digits)
            ;; Rounding place is to the left of EXP => 0 unless mode=CEILING
            (if (filter DIV-CEILING udir)
                (make-fp (u-sub "01" pod) (fp.sign x) "01")
                FP0)

            (make-fp<<1 (fp.xpo x)
                        (fp.sign x)
                        (if (filter 0 num-digits)
                            (rest (uf-round 1 udir (>>1 (fp.uf x))))
                            (uf-round num-digits udir (fp.uf x)))))))

   ((findstring 1 (fp.uf x))
    (fp-round (fp-norm x) pod dir))

   (x
    ;; return NIL when X is NIL
    FP0)))


;; Truncate FX (towards zero).
;;
(define (fp-trunc fx)
  (if (u>0? (fp.xpo fx))
      ;; Avoid using EXP as a word index if it's too large
      (if (word 9 (spread (fp.xpo fx)))
          fx
          (wordlist 1 (u2d (u-add-ones (fp.xpo fx) 11)) fx))
      (if fx FP0)))


;; Raise N to the next integer equal to or larger (away from zero).
;;
(define (fp-mag-ceiling n)
  (if (u>0? (fp.xpo n))
      (let ((tr (fp-trunc n))
            (n n))
        (if (findstring 1 (subst tr nil n))
            ;; round magnitude up
            (make-fp<<1 (fp.xpo tr)
                        (fp.sign tr)
                        (uf-carry (>>1 (concat (fp.uf tr) 1))))
            tr))
      (if (findstring 1 (fp.uf n))
          (make-fp "01" (fp.sign n) "01")
          (if n FP0))))


(define (fp-floor n)
  (cond
   ((findstring " -" n) (fp-mag-ceiling n))
   (n (fp-trunc n))))


(define (fp-ceil n)
  (cond
   ((findstring " +" n) (fp-mag-ceiling n))
   (n (fp-trunc n))))


;; ASSERT: A and B are normalized (fp.uf >= 0.1)
;;
(define (fp-cmp a b)
  (cond
   ;; Both are positive
   ((findstring (findstring "+ 01" a) b)
    (or (u-cmp (fp.xpo a) (fp.xpo b))
        (uf-cmp (fp.uf a) (fp.uf b))))

   ;; Treat NaN as less than everything else
   ((not (and a b))
    (if a 1 (if b "~")))

   ;; A=0 ?
   ((not (findstring 1 (word 3 a)))
    (if (findstring 1 (word 3 b))
        (if (filter "-" (fp.sign b)) 1 "~")
        nil))

   ;; B=0 ?
   ((not (findstring 1 (word 3 b)))
    (if (filter "-" (fp.sign a)) "~" 1))

   ((fp<0? a)
    (if (fp<0? b)
        (fp-cmp (fp-negate b) (fp-negate a))
        "~"))

   (else 1)))


(define `(fp-lt? fa fb)
  (findstring 1 (fp-cmp fb fa)))


;; Return X*X.
;;
(define (fp-sq x)
  (fp-mul x x))


;; Raise X to the power of N.
;;
;; X = FP number
;; N = U-encoded non-negative integer
;;
(define (fp-pwr x n)
  (foreach
      n/2 (subst "11" 2 "10" "022222" n)

      (if (findstring 2 n/2)      ;; n > 1?
          (if (findstring 1 n/2)  ;; n is odd?
              (fp-mul (fp-sq (fp-norm (fp-pwr x (subst 1 nil 2 1 n/2)))) x)
              (fp-sq (fp-norm (fp-pwr x (subst 2 1 n/2)))))
          (if (findstring 1 n)
              x
              FP1))))


;;--------------------------------
;; fp-fix
;;--------------------------------

(define `(n>0? n)
  (filter-out "-%" (subst 0 nil n)))


(define (extend-fn lst len zs)
  (if (subst 0 nil len)
      (if (word len (concat lst zs))
          (wordlist 1 len (concat lst zs))
          (extend-fn (concat lst zs zs) len (concat zs zs)))))


(define `(nzeros len)
  (extend-fn 0 len " 0 0 0 0"))


;; Return a list LEN words long by trimming words from the end of LST or
;; appending zeros.  LEN is a non-negative decimal integer.  If LEN=0 or
;; nil, empty string is returned.
;;
(define `(uf-fix lst len)
  (extend-fn lst len " 0 0 0 0"))


;; Pad with zeros on left until LST is at least LEN words long.  LEN is
;; non-negative decimal integer.  If LEN=0 or nil, no padding is added.
;;
(define (zero-pad lst len)
  (strip
   (+_+ (if len
            (nth-rest (words (concat lst " 0")) (nzeros len)))
        lst)))


;; Trim extraneous leading zeros from a fixed-point number.
;;
(define (ltrimz u)
  (if (filter "00% 0n% 0-%" u)
      (foreach lz (word 1 (subst "01" " " "0." " " "0n" "0 " "0-" "0 "
                                 (patsubst "%0" "% 0" u)))
               (subst (concat "<" lz) (subst 0 " " lz) (concat "<" u)))
      u))


;; X = number to format as fixed-point
;; WD = minimum width of field [decimal]
;; PD = number of digits after the decimal point [decimal]
;; PU = (d2u PD)
;;
(define (fp-fix x wd pd pu)
  (define `uf (fp.uf x))
  (define `uf<<1 (nth-rest 4 x))

  (define `digits
    (foreach
        e (u2d (fp.xpo x))

        (define `frac
          (if (n>0? e)
              ;; Skip digits left of the decimal point
              (nth-rest e uf<<1)
              ;; Pad with -E zeros on left... but watch out for VERY big -E.
              (if (u-lt? (abs (fp.xpo e)) pu)
                  (+_+ (nzeros (abs e)) uf))))

        (concat
         ;; left of decimal
         (if (n>0? e) (uf-fix uf e) 0)
         ;; right of decimal
         (if pd (concat " . " (uf-fix frac pd))))))

  (define `text
    (if x
        (+_+ (filter "-" x) digits)
        "n a n"))

  (u2d (ltrimz (smash (zero-pad text wd)))))
