(require "core.scm")
(require "mcore.scm") ;; ensure it's tested first
(require "mcore.scm" &private) ;; get all symbols


(define `(+_+ a b)
  (concat a " " b))


(define `(u2d-macro u)
  (subst "0111" 3 "3111" 6 "611" 8 "81" 9 "61" 7
         "311" 5 "31" 4 "011" 2 "01" 1 u))


(define `(d2u-macro d)
  (subst 1 "01" 2 "011" 4 "31" 5 "311"
         7 "61" 9 "81" 8 "611" 6 "3111" 3 "0111" (or d "?")))


(define `(uf-lt? a b)
  (findstring 1 (uf-cmp b a)))


(define `(u-lt? a b)
  (findstring 1 (u-cmp b a)))


(define `(u>0? u)
  (filter "1%" (subst 0 nil u)))


(define `(u<0? u)
  (findstring "-1" (subst 0 nil u)))


;;----------------------------------------------------------------
;; FP operations
;;----------------------------------------------------------------

(define `FP0 "0 + 0")
(define `FP1 "01 + 01")

;; True if POD (Place-or-Digits) contains a "place" value: the number of
;; digits to the right of the decimal place, U-encoded.
;;
(define `(pod-is-place? pod)
  (filter "0% -0%" pod))


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
;;
;; POD = precision in internal place-or-digits encoding:
;;    Begins with "0" or "-0" => place, negated, and in signed U-encoded form
;;    Begins with 1-9 => number of significant digits, in decimal
;;    nil => error (return nil)
;;
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


;;----------------------------------------------------------------
;; Operators
;;----------------------------------------------------------------

;; RAW values are strings from the client in which decimal digits have been
;; converted to unary digits (0, 01, 011, ...), and nil values have been
;; replaced by "?".


;; Functions beginning "raw-" generally accept RAW values and return
;; U-numbers or "NaN".
;;
(declare (raw-))


(define (undef ...args)
  NaN)


;; Return non-nil if either A or B are not simple integers: "-"? DIGIT+
;; Assumes that neither A nor B are nil.
;;
(define `(non-integers? a b)
  (subst "9-0" nil 9 nil 1 nil 0 nil (concat 9 a 9 b)))


;; Return non-nil if A or B are not simple non-negative integers: DIGIT+
;;
(define `(non-naturals? a b)
  (non-digit? (concat a b)))


;; Add B to A.
;;
(define (raw-add a b)
  (if (non-integers? a b)
      (fp2u (fp-add (u2fp a) (u2fp b)))
      (u-add a b)))


;; Subtract B from A.
;;
;; For subtraction, this more complicated (than raw-add) is 10% faster on
;; average.
;;
(define (raw-sub a b)
  (if (non-digit? (concat a b))
      (if (non-integers? a b)
          (fp2u (fp-sub (u2fp a) (u2fp b)))
          (u-sub a b))
      (u-sub-unsigned a b)))


(define `(u-mul-uns a b)
  (u-norm-uns (smash (uf-mul (strip (spread a)) (strip (spread b))))))


(define (raw-mul-s a b)
  (if (non-integers? a b)
      (fp2u (fp-mul (u2fp a) (u2fp b)))
      (patsubst
       "-0" 0
       (concat (filter "-" (concat (findstring "-" a) (findstring "-" b)))
               (u-mul-uns (subst "-" nil a) (subst "-" nil b))))))


;; Multiply A by B.
;;
;; A & B are "raw U" strings: all decimal digits have been converted to
;; unary (0, 01, 011, ...); other characters remain as-is; empty string
;; has been replaced with "?".
;;
(define (raw-mul a b)
  (if (non-naturals? a b)
      (raw-mul-s a b)
      (u-mul-uns a b)))


;; Validate PREC (non-zero positive integer), or provide default.
;; Return PREC is valid or defaulted, nil if PREC is invalid.
;;
;; "+NNN", "-NNN", "0", or positive integer.
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


;; Divide A by B, rounding down to the nearest integer (flooring).
;;
(define (raw-fdiv a b)
  (if (non-digit? (concat a b))
      (fp2u (fp-div (u2fp a) (u2fp b) "0" nil))
      (u-fdiv a b DIV-TRUNCATE)))


;; Return A modulo B -- the remainder after (fdiv A B).
;;
(define (raw-mod a b)
  (if (non-digit? (concat a b))
      (fp2u (fp-mod (u2fp a) (u2fp b)))
      (u-fdiv a b DIV-REMAINDER)))


;; Round X
;;
;; X is decimal (raw input from client).
;; POD is in internal format (0% => place, else decimal)
;; DIR = one of {DIV-TRUNCATE, -FLOOR, -NEAREST, -CEILING}
;;
;; Result is decimal.
;;
(define (raw-round x pod dir)
  (fp2d (fp-round (d2fp x) pod dir)))


(define `(check-dir dir)
  (or (if dir
          (filter [DIV-FLOOR DIV-CEILING DIV-TRUNCATE]
                  (subst "-" DIV-FLOOR "+" DIV-CEILING "|" DIV-TRUNCATE dir)))
      DIV-NEAREST))


;; Compare A to B.
;; Out:  If A = B, nil.
;;       If A > B, a word consisting of one or more "1" characters.
;;       If A < B, a word consisting no "1" characters.
;;
;;
(define (raw-cmp a b)
  (if (non-digit? (concat a b))
      (fp-cmp (u2fp a) (u2fp b))
      (u-cmp-unsigned a b)))


(define (math-cmp a b)
  (raw-cmp (d2u-macro a) (d2u-macro b)))


(define (raw-pwr a b)
  (if (non-digit? b)
      ;; B must be non-negative integer
      NaN
      ;;
      (fp2u (fp-pwr (u2fp a) b))))


(define (binop name a b)
  (u2d-macro
   (call (concat (native-name raw-) name)
         (d2u-macro a)
         (d2u-macro b))))


(define (prec-op name x y p ?arg4)
  (declare (fp-))
  (u2d-macro
   (fp2u
    (call (concat (native-name fp-) name)
          (u2fp (d2u-macro x))
          (u2fp (d2u-macro y))
          (prec-to-pod p)
          arg4))))


;;----------------------------------------------------------------
;; Exports
;;----------------------------------------------------------------

;; Arithmetic Functions
;;
;; Numbers are represented as strings of decimal digits, with an optional
;; sign, decimal point, and E-notation suffix.  More precisely:
;;
;;    Number := "-"? Digit+ ("." Digit+)? Exp?
;;    Digit  := "0" | "1" | ... | "9"
;;    Exp    := ("E" | "e") ("+" | "-" | "") Digit+
;;
;; Strings not conforming to the above syntax are treated as non-number
;; values.  When numeric operators are given non-number values, they return
;; "NaN".  Passing a zero divisor to the division or modulo functions
;; results in a "NaN" result.  Comparison operators accept non-number values
;; and treat them as less than all numeric values, but equivalent to other
;; non-number values.
;;
;; The `math` library implements arbitrary-precision arithmetic.  There is
;; no limit imposed on the size of numbers -- except for the memory required
;; to represent them in the above format.  For most operators -- +, -, *,
;; mod -- the result is always numerically exact.  The division operator
;; returns a specified maximum of digits.
;;

;; Return X + Y.
;;
(define `(+ x y)
  &public
  (binop "add" x y))


;; Return X - Y.
;;
(define `(- x y)
  &public
  (binop "sub" x y))


;; Return X * Y.
;;
(define `(* x y)
  &public
  (binop "mul" x y))


;; Return floor(X / Y): the largest integer less than or equal to X/Y.
;;
(define `(// x y)
  &public
  (binop "fdiv" x y))


;; Return X / Y to a precision specified by P.
;;
;; Precision can be specified in terms of a place or a number of digits.
;; When P begins with "+" or "-" followed by an decimal integer N, the least
;; significant digit in the result will be in the 1E<P> place.  When P is a
;; postive decimal integer, the result will contain P significant digits
;; (counting from the most significant non-zero digit).  [No "." or
;; E-notation is allowed within P.]
;;
;; P defaults to 16.  16 significant digits will provide slightly higher
;; precision than 64-bit IEEE-754 floating point numbers.
;;
;; Examples:
;;
;;   (div 200 3 5)  ->  66.666
;;   (div 200 2 -1) ->  66.7
;;   (div 200 2 "+0") ->  67
;;   (div 200 2 "+1") ->  70
;;   (div 200 2 "+2") -> 100
;;   (div 200 2 "+3") ->   0
;;
(define `(/ x y ?p)
  &public
  (prec-op "div" x y p 1))


;; Compute X*Y to the precision given by P.
;;
;; The result should be within one unit of the least significant digit (as
;; specified by P).  P is as documented for `/`.
;;
(define `(*~ x y ?p)
  &public
  (prec-op "mulp" x y p))


;; Raise X to the power of Y.  Y must be an non-negative integer
;; in "simple" format (without a decimal point or E-notation).
;;
(define `(^ x y)
  &public
  (binop "pwr" x y))


;; Return the remainder of floor(X/Y).
;;
;;   (mod X Y) = (- X (* (// X Y) Y))
;;
(define `(mod x y)
  &public
  (binop "mod" x y))


;; Return the greatest integer less than or equal to X.
;;
(define `(floor x)
  &public
  (raw-round x 0 DIV-FLOOR))


;; Return the smallest integer greater than or equal to X.
;;
(define `(ceil x)
  &public
  (raw-round x 0 DIV-CEILING))


;; Return the integer portion of X (rounding towards zero).
;;
(define `(trunc x)
  &public
  (raw-round x 0 DIV-TRUNCATE))


;; Round X to a specified decimal place or number of significant
;; digits.  PREC is as specified for `/`.
;;
;; DIR is one of the following:
;;   "+" => round up to nearest unit (ceiling)
;;   "-" => round down to nearest unit (floor)
;;   "|" => round towards zero to nearest unit (truncate)
;;
(define (round x ?prec ?dir)
  &public
  (raw-round x (if prec (prec-to-pod prec) 0) (check-dir dir)))


;; Return 1 if X > Y, nil otherwise.
;;
(define `(> x y)
  &public
  (findstring 1 (math-cmp x y)))


;; Return 1 if X < Y, nil otherwise.
;;
(define `(< x y)
  &public
  (> y x))


;; Return 1 if X != Y, nil otherwise.
;;
(define `(!= x y)
  &public
  (if (math-cmp x y) 1))


;; Return 1 if X = Y, nil otherwise.
;;
(define `(= x y)
  &public
  (if (math-cmp x y) nil 1))


;; Return 1 if X >= Y, nil otherwise.
;;
(define `(>= x y)
  &public
  (not (< x y)))


;; Return 1 if X <= Y, nil otherwise.
;;
(define `(<= x y)
  &public
  (not (> x y)))


;;----------------------------------------------------------------
;; Misc.
;;----------------------------------------------------------------


;; Negate a number.  This function assumes that X is a valid number; it does
;; not validate or canonicalize X.
;;
(define `(0- x)
  &public
  (subst "--" "" (concat "-" x)))


;; Absolute value of a number.  This function assumes that X is a valid
;; number; it does not validate or canonicalize X.
;;
(define `(abs x)
  &public
  (patsubst "-%" "%" x))


;; Return the larger of A or B.
;;
(define (max a b)
  &public
  (if (< a b)
      b
      a))


;; Return the smaller of A or B.
;;
(define (min a b)
  &public
  (if (> a b)
      b
      a))


(define (fp-sum v)
  (if (word 2 v)
      (fp-add (first v) (fp-sum (rest v)))
      (first v)))


(define (sum-vec nums)
  (if (findstring "!" nums)
      ;; vectors...
      (if (findstring "!0" (subst "!1" "!0" nums))
          (sum-vec (promote nums)))
      (fp2d (fp-sum (for u (d2u nums) (u2fp u))))))


;; Return the sum of all arguments.  Arguments may be numbers, or vectors of
;; numbers, or vectors or vectors of numbers, and so on.
;;
(define (sum ...args)
  &public
  (sum-vec (promote args)))


;;----------------------------------------------------------------
;; range
;;----------------------------------------------------------------


;; SKIP-START and SKIP-END are U digits giving the number of words
;; to trim from the start and end of the list.
;;
(define (uv-trim skip-start skip-end lst)
  (define `(u2d-digit d)
    (words (subst 0 nil 1 "1 " d)))

  (wordlist (u2d-digit (concat skip-start skip-end 1))
            (words lst)
            (+_+ (subst 0 nil 1 "1 " skip-end) lst)))


;; MIN and MAX are unsigned UV numbers.
;;
(define (uv-range min max)
  (define `(uv/10 x)
    (filter-out "%x" (concat x "x")))

  (define `(x10 lst)
    (foreach n lst
             (concat n "0 " n "1 " n "2 " n "3 " n "4 "
                     n "5 "n "6 " n "7 " n "8 " n "9 ")))

  (if (u-lt? max min)
      nil
      (uv-trim (lastword min)
               (subst (lastword max) "0" U9)
               (concat
                (if (not (word 2 min))
                    "0 1 2 3 4 5 6 7 8 9 ")
                (x10 (uv-range (or (uv/10 min) 01) (uv/10 max)))))))


;; UA, UB = absolute value of A and B (UV numbers)
;; A<0, B<0 = truthy when A, B are negative
;;
(define (uv-sign-range ua ub a<0 b<0)
  (concat
   ;; Negative range: A ... min(B,-1)
   (if a<0 (concat (addprefix "-" (reverse (uv-range (if b<0 ub 1) ua))) " "))
   ;; Non-negative range: max(A,0) ... B
   (if b<0 nil (uv-range (if a<0 0 ua) ub))))


(define `(fp-range a b)
  nil)


;; Remove sign and extraneous leading zeros.
;;
(define (u-to-uv u)
  (or (strip (subst "0" " 0" (filter "01%" (subst "-" nil "01" " 01" u))))
      0))


(define (raw-range a b)
  (strip
   (if (non-integers? a b)
       (fp-range (u2fp a) (u2fp b))
       (uv-sign-range (u-to-uv a) (u-to-uv b) (u<0? a) (u<0? b)))))


;; Return a vector of numbers ranging from A to B.  A and B must be integers
;; in "simple" format (without a decimal point or E-notation).
;;
(define (range a b)
  &public
  (raw-range (d2u a) (d2u b)))


;;----------------------------------------------------------------
;; format-fixed
;;----------------------------------------------------------------


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


;; Convert X to a fixed-point representation.
;;
;; MIN-WIDTH = if non-nil, minimum field width.  Padding with spaces on the
;;    left will be added as necessary.
;; PRECISION = if non-nil, number of digits to the right of the decimal.
;;
(define (format-fixed x ?min-width ?precision)
  &public
  (foreach
      p (if precision (d2u precision) "?")

      (if (non-naturals? (if precision p)
                         (if min-width (d2u min-width)))
          (if (and precision (non-digit? p))
              "[invalid_PRECISION]"
              "[invalid_MIN-WIDTH]")
          (fp-fix (fp-round (d2fp x) (if precision p 0) DIV-NEAREST)
                  min-width
                  precision
                  p))))


;;----------------------------------------------------------------
;; num-lex, num-sort
;;----------------------------------------------------------------

;; The lexical form of a number X is one of the following:
;;     "0"                X == 0
;;     LEXP FRAC          X > 0
;;     "-" ~(LEXP FRAC)   X < 0
;;
;; FRAC is the fractional portion of X.
;;
;; `~N` refers to the 9's complement of N.
;;
;; LEXP is one of the following depending on E, the exponent of X:
;;     E            E in {1..8}
;;     "9" E        E in {09..89}
;;     "99" E       E in {090...899}
;;     "9"... E     E>1, generally: prefix one 9 for each digits in E/9
;;     "0" ~E       E in {-8..0}
;;     "00" ~E      E in {-98..-09}
;;     "0"... ~E    E<1, generally: prefix one 0 for each digit in -E/0.9


;; Return the 9's complement of a U value.
;;
(define `(u-complement-digits u)
  (neg-rreduce (subst 1 "~" 0 U9 u)))


(define `(lex-uns u)
  ;; If initial digit of E begins with 9, add another digit
  (subst (concat 9 U9) "9909"
         9 U9
         (concat (subst 1 nil 0 9 u) u)))


(define (lex-exp u)
  (if (u>0? u)
      (patsubst "0111111111%" "%" (lex-uns u))
      (u-complement-digits (lex-uns (subst "-" nil u)))))


(define (fp-lex n)
  (if (fp<0? n)
      (concat "-" (u-complement-digits (fp-lex (fp-negate n))) ":")
      (if (findstring 1 (fp.uf n))
          (concat (lex-exp (fp.xpo n)) (fp.uf n))
          "0")))


;; Convert numbers to strings such that the lexical sort order of the output
;; strings corresponds to the numeric sort order of the input numbers.
;;
;;    (< a b) <==> (string< (num-lex a) (num-lex b))
;;
;; This can be used with `sort-by` to obtain numeric sort order.  E.g.:
;;
;;   (sort-by (lambda (i) (num-lex (nth 2 i))) ...)
;;
(define (num-lex n)
  &public
  (u2d (smash (fp-lex (d2fp n)))))


;; Sort elements of V by the numeric order of the first sub-element.  V may
;; be a simple list of numbers, or vector of vectors to be sorted by the
;; first element of each, or a dictionary to be sorted by the numeric value
;; of each key.
;;
(define (num-sort v)
  &public
  (define `prefixed-v
    (foreach elem v
             (concat (num-lex (word 1 (subst "!" " " elem)))
                     "!#"
                     elem)))
  (filter-out "%!#" (subst "!#" "!# " (sort prefixed-v))))
