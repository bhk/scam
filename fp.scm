;; TODO:
;;  - sum, num-pad
;;  - (div-rem float float)
;;      (div-rem N 1)  -->  [floor(N) N-floor(N)]
;;  - Accept E-notation for integers in: range, ^
;;  - Accept "undefined"?
;;  - round, /%, ...
;;  - fr-exp (return m and e s.t. x = m2ᵉ)
;;  - num-pi(ndigits)
;;  - MSB: make u-cmp, u-norm, and carry less expensive?
;;  - Support "+" prefix
;;  - Parsing:  allow "+DIGITS" and "E+EXP" in SCAM numbers

(require "core")

;; Various encodings of numbers are used by this module:
;;
;; U: This encoding represents non-negative integers in base 10, using one
;;    word per digit in little-endian form.  Each word consists of a ":"
;;    followed by 0-9 "i" characters:
;;
;;       ": :i :ii"  =>  210
;;       ": : :"     =>  000
;;
;;    In this encoding, `(join A B)` can be used to add corresponding
;;    digits, and `subst` can be used multiply and to to carry digit
;;    overflow to the next higher digit.
;;
;; SU: This encoding represents integers as U-encoded values that are
;;    prefixed with "- " when the value is negative.
;;
;; F: This encoding represents a floating point number as three values: "EXP
;;    SIGN MUL". EXP is the exponent as a decimal integer.  SIGN is "+" or
;;    "-". MUL is the absolute value of the multiplier, U-encoded.  The
;;    numeric value is given by: MUL * 10^EXP * (SIGN=="-" ? -1 : 1)
;;
;;       "0 - :ii : :iii"     ==>  -302
;;       "-10 + :iii :ii :i"  ==>  1.23e-8
;;
;; I: Decimal integer: A subset of numbers (those that avoid "." and "e").
;;
;; N: Number: The format accepted and returned by public functions.
;;
;;       "1",  "-0.032e32"
;;
;;       I  <-  SIGN? DIGIT+
;;       N  <-  I ( "." DIGIT* )? ( E I )?
;;           |  "undefined"
;;       E  <-  "e" | "E"
;;       DIGIT  <-  "0" | "1' | ... | "9"
;;       UDIGIT  <- "." "i"*
;;
;; Function prefixes and encodings:
;;
;;    +  : (N, N) -> N
;;    i+ : (I, I) -> I
;;    f+ : (F, F) -> F
;;    u+ : (U, U) -> U
;;

;;----------------------------------------------------------------
;; Utilities
;;----------------------------------------------------------------

;; Return string LEN words long, constructed by repeating W.
(define (nwords len w)
  (if (subst "0" "" len)
      (if (word len w)
          (wordlist 1 len w)
          (nwords len (concat w " " w " " w)))))

(define `(nzeros e)
  (subst " " "" (nwords e "0 0 0")))

(define `(abs n)
  &public
  (patsubst "-%" "%" (subst "+" "" n)))

;;----------------------------------------------------------------
;; U encoding
;;----------------------------------------------------------------

;; Return NIL if U is zero.
(define `(u-nonzero? u)
  (findstring "i" u))

(define `(u-zero? u)
  (not (findstring "i" u)))

;; Return "i" if U is odd, `nil` otherwise.
;;
(define `(u-odd? u)
  (findstring "i" (subst "ii" "" (word 1 u))))

;; Convert from N to U encoding.  Strip any sign symbol.
(define (u-enc n)
  (reverse
   (subst "+" "" "-" ""
          "9" "8i" "8" "7i" "7" "6i" "6" "5i" "5" "4i" "4" "3i"
          "3" "2i" "2" "1i" "1" "0i" "0" " :" n)))

(define (u-dec u)
  (subst " " ""
         (reverse
          (or (subst ":" "0" "0i" "1" "1i" "2" "2i" "3" "3i" "4" "4i" "5"
                     "5i" "6" "6i" "7" "7i" "8" "8i" "9" u)))))

;; Remove extraneous most-significant zeros from U-encoded numbers.
;; trim MSB zeroes
(define (u-norm u)
  (strip (subst ":" " :" (butlast (subst " " "" "i:" "i :"
                                         (concat u ":"))))))

;; Convert a U-encoded value to a non-negative decimal integer,
;; normalized (without extraneous significant 0's).
;;
(define `(u-dec-norm u)
  (or (subst ":" "0" "0i" "1" "1i" "2" "2i" "3" "3i" "4" "4i" "5"
             "5i" "6" "6i" "7" "7i" "8" "8i" "9" " " ""
             (rest (subst " " "" ":i" " :i" (concat ":" (reverse u)))))
      0))

;; Decimal points: If there is a decimal point, u-enc preserves it.
;; Arithmetic functions do not recognize decimal points; they are only
;; present in (otherwise) U-encoded strings when used by f-enc and f-dec.

;; Return the number of digits "below" the decimal point ("to the right of"
;; in ordinary decimal notation).
;;
(define `(u-find-point u)
  (if (findstring "." u)
      (words (subst "_" " " (word 1 (subst " " "_" "." "_ ." u))))
      0))

;; Insert a decimal point "above" EXP digits, extending U as necessary.  In
;; other words, EXP is the number of fracional digits.
;;
(define `(u-add-point u exp)
  ;; Construct U-encoded 0.0000 (with EXP trailing 0's)
  (define `zpz (concat (nwords (abs exp) ": : :") " .:"))
  (subst "::" ":" "." ". " (join zpz u)))


;; Count number of consecutive least-significant (trailing) zeroes in U.
(define (u-ctz u)
  (words (subst "_" " " (word 1 (concat "_" (subst " " "_" ":i" " " u))))))

;; For digits greater than 9, propagate values to the next higher digit,
;; *IF* there is a "next" digit.
(define (u-carry-n n)
  (while (lambda (x) (findstring "iiiiiiiiii :" x))
         (lambda (x) (subst "iiiiiiiiii :" " :i" x))
         n))

(define (u-carry-check x)
  (if (findstring "iiiiiiiiii :" x)
      (u-carry-n (subst "iiiiiiiiii :" " :i" x))
      x))

(define `(u-carry x)
  (u-carry-check (subst "iiiiiiiiii :" " :i" x)))

;; Add two U-encoded values.  Add a most-significant zero to accommodate
;; carry.
(define (u+ a b)
  &inline
  (u-carry (concat (subst "i:" "i" "::" ":" (join a b)) " :")))

(define (u* a b)
  (u+ (subst "i" (subst ":" "" (firstword a)) b)  ;; "big" carry (4,2,1)
      (if (word 2 a)
          (u* (rest a) (concat ": " b)))))

;; Compare two unsigned encoded numbers, returning:
;;   1 if a > b
;;   2 if b > a
;;   nil otherwise
(define (u-cmp a b)
  (if (or a b)
      (or (u-cmp (rest a) (rest b))
          (if (findstring (concat (word 1 b) "i") (word 1 a)) 1)
          (if (findstring (concat (word 1 a) "i") (word 1 b)) 2))))

(define `(u< a b)
  (filter 2 (u-cmp a b)))

;; Make U have DIGITS digits by appending 0's to most-significant end.
(define (u-extend u digits)
  (subst "::" ":" (join (nwords digits ": : :") u)))

;; Replace each digit D in U with 9-D, then add one.  The result is
;; 10^MAG-U, where MAG is the number of digits in U.  Modulo 10^MAG, the
;; result is simply -U.
(define (u-negate u)
  (join
   (foreach w u (concat ":" (subst w "" ":iiiiiiiii")))
   "i"))

;; Divide by two, rounding down.
(define (u/2 n)
  (u-norm (rest (u-carry (concat (subst "i" "iiiii" n) " :")))))

(define (u^2 n)
  (u* n n))

(define (u^ a b)
  (if (u-odd? b)
      (u-norm (u* a (u^2 (u^ a (u/2 b)))))
      (if (findstring "i" b)
          (u-norm (u^2 (u^ a (u/2 b))))
          ":i")))

;;----------------------------------------------------------------
;; SU & I encoding
;;----------------------------------------------------------------

;; SU:  a U-encoded number optionally preceded by a "-"
(define (su-dec u)
  (if (findstring "-" (word 1 u))
      (concat "-" (u-dec-norm (rest u)))
      (u-dec-norm u)))

;; su-sub: (U,U) --> SU
;; Subtract UB from UA, returning an SU-encoded integer.
;;
(define (su-sub ua ub)
  (let ((total (u+ ua (u-negate (u-extend ub (words ua)))))
        (len (words (join ua ub))))
    (if (u-nonzero? (word len (rest total)))
        (wordlist 1 len total)
        (concat "- " (wordlist 1 len (u-negate total))))))

;; Return "-" if I is negative, `nil` otherwise.
;;
(define `(i-neg? i)
  (findstring "-" i))


;; Strip leading zeros and remove sign ("-" or "+") from decimal integer I.
;;
(define `(i-norm i)
  (define `u (subst "-" "" "+" "" i))
  (define `t (subst " " "" (rest (subst 0 "0 " " 0" 0 (concat "0" u)))))

  (patsubst "-0" 0 (concat (i-neg? i) (or t 0))))

;; Trim leading and trailing zeros from a decimal fraction,
;; ensuring number does not begin or end in ".".
;;
(define (frac-trim n)
  (define `trimz (subst " " 0 (nth-rest 1 (subst 0 " " n))))
  (patsubst "%." "%" (patsubst ".%" "0.%" trimz)))


;; Compare two decimal numbers; result same as ucmp.
;;
(define (i-cmp a b)
  (let ((sa (i-neg? a))
        (sb (i-neg? b))
        (ua (u-enc a))
        (ub (u-enc b)))
    (if (xor sa sb)
        ;; different signs ... but check for 0 and -0
        (and (or (u-nonzero? ua) (u-nonzero? ub))
             (if sb 1 2))
        ;; same sign: compare abs vals and invert result if both are negative
        (filter "1 2" (subst "-2" 1 "-1" 2 (concat sa (u-cmp ua ub)))))))


;; su+ : (U,U) -> SU
;;
;;  UA, UB = U-encoded addends
;;  NEG-A, NEG-B = "-" if negative, "" otherwise.
;;
;;   a  b  result
;;   +  +  (+ a b)
;;   -  -  -(+ a b)
;;   +  -  (- a b)
;;   -  +  -(- a b)
;;
(define `(su+ neg-a ua neg-b ub)
  (define `(normalize-sign x)
    (subst "- - " "" (concat (if neg-a "- ") x)))

  (normalize-sign
   (if (xor neg-a neg-b)
       (su-sub ua ub)
       (u+ ua ub))))

(define (i+ a b)
  &public
  (define `i
    (su-dec (su+ (findstring "-" a) (u-enc a) (findstring "-" b) (u-enc b))))
  (patsubst "-0" 0 i))

;; Negate a number.  Applies to floats or ints.
(define `(0- n)
  (subst "--" "" (concat "-" (subst "+" "" n))))

(define `(i- a b)
  (i+ a (0- b)))

(define (i* a b)
  (define `i (u-dec-norm (u* (u-enc a) (u-enc b))))
  (patsubst "-0" 0 (subst "--" "" (concat (i-neg? a) (i-neg? b) i))))

(define (i> a b)
  (findstring 1 (i-cmp a b)))

;; Count "tick marks" (i's) in U, where U has 3 or fewer digits.
;;
(define (ticks u)
  (define `(x10 u)
    (subst "i" "iiiiiiiiii" u))

  (subst ":" "" (concat (word 1 u)
                        (x10 (concat (word 2 u)
                                     (x10 (word 3 u)))))))

;; Divide A by B, where both are strings of zero or more 'i' characters.
;;
(define `(tick-div a b)
  (subst "i" "" "x" "i"
         (subst b "x" a)))

;; Divide A by B, where A < B*10. Returns [Q ...REM], where 0 <= Q <= 9 and
;; A = B*Q + REM.  A, B, Q, and REM are U-encoded non-negative integers.
;;
;; To get an initial guess for Q, we take BTOP, the most significant two
;; digits of B, and ATOP, the corresponding two or three digits from A, then
;; divide ATOP/(BTOP+1) using unary representations (a string of i's).  The
;; (rounded-down) integer result is equal to either Q or Q-1, as shown here:
;;
;; Let non-negative integers N, ATOP, BTOP, and rational numbers AFRAC,
;; BFRAC, be chosen such that:
;;
;;    B = (BTOP + BFRAC)*10^N;  0 <= BFRAC < 1;  10 <= BTOP <= 99
;;    A = (ATOP + AFRAC)*10^N;  0 <= AFRAC < 1
;;
;; Our estimate is chosen to always be less than or equal to A/B:
;;
;;    A/B = (ATOP + AFRAC) / (BTOP + BFRAC)
;;    Est = ATOP / (BTOP + 1)
;;    Error = A/B - Est
;;          = (ATOP+AFRAC)/(BTOP+BFRAC) - ATOP/(BTOP+1)
;;
;; The largest errors occur where BTOP is small and provides a coarser
;; approximation of B.  Also, the error increases with larger values of ATOP
;; and AFRAC and smaller values of BFRAC, but these variables are not all
;; independent, since A < B*10.  A maximum appears to be at BTOP=10 and
;; ATOP=99.  Choosing worst-cases values for AFRAC and BFRAC:
;;
;;    Error < 100/10 - 99/11 = 1
;;
;; Further increases in ATOP affect only the subtractive term, and thereby
;; decress the error.  (The first term, A/B, must be less than 10.)
;; Increases in BTOP also decrease the error.
;;
;;          A/B  -       Est  <= 1
;;    floor(A/B) - floor(Est) <= 1
;;             Q - G          <= 1
;;
(define (u-div1 a b)
  (define `(top u)
    (ticks (nth-rest (words b) (concat ": " u))))

  (foreach
   guess (concat ":" (tick-div (top a) (concat (top b) "i")))
   (let ((rem (su-sub a (u* guess b)))
         (b b))
     (if (u< rem b)
         (concat guess " " (u-norm rem))
         (concat guess "i" " " (u-norm (su-sub rem b)))))))


;; u-div-rem: Divide A by B, returning [Q RWHOLE RFRAC].
;;
;; A is split into whole and fractional portions, AWHOLE and AFRAC.  AWHOLE
;; must be less than B*10.  AFRAC is in big-endian order.  The numerical
;; value of A is given by:
;;
;;    A = AWHOLE + (reverse(AFRAC) / 10^words(AFRAC))
;;
;; Q is an integer with PREC digits that represents the quotient in
;; fixed-point form: the most significant digit is in the one's place.
;; RWHOLE and RFRAC are the integral and fractional portions of the
;; remainder.
;;
;;    Quotient = Q / 10^(PREC-1)                    0 <= Quotient < 10
;;    Remainder = RWHOLE + RFRAC/10^words(RFRAC)    0 <= Remainder < B
;;    A = (Q*B + Remainder) / 10^(PREC-1)           0 <= A < B*10
;;
;; All values are U-encoded non-negative integers, except for AFRAC
;; (big-endian U) and PREC (a decimal integer >= 1).
;;
(define (u-div-loop div1-out frac1 frac b prec q-prev)
  (define `qdigit (word 1 div1-out))
  (define `rem (rest div1-out))
  (define `q (append qdigit q-prev))
  (define `whole (concat (or frac1 ":") " " rem))

  (if (word prec (concat ": " q-prev))
      [ q rem (append (reverse frac) frac1) ]
      (u-div-loop (u-div1 whole b) (word 1 frac) (rest frac) b prec q)))

(define (u-div-rem awhole afrac b prec)
  (u-div-loop (u-div1 awhole b) (word 1 afrac) (rest afrac) b prec nil))


(define (u-mod a b)
  (or
   (if (u-zero? a)
       ":")

   ;; shift-index==1 => no shift;  2 => shift by one
   (let ((shift-index
          (words (or (rest (nth-rest (words b) (patsubst "%" ":" a))) 1)))
         (a a)
         (b b))
     (define `o (u-div-rem (nth-rest shift-index a)
                           (reverse (wordlist 2 shift-index (concat ": " a)))
                           b
                           shift-index))
     (nth 2 o))))


(define (i-mod a b)
  (let ((sa (i-neg? a))
        (ua (u-norm (u-enc a)))
        (ub (u-norm (u-enc b))))
    (if (u-zero? ub)
        "undefined"
        (concat sa (u-dec (u-mod ua ub))))))


;;----------------------------------------------------------------
;; F encoding
;;----------------------------------------------------------------

(define `(f-ctor exp sign mag)
  (concat exp " " sign " " mag))

(define `(f-exp f) (word 1 f))
(define `(f-sign f) (word 2 f))
(define `(f-mul f) (nth-rest 3 f))
(define `(f-neg f) (findstring "-" (word 2 f)))

(define `(f-nonzero? f)
  (findstring "i" f))

(define `(f-negative? f)
  (filter "-" (f-sign f)))

(define (f-negate f)
  (f-ctor (f-exp f)
          (if (f-negative? f) "+" "-")
          (f-mul f)))

;; Construct F-encoding of N.  The result is not normalized in any sense,
;; so redundant zeros may be present and "-0" and "0" will be different.
;;
(define (f-enc n)
  (let ((sgn (if (filter "-%" n) "-"))
        (exp (or (word 2 (subst "e" "e " n)) 0))
        (lhs (u-enc (subst "." " ." (word 1 (subst "e" " " "-" "" n))))))
    (f-ctor (i- exp (u-find-point lhs))
            (or sgn "+")
            (strip (subst "." "" lhs)))))

;; f-dec : Use exponential (XXXeXX) notation when the magnitude is between
;; 20 and -6 inclusive, otherwise use fixed-point.  Then ensures that 64-bit
;; integers (with a digit to spare) print without "eXX" notation, and
;; matches JavaScript behavior, for whatever that's worth.

;; EXP = exponent (integer)
;; IS-NEG = true if number is negative
;; MUL = multiplier, normalized (no zero in most-siginificant digit)
;;
(define (f-dec3 exp sign mul)
  (or
   (if (filter-out "+" sign)
       (if (filter "-" sign)
           ;; Negative
           (concat "-" (f-dec3 exp "+" mul))
           ;; undefined
           "undefined"))

   ;; Zero
   (if (not mul) 0)

   ;; Exponential
   (let ((n (i+ exp (words (rest mul)))))
     (if (or (i> n 20)
             (i> -6 n))
         (let& ((d (u-dec (patsubst "%." ". %" (concat mul ".")))))
               (concat (frac-trim d) "e" n))))

   ;; Fixed-point
   (if (findstring "-" exp)
       ;; exp is negative
       (frac-trim (u-dec (u-add-point mul (abs exp))))
       ;; exp is positive
       (concat (u-dec mul) (nzeros exp)))))


(define (f-dec f)
  (f-dec3 (f-exp f)
          (f-sign f)
          (u-norm (f-mul f))))


(define (f+ a b)
  ;; Shift B to match the exponent of A (add least-significant 0's)
  (define `shift-b
    (concat (nwords (i- (f-exp b) (f-exp a)) ": : :") " " (f-mul b)))

  (if (i> (f-exp a) (f-exp b))
      ;; swap
      (f+ b a)
      ;; shift B
      (let& ((total (su+ (f-neg a) (f-mul a) (f-neg b) shift-b)))
            (append (f-exp a)
                    (subst "+ -" "-"
                           "- - " ""
                           (concat "+ " total))))))


(define (f* a b)
  (f-ctor (i+ (f-exp a) (f-exp b))
          (if (filter (f-sign a) (f-sign b))
              "+"
              "-")
          (u* (f-mul a) (f-mul b))))


(define (f-div3 aexp asign amul bexp bsign bmul prec)
  (define `amag (words amul))
  (define `bmag (words bmul))

  ;; Aim for B <= AWHOLE < B*10
  (define `awhole
    (if (word bmag amul)
        ;; shift BMUL; take BMAG top digits of AMUL
        (nth-rest (words (nth-rest bmag amul)) amul)
        ;; shift AMUL to make it BMAG digits long
        (append (nth-rest (words (concat amul " x")) (patsubst "%" ":" bmul))
                amul)))

  ;; All remaining digits of A following AWHOLE (if any)
  (define `afrac
    (rest (nth-rest bmag (reverse amul))))

  (or
   (if (u-zero? bmul)
       ;; undefined
       "0 N")

   (if (u-zero? amul)
       ;; zero
       "0 + :")

   (if (filter 2 (u-cmp awhole bmul))
       ;; AWHOLE < B : add a "0" digit so next time through AWHOLE >= B,
       ;; so that all PREC digits of Q will be significant.
       (f-div3 aexp asign amul bexp bsign (append bmul ":") prec))

   (let ((drr (u-div-rem awhole afrac bmul prec)))
     (define `q (first drr))
     (define `rwhole (nth 2 drr))
     (define `rfrac (nth 3 drr))

     ;; Round to nearest, ties to even.
     (define `round-up?
       (filter 1 (or (u-cmp (u+ rwhole rwhole) bmul)
                     (if (u-nonzero? rfrac) 1)
                     (if (u-odd? q) 1))))

     (f-ctor (i- (i+ (i+ amag aexp) 1)
                 (i+ (i+ bmag bexp)
                     prec))
             (if (filter asign bsign)
                 "+"
                 "-")
             (if round-up?
                 (u-carry (join q "i"))
                 q)))))

;; Divide A by B, returning rounded quotient with PREC significant digits.
;;
;; PREC = number of digits of precision; simple integer >= 1.
;;
(define (f/ a b prec)
  (f-div3 (f-exp a) (f-sign a) (f-mul a) (f-exp b) (f-sign b) (f-mul b)
          (or prec 16)))

(define (f-cmp a b)
  (if (filter (f-sign a) (f-sign b))
      ;; same sign
      (let ((sum (f+ a (f-negate b))))
        (if (f-nonzero? sum)
            (if (f-negative? sum) 2 1)
            nil))
      ;; different sign
      (if (or (f-nonzero? a) (f-nonzero? b))
          (if (f-negative? a) 2 1))))

;; These forms accept and return N-encoded values.
(define `(fnum+ a b) (f-dec (f+ (f-enc a) (f-enc b))))
(define `(fnum- a b) (f-dec (f+ (f-enc a) (f-enc (0- b)))))
(define `(fnum* a b) (f-dec (f* (f-enc a) (f-enc b))))
(define `(fnum/ a b p) (f-dec (f/ (f-enc a) (f-enc b) p)))
(define `(fnum-cmp a b) (f-cmp (f-enc a) (f-enc b)))

;;----------------------------------------------------------------
;; N encoding
;;----------------------------------------------------------------

(define `(float? str)
  (findstring "." (subst "e" "." (subst "E" "e" str))))

(define (num+ a b)
  (if (float? (concat a b))
      (fnum+ a b)
      (i+ a b)))

(define (num- a b)
  (if (float? (concat a b))
      (fnum- a b)
      (i- a b)))

(define (num* a b)
  (if (float? (concat a b))
      (fnum* a b)
      (i* a b)))

(define (num-cmp a b)
  (if (float? (subst "-" "e" (concat a b)))
      (fnum-cmp a b)
      (i-cmp a b)))

(define (num^ a b)
  &public
  (let ((sa (i-neg? a))
        (sb (i-neg? b))
        (ua (u-enc a))
        (ub (u-enc b)))
    (if (or sb (float? b))
        "undefined"
        (concat (and sa
                     (u-odd? ub)
                     "-")
                (u-dec (u^ ua ub))))))


;; Note: macros avoid conflict with Make "automatic" variables (`$*`, `$+`,
;; `$^`, `$<`, `$%`), since their names are not used as Make variables.

(define `(+ a b) &public (num+ a b))
(define `(- a b) &public (num- a b))
(define `(* a b) &public (num* a b))
(define `(/ a b ?p) &public (fnum/ a b p))

(define `(^ a b) &public (num^ a b))

(define `(mod a b) &public (i-mod a b))

(define `(> a b) &public (filter 1 (num-cmp a b)))
(define `(< a b) &public (> b a))
(define `(>= a b) &public (not (< a b)))
(define `(<= a b) &public (>= b a))
(define `(= a b) &public (not (num-cmp a b)))
(define `(== a b) &public (not (num-cmp a b)))
(define `(!= a b) &public (if (num-cmp a b) 1))

(define (max a b)
  &public
  (if (< a b)
      b
      a))

(define (min a b)
  &public
  (if (> a b)
      b
      a))


;;----------------------------------------------------------------
;; Other functions
;;----------------------------------------------------------------


;; (range MIN MAX) --> list/vector of numbers from MIN through MAX inclusive.
;; MIN and MAX must be non-negative integers.
;;
;; MIN and MAX must be normalized (no extraneous trailing zeros).
;;
(define (i-range min max)
  (if (>= min max)
      (if (eq? min max)
          min)
      (if (filter "%0" min)
          (if (filter "%9" max)
              (concat
               ;; Leading zero's are a special case to work around
               (if (filter 0 min)
                   "0 1 2 3 4 5 6 7 8 9 ")
               (foreach n (i-range (or (patsubst "%0" "%" min) 1)
                                   (or (patsubst "%9" "%" max) 0))
                        (concat n "0 " n "1 " n "2 " n "3 " n "4 "
                                n "5 "n "6 " n "7 " n "8 " n "9 ")))
              (concat (i-range min (- max 1)) " " max))
          (concat min " " (i-range (1+ min) max)))))


(define (range a b)
  &public
  (strip
   (if (i-neg? a)
       ;; Negative A
       (concat
        (addprefix "-" (reverse (range (max (0- b) 1) (0- a))))
        " "
        (range 0 b))
       ;; Non-negative A
       (i-range (i-norm a) (i-norm b)))))


;;========================================================================
;; tests
;;========================================================================


;;---------------- Utilities

;; nwords
(expect "" (nwords 0 ":"))
(expect ":" (nwords 1 ":"))
(expect ": : :" (nwords 3 ":"))
(expect ": : : :" (nwords 4 ":"))

;; nzeros
(expect "" (nzeros 0))
(expect "000" (nzeros 3))

;; abs
(expect 0 (abs -0))
(expect 1e-20 (abs 1e-20))
(expect 1 (abs -1))

;;---------------- U encoding

;; u-enc
(expect 0 (u-find-point (u-enc "123")))
(expect 2 (u-find-point (u-enc "1 .23")))

;; u-ctz
(expect 0 (u-ctz (u-enc "123")))
(expect 1 (u-ctz (u-enc "120")))
(expect 3 (u-ctz (u-enc "000")))

;; u+
(define `(i-u+ a b) (u-dec-norm (u+ (u-enc a) (u-enc b))))
(expect 5 (i-u+ 2 3))
(expect 198 (i-u+ 99 99))

;; u*
(define `(i-u* a b) (u-dec-norm (u* (u-enc a) (u-enc b))))
(expect 6 (i-u* 2 3))
(expect 998001 (i-u* 999 999))

;; u-negate
(expect ": : :" (u-extend ":" 3))

;; u-negate
(expect ":iiiiiiiiii" (u-negate ":"))
(expect ":i" (u-negate ":iiiiiiiii"))
(expect ":iiiiiiiii :iiiiiiiii :" (u-negate ":i : :iiiiiiiii"))

;; u-carry
(expect ":ii :i" (u-carry ":iiiiiiiiiiii :"))

;; u/2
(expect ":i" (u/2 ":ii"))
(expect ":ii" (u/2 ":iiiii"))
(expect ":ii" (u/2 ":iiiii"))
(expect ":iiiiiii" (u/2 ":iiiii :i"))


;;---------------- SU & I encoding

;; su+
(expect "- :i" (su+ "-" ":ii" "" ":i"))
(expect ":i" (su+ "-" ":ii" "" ":iii"))

;; su-sub, su-dec
(expect 1 (su-dec (su-sub ":iiii" ":iii")))
(expect -1 (su-dec (su-sub ":iii" ":iiii")))
(expect 0 (su-dec (su-sub ":ii" ":ii")))
(expect 1 (su-dec (su-sub ":i" nil)))
(expect -1 (su-dec (su-sub nil ":i")))

;; i+
(expect 3 (i+ 2 1))
(expect 3 (i+ "+2" 1))
(expect -3 (i+ -2 -1))
(expect 99 (i+ 100 -1))
(expect -99 (i+ 1 -100))
(expect -99 (i+ -100 1))
(expect 99 (i+ -1 100))
(expect 0 (i+ -1 1))
(expect 0 (i+ -0 0))
(expect 0 (i+ 0 -0))
(expect 0 (i+ -0 -0))

;; i-
(expect 1 (i- 2 1))
(expect -2 (i- 1 3))
(expect 4 (i- 1 -3))

;; i*
(expect 6 (i* "+2" "+3"))
(expect -6 (i* -2 "+3"))
(expect -6 (i* "+2" -3))
(expect 6 (i* -2 -3))


;;----------------  F encoding

;; f-enc
(expect "2 + :ii"  (f-enc "2e2"))
(expect "0 + : : :ii"  (f-enc "200"))
(expect "-2 - :i" (f-enc "-1e-2"))
(expect "2 + :iii :ii :i" (f-enc "1.23e4"))
(expect "-1 + :iii :ii :i" (f-enc "1.23e1"))

;; trim
(expect "0.01005" (frac-trim 000.0100500))
(expect "0" (frac-trim "0."))
(expect "0" (frac-trim ".0"))

;; f-enc & f-dec
(expect "undefined" (f-dec "0 N"))
(expect "20" (f-dec "1 + :ii"))
(expect "0.02" (f-dec "-2 + :ii"))

(define (f-norm n) (f-dec (f-enc n)))
(expect 1 (f-norm 1))
(expect 10 (f-norm 100e-1))
(expect 100 (f-norm 1e2))
(expect 12.3 (f-norm 1230e-2))
(expect -0.012 (f-norm -1200e-5))
(expect 1e23 (f-norm 1000e20))
(expect 1.234e23 (f-norm 1234e20))
(expect 101 (f-norm 1.01e2))
(expect 100.1 (f-norm 1.001e2))

(expect 0.000001 (f-norm 1e-6))
(expect 1e-7 (f-norm 1e-7))
(expect 100000000000000000000 (f-norm 1e20))
(expect 1e21 (f-norm 1e21))

;; fp+
(expect 3 (fnum+ 1 2))
(expect 300 (fnum+ 1e2 2e2))
(expect 102 (fnum+ 1e2 2e0))
(expect 200.01 (fnum+ 2e2 1e-2))
(expect 100.02 (fnum+ 2e-2 1e2))
(expect 1000.0001 (fnum+ 1000 0.0001))
(expect -3 (fnum+ -1 -2))
(expect -2 (fnum+ 1 -3))

;; fp*
(expect 1.0201 (fnum* 1.01 1.01))
(expect 21.21 (fnum* 1.01 21))
(expect 12.12 (fnum* 12 1.01))

;; fp/

(expect (subst " " "" (nwords 123 "i"))
        (ticks ":iii :ii :i"))
(expect "iii" (tick-div "iiiiiiiiiiiii" "iiii"))

(expect ":i :ii" (u-div1 ":iiiii" ":iii"))
(expect ":iii :i :i" (u-div1 ":i : :i" ": :iii"))
;; 1080 / 109 = 9, remainder 99
(expect ":iiiiiiiii :iiiiiiiii :iiiiiiiii"
        (u-div1 ": :iiiiiiii : :i" ":iiiiiiiii : :i"))

(define `(n-div-rem a afrac b prec)
  (let ((o (u-div-rem (u-enc a) (reverse (u-enc afrac)) (u-enc b) prec)))
    (for u o (u-dec u))))

;; Return PREC digits
(expect [0200 nil nil] (n-div-rem 12 nil 60 4))
(expect [5000 nil nil] (n-div-rem 60 nil 12 4))
(expect [12 340 nil] (n-div-rem 1234 nil 1000 2))
;; Use AFRAC
(expect [12345 6 nil] (n-div-rem 12 3456 10 5))

;; divide by zero
(expect "undefined" (fnum/ 12 0 12))
;; digits in B < digits in A > prec
(expect 110 (fnum/ 1234 11 2))
;; digits in B < digits in A < prec (rounds down)
(expect 112.18 (fnum/ 1234 11 5))
;; digits in B > digits in A < prec
(expect 0.00218962 (fnum/ 10 4567 6))
;; top digits of A < top digits of B
(expect 0.12281 (fnum/ 112 912 5))
;; top digits of A > top digits of B
(expect 9.99 (fnum/ 999 100 5))
;; signs...
(expect 2 (fnum/ -4 -2 4))
(expect -2 (fnum/ -4 2 4))
(expect -2 (fnum/ 4 -2 4))
;; Round to even (up or down)) at 0.5
(expect 2 (fnum/ 15 10 1))
(expect [2 5 nil] (n-div-rem 25 nil 10 1))
(expect 2 (fnum/ 25 10 1))

;; f-cmp
(expect nil (fnum-cmp 0 0))
(expect 1 (fnum-cmp 2 1))
(expect 2 (fnum-cmp 1 2))
(expect 2 (fnum-cmp 0001 2))
(expect nil (fnum-cmp 00 -0))

;;----------------  N encoding (ordinary decimal)

;; +
(expect 3 (+ 1 2))
(expect 1.01 (+ 1 0.01))
(expect 101 (+ 1 1e2))

;; -
(expect 3 (- 1 -2))
(expect 3.1 (- 1 -2.1))

;; *
(expect 6.03 (* 3 2.01))
(expect 6 (* 2 3))

;; /
(expect 0.3333 (/ 1 3 4))


;; <, >
(expect 1 (< 1 2))
(expect nil (< 1 1))
(expect nil (< 2 1))
(expect 1 (< -2 1))
(expect 1 (< -2 -1))
;;TODO(expect nil (< -0 0))

(expect 1 (> 2 1))

;; <=, >=
(expect 1 (<= 1 2))
(expect 1 (<= 1 1))
(expect nil (<= 2 1))
(expect 1 (>= 2 1))

;; =
(expect 1 (= 1 01))

;; min
(expect 1 (min 1 2))
(expect 1 (min 2 1))
(expect -2 (min -2 1))
(expect -1 (min -1 2))
(expect -2 (min -1 -2))

;; max
(expect 2 (max 1 2))
(expect 2 (max 2 1))
(expect 1 (max -2 1))
(expect 2 (max -1 2))
(expect -1 (max -1 -2))

;; ^
(expect 1 (^ 1 4))
(expect 1 (^ 9 0))
(expect 9 (^ 9 1))
(expect -1 (^ -1 3))
(expect 1024 (^ 2 10))

;; mod
(expect 0 (mod 0 7))
(expect "undefined" (mod 0 0))
(expect 2 (mod 2 7))
(expect -2 (mod -2 7))
(expect 6 (mod 20 7))
(expect 3 (mod 80 7))


;;----------------  Other functions

(expect 1 (i-norm 0001))
(expect 1 (i-norm "+0001"))
(expect -10200 (i-norm -00010200))
(expect 0 (i-norm -0))
(expect 0 (i-norm 0))

;; range
(expect "3 4 5 6" (range 3 6))
(expect "-2 -1 0 1 2" (range -2 2))
(expect "-2 -1 0" (range -2 0))
(expect "-2 -1 0" (range -2 -0))
(expect "-2 -1" (range -2 -1))
(expect "0 1" (range -0 1))
(expect "0" (range -0 0))
(expect "0 1" (range 000 001))
(expect "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20" (range 0 20))
