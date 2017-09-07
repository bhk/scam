;;--------------------------------------------------------------
;; num : numeric operations
;;--------------------------------------------------------------

(require "core")

;; Exported functions:
;;
;;    +, -, *, / ^, mod, =, !=, <, >, <=, >=
;;    min, max, sum, range
;;
;; Most functions accept exactly two arguments and operate on
;; arbitrary-precision floating point: `+`, `-`, and `*` return exact
;; results.  `/` accepts a third argument that specifies the precision
;; required.  `^`, `mod`, and `range` accept integral parameters.


;; Various encodings of numbers are used internally by this module:
;;
;; U: This encoding represents non-negative integers in base 10, using one
;;    word per digit in little-endian form.  Each word consists of a ":"
;;    followed by 0-9 "i" characters:
;;
;;       ": :i :ii"  =>  210
;;       ": : :"     =>  000
;;
;;    In this encoding, `(join A B)` can be used to add corresponding
;;    digits, and `subst` can be used multiply and to propagate overflow to
;;    the next higher digit.
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
(define (nwords-loop len w)
  (if (subst "0" "" len)
      (if (word len w)
          (wordlist 1 len w)
          (nwords-loop len (concat w " " w " " w)))))

(define `(nwords len w)
  (nwords-loop len (concat w " " w " " w)))

(define `(nzeros e)
  (subst " " "" (nwords e "0")))

;; Absolute value of a number.  Applies to any decimal representation; does
;; not return a normalized form.
;;
(define `(abs n)
  &public
  (patsubst "-%" "%" (subst "+" "" n)))

;; Negate a number.  Applies to any decimal representation; does not return
;; a normalized form.
;;
(define `(0- n)
  (subst "--" "" (concat "-" (subst "+" "" n))))

;; Return the last N words in LST, where N >= 0.
;;
(define (tail n lst)
  (if (subst 0 "" n)
      (if (word n lst)
          (nth-rest (words (nth-rest n lst)) lst)
          lst)))

;;----------------------------------------------------------------
;; U encoding
;;----------------------------------------------------------------

;; Convert each ":iii..." word to a digit.  Do not reverse or remove spaces.
;;
(define `(unary-to-digits u)
  (subst ":" "0" "0i" "1" "1i" "2" "2i" "3" "3i" "4" "4i" "5"
         "5i" "6" "6i" "7" "7i" "8" "8i" "9" u))

;; Convert a string of digits to a list of ":iii..." words.
;;
(define `(digits-to-unary n)
  (subst "9" "8i" "8" "7i" "7" "6i" "6" "5i" "5" "4i" "4" "3i"
         "3" "2i" "2" "1i" "1" "0i" "0" " :" n))

;; Return NIL if U is zero.
(define `(u-nonzero? u)
  (findstring "i" u))

(define `(u-zero? u)
  (not (findstring "i" u)))

;; Return "i" if U is odd, `nil` otherwise.
;;
(define `(u-odd? u)
  (findstring "i" (subst "ii" "" (word 1 u))))

;; More efficient reverse for lists of digits (at least when small).
;;
(define (numreverse n)
  (if (word 5 n)
      (reverse n)
      (strip (subst "~" "" (sort (join "~~~ ~~ ~" n))))))

;; Convert from N to U encoding.  Strip any sign symbol.
;;
(define (u-enc n)
  (if (filter "0 1 2 3 4 5 6 7 8 9" (subst " " "x" n))
      ;; one word => no reverse, fewer ops
      (concat ":" (subst " " "" (wordlist 1 n "i i i i i i i i i")))
      (numreverse (digits-to-unary (subst "+" "" "-" "" n)))))

;; Convert from U to N encoding.
;;
(define (u-dec u)
  (subst " " "" (unary-to-digits (numreverse u))))

;; Convert a U-encoded value to a non-negative decimal integer and
;; normalize: no extraneous significant 0's, "0" instead of empty string.
;;
(define (u-dec-norm u)
  (define `(ztrim digits)
    (subst " " "" (rest (subst " 0" 0 (concat "0 " digits)))))
  (or (ztrim (numreverse (unary-to-digits u)))
      0))

;; Remove extraneous most-significant zeros from U-encoded numbers.
;; trim MSB zeroes
(define (u-norm u)
  (strip (subst ":" " :" (filter "%i" (subst " " "" "i:" "i :" u)))))

;; Return decimal fixed-point representation of U/10^N.  Extraneous
;; zeros (at the MS and LS ends) will be trimmed.
;;
(define (u-dec/10^n u n)
  ;; zpz: U-encoded 0.0000 (with N trailing 0's)
  (define `zpz (concat (nwords n ":") " .:"))
  ;; Normalize: no trailing "0", no inital "00", no leading/trailing ".".
  (define `(norm n)
    (patsubst "%." "%"
              (patsubst ".%" "0.%"
                        (subst " " 0 (nth-rest 1 (subst 0 " " n))))))

  (norm (u-dec (subst "::" ":" "." ". " (join zpz u)))))

(define (u-carry-loop x)
  (if (findstring "iiiiiiiiii :" x)
      (u-carry-loop (subst "iiiiiiiiii :" " :i" x))
      x))

;; Propagate value from digits greater than 10 to the next higher digit.
;;
(define `(u-carry x)
  (u-carry-loop (subst "iiiiiiiiii :" " :i" x)))

;; Combine respective digits without performing carry or extending digits.
;;
(define `(raw-add a b)
  (subst "i:" "i" "::" ":" (join a b)))

;; Add two U-encoded values.  Add a most-significant zero to accommodate
;; carry.
(define (u+ a b)
  &inline
  (u-carry (concat (raw-add a b) " :")))

(define `(mul-carry u)
  (u-carry-loop
   (subst "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii :" " :iiii"
          "iiiiiiiiiiiiiiiiiiii :" " :ii"
          "iiiiiiiiii :" " :i"
          "iiiiiiiiii :" " :i"
          (concat u " :"))))

(define (u* a b)
  (mul-carry
   (raw-add
    (subst "i" (subst ":" "" (firstword a)) b)
    (if (word 2 a)
        (u* (rest a) (concat ": " b))))))

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
(define (u-extend-ms u digits)
  (subst "::" ":" (join (nwords digits ":") u)))

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

;; Return A^B.
;;
(define (u^ a b)
  (if (u-odd? b)
      (u-norm (u* a (u^2 (u^ a (u/2 b)))))
      (if (findstring "i" b)
          (u-norm (u^2 (u^ a (u/2 b))))
          ":i")))

;;----------------------------------------------------------------
;; SU & I encoding
;;----------------------------------------------------------------

;; Return "-" if I is preceded with a minus sign.  This means that
;; I <= 0.
;;
(define `(i-minus i)
  (findstring "-" i))

(define `(i-nonzero? i)
  (subst "-" "" "0" "" i))

(define `(i-neg? i)
  (and (i-minus i) (i-nonzero? i) "-"))

(define `(i-odd? i)
  (filter "%1 %3 %5 %7 %9" i))

;; SU:  a U-encoded number optionally preceded by a "-"
(define (su-enc n)
  (append (i-minus n)
          (u-enc n)))

(define (su-dec u)
  (concat (if (u-nonzero? u)
              (filter "-" (word 1 u)))
          (u-dec-norm (filter-out "-" u))))

;; su-sub: (U,U) --> SU
;; Subtract UB from UA, returning an SU-encoded integer.
;;
(define (su-sub ua ub)
  (let ((total (u+ ua (u-negate (u-extend-ms ub (words ua)))))
        (len (words (join ua ub))))
    (if (u-nonzero? (word len (rest total)))
        (wordlist 1 len total)
        (concat "- " (wordlist 1 len (u-negate total))))))

;; Strip leading zeros and remove extraneous sign ("-" or "+") from decimal
;; integer I.
;;
(define `(i-norm i)
  (define `u (subst "-" "" "+" "" i))
  (define `t (subst " " "" (rest (subst 0 "0 " " 0" 0 (concat "0" u)))))
  (or (addprefix (i-minus i) t) 0))

;; Compare two decimal numbers; result same as ucmp.
;;
(define (i-cmp a b)
  (let ((sa (i-minus a))
        (sb (i-minus b))
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
  (su-dec (su+ (findstring "-" a) (u-enc a) (findstring "-" b) (u-enc b))))

(define `(i- a b)
  (i+ a (0- b)))

(define (i* a b)
  (define `i (u-dec-norm (u* (u-enc a) (u-enc b))))
  (patsubst "-0" 0 (subst "--" "" (concat (i-minus a) (i-minus b) i))))

(define `(i> a b)
  (filter 1 (i-cmp a b)))

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


;; u-divx: Divide A by B, returning [Q RWHOLE RFRAC].
;;
;; A must be less than 10*B.  A is split into whole and fractional portions,
;; AWHOLE and AFRAC.  The numerical value of A is given by:
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
(define (u-divx-loop div1-out frac1 frac b prec q-prev)
  (define `qdigit (word 1 div1-out))
  (define `rem (rest div1-out))
  (define `q (append qdigit q-prev))
  (define `whole (concat (or frac1 ":") " " rem))

  (if (word prec (concat ": " q-prev))
      [ q rem (append (numreverse frac) frac1) ]
      (u-divx-loop (u-div1 whole b) (word 1 frac) (rest frac) b prec q)))

(define (u-divx awhole afrac b prec)
  (u-divx-loop (u-div1 awhole b) (word 1 afrac) (rest afrac) b prec nil))


;; Divide A by B, returning [QUOTIENT REMAINDER].
;;
(define (u-div a b)
  (or
   (if (u-zero? a)
       ":")

   ;; shift-index==1 => no shift;  2 => shift by one
   (let ((shift-index
          (words (or (rest (nth-rest (words b) (patsubst "%" ":" a))) 1)))
         (a a)
         (b b))
     (u-divx (nth-rest shift-index a)
             (numreverse (wordlist 2 shift-index (concat ": " a)))
             b
             shift-index))))


(define (u-mod-loop a bi 10-mod-b)
  (define `a/10-mod-b
    (u-mod-loop (rest a) bi 10-mod-b))

  (if a
      (subst bi "" (concat (subst ":" "" (word 1 a))
                           (if 10-mod-b
                               (subst "i" 10-mod-b a/10-mod-b))))))

;; A mod B in ticks, where BI = ticks in B.  For B in {1, 2, 4, 5, 8} not
;; all digits will need to be examined.
(define (u-mod-loop a bi digit-mod-b residue)
  (if (and a digit-mod-b)
      (u-mod-loop (rest a)
                  bi
                  (subst "i" "iiiiiiiiii" bi "" digit-mod-b)
                  (subst bi "" (concat (subst ":" "" "i" digit-mod-b (word 1 a))
                                       residue)))
      residue))

;; Return A modulo B, where BI = B in ticks (single digit, no ":").
(define (u-mod a bi)
  (define `ticks
    (if (filter "iii iiiiiiiii" bi)
        (subst ":" "" " " "" bi "" a)
        (u-mod-loop a bi "i" nil)))
  (words (subst "i" "i " ticks)))

(define (i-mod a b)
  (let ((sa (i-neg? a))
        (ua (u-norm (u-enc a)))
        (ub (u-norm (u-enc b))))
    (if (u-zero? ub)
        "undefined"
        (concat sa (if (word 2 ub)
                       (u-dec-norm (nth 2 (u-div ua ub)))
                       (u-mod ua (subst ":" "" ub)))))))

(expect 1 (i-mod 1 7))
(expect 0 (i-mod 0 7))

;;----------------------------------------------------------------
;; F encoding
;;----------------------------------------------------------------

(define `(f-ctor exp sign mag)
  (concat exp " " sign " " mag))

(define `(f-exp f) (word 1 f))
(define `(f-sign f) (word 2 f))
(define `(f-mul f) (nth-rest 3 f))

(define `(f-nonzero? f)
  (findstring "i" f))

(define `(f-minus f)
  (filter "-" (f-sign f)))

(define (f-negate f)
  (f-ctor (f-exp f)
          (if (f-minus f) "+" "-")
          (f-mul f)))

(define `(float? n)
  (findstring "." (subst "e" "." (subst "E" "e" n))))

;; Construct F-encoding of N.  The result is not normalized in any sense,
;; so redundant zeros may be present and "-0" and "0" will be different.
;;
(define (f-enc n)
  (if (float? n)
      (let ((w (subst "E" " " "e" " " "+" "" n)))
        (define `m (word 1 w))
        (define `e (word 2 w))
        (define `frac (word 3 (subst "." " . " (concat 0 m))))

        (f-ctor (i- e (words (u-enc frac)))
                (or (i-minus m) "+")
                (u-enc (subst "." "" m))))
      (f-ctor 0
              (or (i-minus n) "+")
              (u-enc n))))

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
           (patsubst "-0" 0
                    (concat "-" (f-dec3 exp "+" mul)))
           ;; undefined
           "undefined"))

   ;; Zero
   (if (not mul) 0)

   ;; Scientific
   (let ((n (i+ exp (words (rest mul)))))
     (if (or (i> n 20)
             (i> -6 n))
         (let& ((frac (u-dec/10^n mul (words (rest mul)))))
               (concat frac "e" (subst "+-" "-" (concat "+" n))))))

   ;; Fixed-point
   (if (findstring "-" exp)
       ;; exp is negative
       (u-dec/10^n mul (abs exp))
       ;; exp is positive
       (concat (u-dec mul) (nzeros exp)))))


(define (f-dec f)
  (f-dec3 (f-exp f)
          (f-sign f)
          (u-norm (f-mul f))))


(define (f+ a b)
  ;; Shift B to match the exponent of A (add least-significant 0's)
  (define `shift-b
    (concat (nwords (i- (f-exp b) (f-exp a)) ":") " " (f-mul b)))

  (if (i> (f-exp a) (f-exp b))
      ;; swap
      (f+ b a)
      ;; shift B
      (let& ((total (su+ (f-minus a) (f-mul a) (f-minus b) shift-b)))
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
    (rest (nth-rest bmag (numreverse amul))))

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

   (let ((drr (u-divx awhole afrac bmul prec)))
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
                 (u-carry-loop (join q "i"))
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
            (if (f-minus sum) 2 1)
            nil))
      ;; different sign
      (if (or (f-nonzero? a) (f-nonzero? b))
          (if (f-minus a) 2 1))))

;; These forms accept and return N-encoded values.
(define `(fnum+ a b) (f-dec (f+ (f-enc a) (f-enc b))))
(define `(fnum- a b) (f-dec (f+ (f-enc a) (f-enc (0- b)))))
(define `(fnum* a b) (f-dec (f* (f-enc a) (f-enc b))))
(define `(fnum/ a b p) (f-dec (f/ (f-enc a) (f-enc b) p)))
(define `(fnum-cmp a b) (f-cmp (f-enc a) (f-enc b)))

;;----------------------------------------------------------------
;; N encoding
;;----------------------------------------------------------------

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
  (if (float? (concat a b))
      (fnum-cmp a b)
      (i-cmp a b)))

(define (num^ a b)
  &public
  (if (or (float? b) (i-neg? b))
      "undefined"
      (let ((fa (f-enc a)))
        (f-dec3
         (i* (f-exp fa) b)
         (if (and (f-minus fa) (i-odd? b)) "-" "+")
         (u^ (f-mul fa) (u-enc b))))))


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
(define `(>= a b) &public (filter 1 (or (num-cmp a b) 1)))
(define `(<= a b) &public (>= b a))
(define `(= a b) &public (not (num-cmp a b)))
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

(define (fsum-loop a nums)
  (if nums
      (fsum-loop (f+ a (f-enc (word 1 nums))) (rest nums))
      a))

(define (susum-loop a v1 vec)
  (if v1
      (susum-loop (su+ (findstring "-" (word 1 a))
                       (filter-out "-" a)
                       (i-minus v1)
                       (u-enc v1))
                  (word 1 vec)
                  (rest vec))
      a))

;; Sum a vector of numbers.
;;
(define (sum vec)
  &public
  (if (word 1 vec)
      (if (float? vec)
          (f-dec (fsum-loop (f-enc (word 1 vec)) (rest vec)))
          (su-dec (susum-loop (su-enc (word 1 vec))
                              (word 2 vec)
                              (nth-rest 3 vec))))
      0))

;; (range MIN MAX) --> list/vector of numbers from MIN through MAX inclusive.
;; MIN and MAX must be non-negative integers.
;;
;; MIN and MAX must be normalized (no extraneous trailing zeros).
;;
(define (i-range min max)
  (if (i> min max)
      nil
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
              (concat (i-range min (i- max 1)) " " max))
          (concat min " " (i-range (1+ min) max)))))


(define (range a b)
  &public
  (strip
   (if (i-minus a)
       ;; Negative A
       (concat
        (addprefix "-" (reverse (range (max (0- b) 1) (0- a))))
        " "
        (range 0 b))
       ;; Non-negative A
       (i-range (i-norm a) (i-norm b)))))


;; f-enc-norm: `-` only when non-zero, no leading zeros
;;
(define (f-enc-norm x)
  (let ((f (f-enc x)))
    (f-ctor (i-norm (f-exp f))
            (if (f-nonzero? f) (f-sign f) "+")
            (u-norm (f-mul f)))))


(define (pad-and-sign digits pad minus)
  (define `(pad-pat char)
    (if (filter 0 pad)
        (concat char "%")
        (concat "%" char)))

  ;; replace one of the pad characters with "-"
  (define `minus-digits
    (subst " " ""
           (patsubst (pad-pat "_") (pad-pat "-")
                     (subst "_" "_ " " _" "_" digits))))

  (subst "_" pad (if minus minus-digits digits)))


;; NWHOLE: The maximum number of characters to be used to display the whole
;;    (non-fractional) portion, including `-` if the value is negative.
;;
;; PAD: when this is `0` or ` `, the whole portion will be padded with
;;    this character to consume NWHOLE characters.  When nil, the number
;;    will not be extended.
;;
;; NFRAC: If non-nil, a decimal point and this many fractional digits
;;    will be included.  If nil, no decimal point will be included. The
;;    number will be rounded to the nearest value that can be represented.
;;
;; If NFRAC is too small to include all fractional digits, the number will
;; be rounded to the nearest value that can be displayed.  If NWHOLE is too
;; small to include all significant digits, all digits will be replaced with
;; `?` characters.
;;
;; When the pad character is `0`, padding is to the righ of the `-` sign,
;; and when it is ` ` the padding appears to the left of the sign.
;;
(define (num-format x nwhole ?pad ?nfrac)
  &public
  (let ((f (f-enc-norm x)))
    (define `mul (f-mul f))
    (define `exp (f-exp f))
    (define `minus (filter "-" (f-sign f)))

    (define `whole-len
      (words (filter-out "+" (nth-rest 2 f))))

    (define `whole-val
      (if (i-minus exp)
          (nth-rest (1+ (0- exp)) mul)
          (append (nwords exp ":") mul)))

    (define `frac-val
      (if (i-minus exp)
          (wordlist 1 (0- exp) mul)
          nil))

    (define `frac
      (tail nfrac (append (nzeros nfrac) frac-val)))

    ;; formatted string with "_" where padding should/would go
    (define `digits
      (append (if nfrac (append frac "."))
              (subst "_:" ":"
                     (join (nwords nwhole "_")
                           whole-val))))

    ;; Handle large numbers without excessive memory usage.
    (if (i> whole-len nwhole)
        ;; too big
        (subst "0" "?" (concat (nzeros nwhole)
                               (if nfrac (concat "." (nzeros nfrac)))))
        ;; not too big
        (pad-and-sign (u-dec digits) pad minus))))
