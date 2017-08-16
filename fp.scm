;; TODO:
;;  - sub, divide
;;  - Accept (emit?) "e+XX"
;;  - Projectively extended real number line:  q/0 == Infinity
;;  - benchmark add/subtract
;;     - faster carry? multi-digit carry?
;;  - switch to ":" (or 0) from "."
;;  - round
;;  - fr-exp (return m and 2 s.t. x = m2ᵉ)
;;  - num-pi(ndigits)
;;
;;  +  ->  i+  -> u+
;;         fp+ -> f+, i+, u+
;;

(require "core")

;; Various encodings of numbers are used by this module:
;;
;; U: This encoding represents non-negative integers in base 10, using one
;;    word per digit in little-endian form.  Each word consists of a ":"
;;    followed by 0-9 "i" characters:
;;
;;       ":i : :ii"  =>  201
;;       ": : :"     =>  000
;;
;; F: This encoding represents a floating point number as three values: "EXP
;;    SIGN MAN". EXP is the exponent as a decimal integer.  SIGN is "+" or
;;    "-". MAN is the absolute value of the mantissa, U-encoded.  The
;;    numeric value is given by: MAN * 10^EXP * (SIGN=="-" ? -1 : 1)
;;
;;       "0 - :ii : :iii"     ==>  -302
;;       "-10 + :iii :ii :i"  ==>  1.23e-8
;;
;; N: Number: The format accepted and returned by public functions.
;;
;;       "1",  "-0.032e32"
;;
;; I: Decimal integer: A subset of numbers (those that avoid "." and "e").
;;
;;       I  <-  SIGN? DIGIT+
;;       N  <-  I ( "." DIGIT* )? ( E I )?
;;           |  "NaN"
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

(expect "" (nwords 0 ":"))
(expect ":" (nwords 1 ":"))
(expect ": : :" (nwords 3 ":"))
(expect ": : : :" (nwords 4 ":"))


(define `(nzeros e)
  (subst " " "" (nwords e "0 0 0")))

(expect "" (nzeros 0))
(expect "000" (nzeros 3))

(define (abs n)
  (patsubst "-%" "%" (subst "+" "" n)))

;;----------------------------------------------------------------
;; U encoding
;;----------------------------------------------------------------

;; U-encode N, a non-negative decimal integer.
(define (u-enc n)
  (reverse
   (subst "9" "8i" "8" "7i" "7" "6i" "6" "5i" "5" "4i" "4" "3i"
          "3" "2i" "2" "1i" "1" "0i" "0" " :" n)))

(define (u-dec u)
  (subst " " ""
         (reverse
          (or (subst ":" "0" "0i" "1" "1i" "2" "2i" "3" "3i" "4" "4i" "5"
                     "5i" "6" "6i" "7" "7i" "8" "8i" "9" u)))))


;; Remove extraneous most-significant zeros from U-encoded numbers.
;; trim MSB zeroes
(define (u-norm u)
  (strip (subst ":" " :" (butlast (subst " " ""  "i:" "i :"
                                         (concat u ":"))))))

;; Convert a U-encoded value to a non-negative decimal integer,
;; normalized (without extraneous significant 0's).
;;
(define (u-dec-norm u)
  (or (subst ":" "0" "0i" "1" "1i" "2" "2i" "3" "3i" "4" "4i" "5"
             "5i" "6" "6i" "7" "7i" "8" "8i" "9" " " "" "*" ":"
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

(expect 0 (u-find-point (u-enc "123")))
(expect 2 (u-find-point (u-enc "1 .23")))


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

(expect 0 (u-ctz (u-enc "123")))
(expect 1 (u-ctz (u-enc "120")))
(expect 3 (u-ctz (u-enc "000")))


;; For digits greater than 9, propagate values to the next higher digit,
;; *IF* there is a "next" digit.
(define (u-carry n)
  (while (lambda (x) (findstring "iiiiiiiiii :" x))
         (lambda (x) (subst "iiiiiiiiii :" " :i" x))
         n))


;; Add two U-encoded values.  Add a most-significant zero to accommodate
;; carry.
(define (u+ a b)
  (u-carry (concat (subst "i:" "i" "::" ":" (join a b)) " :")))

   (define `(i-u+ a b) (u-dec-norm (u+ (u-enc a) (u-enc b))))
   (expect 5 (i-u+ 2 3))
   (expect 198 (i-u+ 99 99))

(define (u* a b)
  (u+ (subst "i" (subst ":" "" (firstword a)) b)  ;; "big" carry (4,2,1)
      (if (word 2 a)
          (u* (rest a) (concat ": " b)))))

   (define `(i-u* a b) (u-dec-norm (u* (u-enc a) (u-enc b))))
   (expect 6 (i-u* 2 3))
   (expect 998001 (i-u* 999 999))


;;(define (u+1 a)
;;  (u-carry (join (or a ":") "i")))


;; Compare two unsigned encoded numbers, returning:
;;   1 if a > b
;;   2 if b > a
;;   nil otherwise
(define (u-cmp a b)
  (if (or a b)
      (or (u-cmp (rest a) (rest b))
          (if (findstring (concat (word 1 b) "i") (word 1 a)) 1)
          (if (findstring (concat (word 1 a) "i") (word 1 b)) 2))))

(define (u-extend u digits)
  (subst "::" ":" (join (nwords digits ": : :") u)))

  ;; test
  (expect ": : :" (u-extend ":" 3))

(define (u-negate u)
  (join
   (foreach w u (concat ":" (subst w "" ":iiiiiiiii")))
   "i"))

  ;; test
  (expect ":iiiiiiiiii" (u-negate ":"))
  (expect ":i" (u-negate ":iiiiiiiii"))
  (expect ":iiiiiiiii :iiiiiiiii :" (u-negate ":i : :iiiiiiiii"))


;;----------------------------------------------------------------
;; I encoding
;;----------------------------------------------------------------

(define `(i-neg? i)
  (if (filter "-%" i) "-"))

;; Compare two decimal numbers; result same as ucmp.
;;
(define (i-cmp a b)
  (let ((sa (i-neg? a))
        (sb (i-neg? b))
        (ua (u-enc a))
        (ub (u-enc b)))
    (if (xor sa sb)
        ;; different signs ... but check for 0 and -0
        (and (or (findstring "i" ua) (findstring "i" ub))
             (if sb 1 2))
        ;; same sign: compare abs vals and invert result if both are negative
        (filter "1 2" (subst "-2" 1 "-1" 2 (concat sa (u-cmp ua ub)))))))


;; Subtract UB from UA, returning Integer (decimal).  Assumes UA >= UB.
(define (u-sub ua ub sgn)
  (define `nub (u-negate (u-extend ub (words ua))))
  (define `u
    (if (not ub)
        ua
        (wordlist 1 (words ua) (u+ ua nub))))

  (subst "-0" "0"
         (concat (if sgn "-") (u-dec-norm u))))

  ;; test
  (expect "-1" (u-sub (u-enc 3) (u-enc 2) "-2"))
  (expect "99" (u-sub (u-enc 100) (u-enc 1) nil))


(define (i+ a b)
  (let ((sa (i-neg? a))
        (sb (i-neg? b))
        (ua (u-enc (abs a)))
        (ub (u-enc (abs b))))
    (if (xor sa sb)
        (if (filter 1 (u-cmp ua ub))
            (u-sub ua ub sa)
            (u-sub ub ua sb))
        (concat (if sa "-") (u-dec-norm (u+ ua ub))))))

  ;; test
  (expect 3 (i+ 2 1))
  (expect -3 (i+ -2 -1))
  (expect 99 (i+ 100 -1))


(define `(i- a b)
  (i+ a (subst "--" "" (concat "-" b))))

  ;; test
  (expect 1 (i- 2 1))
  (expect -2 (i- 1 3))
  (expect 4 (i- 1 -3))


(define (i> a b)
  (findstring 1 (i-cmp a b)))


;;----------------------------------------------------------------
;; F encoding
;;----------------------------------------------------------------

(define `(f-exp f) (word 1 f))
(define `(f-sign f) (word 2 f))
(define `(f-man f) (nth-rest 3 f))


(define (f-enc n)
  (let ((sgn (if (filter "-%" n) "-"))
        (exp (or (word 2 (subst "e" "e " n)) 0))
        (lhs (u-enc (subst "." " ." (word 1 (subst "e" " " "-" "" n))))))
    (append (i- exp (u-find-point lhs))
            (or sgn "+")
            (strip (subst "." "" lhs)))))


(expect "2 + :ii"  (f-enc "2e2"))
(expect "0 + : : :ii"  (f-enc "200"))
(expect "-2 - :i" (f-enc "-1e-2"))
(expect "2 + :iii :ii :i" (f-enc "1.23e4"))
(expect "-1 + :iii :ii :i" (f-enc "1.23e1"))


;; Trim leading and trailing zeros from a decimal fraction,
;; ensuring number does not begin or end in ".".
(define (trim n)
  (define `trimz (subst " " 0 (nth-rest 1 (subst 0 " " n))))
  (patsubst "%." "%" (patsubst ".%" "0.%" trimz)))

(expect "0.01005" (trim 000.0100500))
(expect "0" (trim "0."))
(expect "0" (trim ".0"))


;;--------------------------------
;; f-dec
;;
;; Use exponential (XXXeXX) notation when the magnitude is between 20 and -6
;; inclusive, otherwise use fixed-point.  Then ensures that 64-bit integers
;; (with a digit to spare) print without "eXX" notation, and matches JavaScript
;; behavior, for whatever that's worth.

;; EXP = exponent (integer)
;; IS-NEG = true if number is negative
;; MAN = mantissa, normalized (no zero in most-siginificant digit)
;;
(define (f-dec3 exp is-neg man)
  (or
   ;; Negative
   (if is-neg
       (concat "-" (f-dec3 exp nil man)))

   ;; Zero
   (if (not man) 0)

   ;; Exponential
   (let ((n (i+ exp (words (rest man)))))
     (if (or (i> n 20)
             (i> -6 n))
         (let& ((d (u-dec (patsubst "%." ". %" (concat man ".")))))
               (concat (trim d) "e" n))))

   ;; Fixed-point
   (if (findstring "-" exp)
       ;; exp is negative
       (trim (u-dec (u-add-point man (abs exp))))
       ;; exp is positive
       (concat (u-dec man) (nzeros exp)))))


(define (f-dec f)
  (f-dec3 (f-exp f)
          (findstring "-" (f-sign f))
          (u-norm (f-man f))))


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


;;--------------------------------
;; f+
;;--------------------------------

(define (f+ a b)
  (define `shift-b
    (concat (nwords (i- (f-exp b) (f-exp a)) ": : :") " " (f-man b)))

  (if (i> (f-exp a) (f-exp b))
      ;; swap
      (f+ b a)
      ;; shift B
      (append (f-exp a) "+" (u+ (f-man a) shift-b))))

(define (fp+ a b) (f-dec (f+ (f-enc a) (f-enc b))))


(expect 3 (fp+ 1 2))
(expect 300 (fp+ 1e2 2e2))
(expect 102 (fp+ 1e2 2e0))
(expect 200.01 (fp+ 2e2 1e-2))
(expect 100.02 (fp+ 2e-2 1e2))
(expect 1000.0001 (fp+ 1000 0.0001))

;;TODO
;;(expect -3 (fp+ -1 -2))
;;(expect -2 (fp+ 1 -3))

(define (f* fa fb)
  (append (i+ (f-exp fa) (f-exp fb))
          (if (findstring (f-sign fa) (f-sign fb))
              "+"
              "-")
          (u* (f-man fa) (f-man fb))))

(define (fp* a b) (f-dec (f* (f-enc a) (f-enc b))))


(expect 1.0201 (fp* 1.01 1.01))
(expect 21.21 (fp* 1.01 21))
(expect 12.12 (fp* 12 1.01))


;; (define (integer? a b)
;;   (not (findstring "e" (subst "." "e" (concat a b)))))
;;
;; (define (x+ a b)
;;   (if (integer? a b)
;;       (i+ a b)
;;       (fp+ a b)))


;;----------------------------------------------------------------
;; https://link.springer.com/article/10.1023/A%3A1015569431383
;;
;; Rump’s example is to compute the expression:
;;
;;   ƒ = 333.75b⁶ + a²(11a²b² − b⁶ − 121b⁴ − 2) + 5.5b⁸ + a/(2b)
;;
;; with a = 77617 and b = 33096. On an IBM S/370 main frame he computed ƒ
;; using single, double, and extended-precision arithmetic, to produce the
;; results:
;;
;;    Single precision: ƒ = 1.172603...
;;    Double precision: ƒ = 1.1726039400531...
;;    Extended precision: ƒ = 1.172603940053178...
;;
;; This suggests a reliable result of approximately 1.172603 or even
;; 1.1726039400532. In fact, however, the correct result (within one unit of
;; the last digit) is
;;
;;   ƒ = −0.827396059946821368141165095479816...
;;
;; Even the sign was wrong.
;;
;; ... For example, the following rearrangement of Rump’s expression
;; produces his original effect on IEEE-754 computers:
;;
;;    ƒ = (333.75 − a²)b⁶ + a²(11a²b² − 121b⁴ − 2) + 5.5b⁸ + a/2b
;;
;; Using 32-bit, 64-bit, and 128-bit round-to-nearest IEEE-754 arithmetic,
;; this produces:
;;
;;    32-bit:  ƒ = 1.172604,
;;    64-bit:  ƒ = 1.1726039400531786,
;;    128-bit: ƒ = 1.1726039400531786318588349045201838.
;;
;; Even this reformulated expression is not completely stable. Wild
;; variations in results may occur for alternative rounding modes, rounding
;; precisions, arithmetic formats (such as 96-bit words), or exponential
;; evaluation methods.
