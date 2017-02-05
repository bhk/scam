;;----------------------------------------------------------------
;; num: Numeric operators for arbitrarily large integers.
;;----------------------------------------------------------------

(require "core")

;; Most internal functions operate on an encoded form of numbers that uses
;; one word for each digit, in LSB form.  For non-negative (unsigned)
;; numbers, each word consists of a "." followed by 0-9 "i" characters.
;;
;;      .i . .ii => 201
;;      . . .    => 000
;;
;; Negative numbers are expressed in 10's complement form: the MSB "digit"
;; is ".-" representing -1 in that place (-1, -10, -100, etc.).  All digits
;; of lower significance constitute a non-negative number to be added the
;; to the negative value.
;;
;;     . . .-     =>  -100
;;     .i .ii .-  =>  -79


;; return "-" if number includes with "-", "" otherwise
(define (sign n)
  (findstring "-" n))


;; trim MSB zeroes
(define (nnorm n)
  (strip (subst "." " ." (butlast (subst " " ""  "i." "i ."  "-." "- ."
                                         (concat n "."))))))


;; encode the absolute value of a number, yielding an unsigned encoded number.
(define (uencode s)
  (reverse
   (subst "9" "8i" "8" "7i" "7" "6i" "6" "5i" "5" "4i" "4" "3i"
          "3" "2i" "2" "1i" "1" "0i" "-" "" "0" " ." s)))


;; SIGN = "-" or ""
(define (udecode n sign)
  (let& ((nr (subst " " "" (reverse n)))
         (nchop (subst " " "" (rest (subst ".i" " .i" (concat "." nr))))))
        (concat
         (and (findstring "i" n) sign)
         (or (subst "." "0" "0i" "1" "1i" "2" "2i" "3" "3i" "4" "4i" "5"
                    "5i" "6" "6i" "7" "7i" "8" "8i" "9" nchop)
             "0"))))


;; "Carry" values greater than 9 to the next significant place.
;; It assumes an individual digit is not larger than 99.
(define (ucarry n)
  (while (lambda (x) (findstring "iiiiiiiiii ." x))
         (lambda (x) (subst "iiiiiiiiii ." " .i" x))
         (concat n " .")))


(define (u+ a b)
  (ucarry (subst "i." "i" ".." "." (join a b))))


(define (u+1 a)
  (ucarry (join (or a ".") "i")))


(define (u* a b)
  (u+ (subst "i" (subst "." "" (firstword a)) b)
      (if (word 2 a)
          (u* (rest a) (concat ". " b)))))



;; decode a (possibly negative) number.
(define (ndecode n)
  (udecode (if (findstring "-" n)
               (wordlist 2 (words n)
                         (concat ". " (u+1 (foreach d n (subst d "." ".iiiiiiiii")))))
               n)
           (findstring "-" n)))


;; Subtract two unsigned numbers, returning a (possibly negative) number.
;;
;; a - b = a + (NINES - b) + -NINES
;; NINES-b = for each digit in b, 9 - digit
;; -NINES  = -100... + 1
;;
;; We pass a negative number of 'u+', but first ensure that the negative
;; digit is above the range of digits that might generate carries.
;;
(define (u- a b)
  (let& ((wideb (subst ".." "." (join (subst "i" "" a) b)))
         (negb (foreach d wideb (subst d "." ".iiiiiiiii")))
         (size (words a)))
        (nnorm (subst ".i-" "."
                      (u+ a (concat (join negb "i") " .-"))))))


;; Compare two unsigned encoded numbers, returning:
;;   1 if a > b
;;   2 if b > a
;;   nil otherwise
(define (ucmp a b)
  (if (or a b)
      (or (ucmp (rest a) (rest b))
          (if (findstring (concat (word 1 b) "i") (word 1 a)) 1)
          (if (findstring (concat (word 1 a) "i") (word 1 b)) 2))))

;; Compare two decimal numbers; result same as ucmp.
;;
(define (cmp a b)
  (let ((sa (sign a))
        (sb (sign b))
        (ua (uencode a))
        (ub (uencode b)))
    (if (xor sa sb)
        ;; different signs ... but check for 0 and -0
        (and (or (findstring "i" ua) (findstring "i" ub))
             (if sb 1 2))
        ;; same sign: compare abs vals and invert result if both are negative
        (filter "1 2" (subst "-2" 1 "-1" 2 (concat sa (ucmp ua ub)))))))


(define (nodd n)
  (findstring "i" (subst "ii" "" (word 1 n))))


;; Divide by two, rounding down.
(define (u/2 n)
  (nnorm (rest (ucarry (subst "i" "iiiii" n)))))


(define (u^2 n)
  (u* n n))


(define (u^ a b)
  (if (nodd b)
      (nnorm (u* a (u^2 (u^ a (u/2 b)))))
      (if (findstring "i" b)
          (nnorm (u^2 (u^ a (u/2 b))))
          ".i")))


;;--------------------------------------------------------------
;; Integer operators
;;--------------------------------------------------------------

(define (num+ a b)
  (let ((sa (sign a))
        (sb (sign b))
        (ua (uencode a))
        (ub (uencode b)))
  (if (xor sa sb)
      (ndecode (if sb
                   (u- ua ub)
                   (u- ub ua)))
      (udecode (u+ ua ub) sa))))


;; Avoid conflict with Make's `$+` automatic variable
(define `(+ a b)
  &public
  (num+ a b))


(define (- a b)
  &public
  (+ a (subst "--" "" (concat "-" b))))


(define (num* a b)
  (udecode (u* (uencode a) (uencode b))
            (xor (sign a) (sign b))))

;; Avoid conflict with Make's `$*` automatic variable
(define `(* a b)
  &public
  (num* a b))


(define (num^ a b)
  &public
  (let ((sa (sign a))
        (sb (sign b))
        (ua (uencode a))
        (ub (uencode b)))
    (if sb
        "nan"
        (udecode (u^ ua ub)
                 (and sa (nodd ub) "-")))))

;; Use macro to avoid conflict with Make's `$^` automatic variable.
(define `(^ a b)
  &public
  (num^ a b))


(define `(== a b) &public (not (cmp a b)))
(define `(!= a b) &public (if (cmp a b) 1))
(define (> a b)   &public (filter 1 (cmp a b)))
(define `(< a b)  &public (> b a))
(define (>= a b)  &public (not (< a b)))
(define `(<= a b) &public (>= b a))

(define (max a b) &public (if (> b a) b a))
(define (min a b) &public (if (< b a) b a))

(define `(abs n)  &public (patsubst "-%" "%" n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Convert up to three digits to string of that many i's.
;;  (topdigits "? ? .iii .ii .i" 3)  ->  <123-char-long string of i's>
(define (topdigits num at)
  (let ((digits (nth-rest at num)))
    (subst "." ""
           (concat (word 1 digits)
                   (subst "i" "iiiiiiiiii"
                          (concat (word 2 digits)
                                  (subst "i" "iiiiiiiiii"
                                         (word 3 digits))))))))


;; Divide a (string of i's) by b (string of i's)
(define (strdiv a b)
  (subst b "I" "i" "" "I" "i" a))


;; Compute range of possible values for next digit
(define (digit-range a b more-a? more-b? at)
  (let ((atop (topdigits a at))
        (btop (topdigits b at))
        (more-a? more-a?)
        (more-b? more-b?))
    (let& ((dmin (strdiv atop (concat btop (if more-b? "i"))))
           ;; limit max to 9
           (dmax (patsubst "iiiiiiiii%" "iiiiiiiii"
                           (strdiv (concat atop (if more-a? "i")) btop))))
          (concat "." dmin " ." dmax))))


(declare (longdiv a more-a b))


(define (longdiv-next digit remainder more-a b)
  (append (if more-a (longdiv (append (lastword more-a) remainder)
                              (butlast more-a)
                              b))
          digit))


;; (longdiv a more-a b)
;;
;;   b is the divisor
;;   a is some value >= 0 and < b*10
;;   more-a holds the remaining (LSB) digits of the quotient
;;
;; We sample the top two digits of b and corresponding MSB digits of a and
;; then use strdiv to constrain possibilities for the next digit.
;;
;;   btop = 1 ... 99    [top two digits, unless b has only one digit]
;;   atop = 0 ... 999   [up to top three digits]
;;
;; When b has two or fewer digits, this is an exact answer.  When b has more
;; digits, it lets us bound the range of possibilities, since atop and btop
;; bound the range of possibilities for a and b: they essentially round down
;; to the next whole number since less significant digits are discarded.
;;
;;   min digit => atop / (btop + 0.999...)   [+0.999 if b has more the two digits]
;;   max digit => (atop + 0.999...) / btop    [+0.999 if more-a non-empty]
;;
;; The range includes at most two possible values for the next digit.
;; Proof: Absolute variance is greatest where digits are high (small
;; variations in b are amplified).  The highest range that would include
;; three values is 7...9.  The minimum value for btop is 10 (except in cases
;; where we get an exact answer).  The largest atop that would yield a
;; *minimum* of 7 is 87: 87 / 11 => 7.  The *maximum* possible digit in this
;; cases is 88 / 10 => 8.
;;

(define (longdiv a more-a b)
  (let ((range (digit-range a b more-a (word 3 b)
                            (patsubst 0 1 (words (rest b)))))
        (a a)
        (b b)
        (more-a more-a))
    (let& ((dmin (word 1 range))
           (dmax (word 2 range)))
          (let ((rmin (u- a (ucarry (subst "i" (subst "." "" dmin) b))))
                (a a)
                (b b)
                (more-a more-a)
                (dmin dmin)
                (dmax dmax))
            ;; Use DMIN if it's the same as DMAX or if DMAX is too big
            (if (or (eq? dmin dmax)
                    (filter 2 (ucmp rmin b)))
                (longdiv-next dmin rmin more-a b)
                (longdiv-next dmax (u- rmin b) more-a b))))))


;; work on LenB digits at a time
;;   d c b a, z y ==>    b a,  z y,  d c
(define (u/ a b ?more-a)
  (if (word (words (concat ". " b)) a)
      ;; a is longer than b
      (u/ (rest a) b (append more-a (first a)))
      (longdiv a more-a b)))


(define (/ a b)
  &public
  (let ((sa (sign a))
        (sb (sign b))
        (ua (nnorm (uencode a)))
        (ub (nnorm (uencode b))))
    (if (not ub)
        "nan"
        (udecode (u/ ua ub)
                 (xor sa sb)))))


(define (umod a b)
  (let ((a a)
        (b b)
        (bi (subst "." "" b))
        (m10 (subst (subst "." "" b) "" "iiiiiiiiii")))
    (if (eq? m10 "")
        ;; mod(a, 1|2|5) = mod(least_significant_digit_of_a, 1|2|5)
        (subst bi "" (word 1 a))
        (if (eq? m10 "i")
            ;; mod(a, 3|9) = mod(sum_of_digits_of_a, 3|9)
            (concat "." (subst "." "" " " ""  bi "" a))
            (u- a (u* (u/ a b) b))))))


(define (mod-10 n)
  (if (filter "%1 %2 %3 %4" n)
      (if (filter "%1 %2" n)
          (if (filter "%1" n) 1 2)
          (if (filter "%3" n) 3 4))
      (if (filter "%5 %6 %7 %8" n)
          (if (filter "%5 %6" n)
              (if (filter "%5" n) 5 6)
              (if (filter "%7" n) 7 8))
          (if (filter "%9" n)
              9
              0))))

(define (mod-10 n)
  (if (filter "%8 %9" n)
      (if (filter "%8" n) 8 9)
      (words (concat (filter "%1 %3 %5 %7 %9" n)
                     (if (filter "%2 %3 %6 %7" n) " 1 1")
                     (if (filter "%4 %5 %6 %7" n) " 1 1 1 1")))))

(define (div-10 n)
  (patsubst (concat "%" (mod-10 n)) "%" n))

(define (mod-1 n)
  0)

(define (mod-2 n)
  (if (filter "%1 %3 %5 %7 %9" n) 1 0))

(define (mod-3 n)
  (or (subst 0 "" 9 "" 6 "" 3 "" 7 1 4 1 8 11 5 11 2 11 111 "" 11 2 n)
      0))

(define (mod-5 n)
  (word (subst 0 10 (mod-10 n)) "1 2 3 4 0 1 2 3 4 0"))

(define (mod-8 n)
  (subst 9 1 8 0 (mod-10 n)))

(define (mod-9 n)
  (words
   (subst 0 "" 9 "" 8 71 7 61 6 51 5 41 4 31 3 21 2 11 111111111 "" 1 "1 " n)))

(define (mod a b)
  &public
  (declare (mod-))

  (if (filter "1 2 3 5 9 10" b)
      (call (concat (global-name mod-) b) a)
      (let ((sa (sign a))
            (ua (nnorm (uencode a)))
            (ub (nnorm (uencode b))))
        (if (not ub)
            "nan"
            (udecode (umod ua ub) sa)))))



;; (range MIN MAX) --> list/vector of numbers from MIN through MAX inclusive.
;; MIN and MAX must be non-negative integers.
;;
;; This optimized version avoids O(n^2) performance that results from
;; concatenation.
;;
(define (u-range min max)
  (if (>= min max)
      (if (== min max)
          min)
      (if (filter "%0" min)
          (if (filter "%9" max)
              (concat
               ;; Avoid leading zero on multi-digit results
               (if (filter 0 min)
                   "0 1 2 3 4 5 6 7 8 9 ")
               (foreach n (u-range (or (patsubst "%0" "%" min) 1)
                                   (or (patsubst "%9" "%" max) 0))
                        (concat n "0 " n "1 " n "2 " n "3 " n "4 "
                                n "5 "n "6 " n "7 " n "8 " n "9 ")))
              (concat (u-range min (- max 1)) " " max))
          (concat min " " (u-range (1+ min) max)))))


(define (0- n)
  (subst "--" "" (concat "-" (or n 0))))

(define (range min max)
  &public
  (strip
   (if (>= min 0)
       (u-range min max)
       ;; include negative range
       (concat (addprefix "-" (reverse (u-range (if (< max 0) (0- max) 1)
                                                (0- min))))
               " "
               (if (>= max 0)
                   (u-range 0 max))))))


(define (sum-small list)
  (if list
      (+ (word 1 list) (sum-small (rest list)))
      0))

(define (sum list)
  &public
  (if (word 50 list)
      (+ (sum (wordlist 1 (/ (words list) 2) list))
         (sum (nth-rest (1+ (/ (words list) 2)) list)))
      (sum-small list)))


;; Return number of characters in N, assuming N is a valid number.
;;
(define `(num-length num)
  (words (subst "0" "0 " "1" "1 " "2" "2 " "3" "3 " "4" "4 "
                "5" "5 " "6" "6 " "7" "7 " "8" "8 " "9" "9 "
                "." ". " "-" "- "
                num)))

;; Return 1 if NUM is equal to 0.
;;
(define `(is-zero num)
  (if (subst "0" "" "-" "" "." "" num) nil 1))


;; Return list of words at least WIDTH long, repeating PADDING as necessary.
;;
(define (extend-pad width padding)
  (if (word width padding)
      padding
      (extend-pad width (concat padding " " padding))))


;; Pad a number NUM to FIELD-WIDTH (if it is not that wide) with the
;; character in PAD-CHAR.  If PAD-CHAR is "0", insert padding after a
;; leading "-" (if any).  PAD-CHAR defaults to " ".
;;
(define (num-pad num field-width ?pad-char)
  &public
  (define `padding
    (subst " " "" "~" (or pad-char " ")
           (rest (wordlist (num-length num) field-width
                           (extend-pad field-width "~ ~ ~ ~ ~ ~ ~ ~")))))

  (if (is-zero field-width)
      ;; add no padding
      num
      (concat (subst "-" (sign num)
                     "P" padding
                     (if (filter 0 pad-char) "-P" "P-"))
              (abs num))))
