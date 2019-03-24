;;----------------------------------------------------------------
;; math : numeric library
;;----------------------------------------------------------------

(require "core.scm")
(require "math0.scm" &private)
(require "math1.scm" &private)
(require "math2.scm" &private)

;;--------------------------------
;; Operators
;;--------------------------------

;; RAW values are strings from the client in which decimal digits have been
;; converted to unary digits (0, 01, 011, ...), and nil values have been
;; replaced by "?".


;; Functions beginning "raw-" generally accept RAW values and return
;; U-numbers or "NaN".
;;
(declare (raw-))


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


;; Divide A by B, rounding down to the nearest integer (flooring).
;;
(define (raw-fdiv a b)
  (if (non-digit? (concat a b))
      (fp2u (fp-div (u2fp a) (u2fp b) "0" nil))
      (or (u-fdiv a b DIV-TRUNCATE)
          NaN)))


;; Return A modulo B -- the remainder after (fdiv A B).
;;
(define (raw-mod a b)
  (if (non-digit? (concat a b))
      (fp2u (fp-mod (u2fp a) (u2fp b)))
      (or (u-fdiv a b DIV-REMAINDER)
          NaN)))


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


;;--------------------------------
;; Exports
;;--------------------------------

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
;; digits.  PREC is as specified for `/`, *except* it defaults
;; to "+0".
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


;;--------------------------------
;; Misc.
;;--------------------------------


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


;; Return the fraction and exponent portions of X.
;;
;; Result = [M E] where X = M * 10ᴱ and E is an integer.
;;   When X ≠ 0, 0.1 ≤ abs(M) < 1.
;;   When X = 0, Result is [0 0].
;;   When X is not a number, Result is nil.
;;
(define (frexp10 x)
  &public
  (let ((fx (uf-trim-tz (d2fp x))))
    (if (word 3 fx)
        (u2d (concat (findstring "-" (fp.sign fx))
                     (concat "0." (smash (fp.uf fx)))
                     " "
                     (fp.xpo fx)))
        (if fx
            "0 0"))))


;;--------------------------------
;; range
;;--------------------------------


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


;;--------------------------------
;; format-fixed
;;--------------------------------


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


;;--------------------------------
;; num-lex, num-sort
;;--------------------------------

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
  (if (n>0? u)
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


;;--------------------------------
;; Transcendentals
;;--------------------------------


;; Calculate the natural logarithm of X.
;; For X<=0, the result is NaN.
;;
;; PREC is as documented for `/`.
;;
(define (log x ?b ?prec)
  &public
  (fp2d
   (if b
      (fp-log-x-b (d2fp x) (d2fp b) (prec-to-pod prec))
      (fp-log (d2fp x) (prec-to-pod prec)))))


;; Calculate eˣ.
;;
;; PREC is as documented for `/`.
;;
(define (exp x ?prec)
  &public
  (u2d (fp2u (fp-exp (u2fp (d2u x)) (prec-to-pod prec)))))


;; Compute xʸ
;;
;; X must be non-negative.
;; PREC is as documented for `/`.
;;
(define (pow x y ?prec)
  &public
  (fp2d (fp-pow (d2fp x) (d2fp y) (prec-to-pod prec))))

;; Compute the sine of X.
;;
;; PREC is as documented for `/`.
;;
(define `(sin x ?prec)
  &public
  (xsin 1 x prec))


;; Compute the consine of X.
;;
;; PREC is as documented for `/`.
;;
(define `(cos x ?prec)
  &public
  (xsin nil x prec))


;; Compute π.
;;
;; PREC is as documented for `/`.
;;
(define (get-pi ?prec)
  &public
  (or (foreach
          pod (prec-to-pod prec)

          (define `count
            (if (pod-is-place? pod)
                (u-zeros (u-add-ones pod 1))
                (nzeros pod)))

          (or (patsubst "31%" "3.1%"
                        (u2d (smash (uf-trim-tz (const-pi count)))))
              0))

      "NaN:PREC"))


;; Return the angle between the X axis and the line from the origin to
;; the point (x,y).  Clockwise = positive.  Result is in range (-π,π).
;;
;; PREC is as documented for `/`.
;;
(define (atan2 y x ?prec)
  &public
  (or (foreach
          pod (prec-to-pod prec)
          (fp2d (fp-round (fp-atan2 (d2fp y) (d2fp x) pod)
                          (or result-pod pod) DIV-NEAREST)))
      "NaN:PREC"))


;; Return the arctangent of M.  Result is in range (-π/2,π/2).
;;
;; PREC is as documented for `/`.
;;
(define (atan m ?prec)
  &public
  (atan2 m 1 prec))
