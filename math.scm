;; # math: Numeric Operations
;;
;; The `math` library implements operations on numbers.
;;
;; Arbitrary-precision arithmetic is supported.  There is no *a priori*
;; limit imposed on the size of numbers.
;;
;; ## Numbers
;;
;; Numbers are represented as strings of decimal digits, with an optional
;; sign, decimal point, and E-notation suffix.  More precisely:
;;
;;     Number  := "-"? Integer ("." Integer)? Exp?
;;     Integer := ( "0" | "1" | ... | "9" )+
;;     Exp     := ("E" | "e") ("+" | "-" | "") Integer
;;
;; Strings not conforming to the above syntax are treated as non-number
;; values.  Functions in this library typically return `"NaN"` when a
;; numeric argument was given a non-number value, or when the results are
;; otherwise undefined.  `"NaN:P"` may be returned when an invalid precision
;; parameter is provided.
;;
;; Comparison operators treat non-number values as less than all numeric
;; values, but equivalent to all other non-number values.
;;
;; ## Precision
;;
;; For many operators -- such as `+`, `-`, `*`, `//`, `mod`, and `^` -- the
;; result is always numerically exact.  Some functions -- like `/`, `log`,
;; `sin`, etc. -- yield an approximation with a limited number of digits.
;; These functions accept an optional argument for specifying precision;
;; otherwise the default is 16 significant digits (slightly more precise
;; than 64-bit IEEE-754 binary floating point).
;;
;; Precision can be specified in two ways: significant digits, or place.
;;
;; 1. A positive decimal integer specifies a number of significant digits in the
;;    result.
;;
;; 2. An integer N preceded by a `+` or `-` character specifies the *place*
;;    (relative to the decimal point) of the least significant digit as the
;;    place with with value of 10^N.  (Note that SCAM numeric literals may
;;    not begin with "+", so places beginning with "+" must be quoted.)
;;
;; The functions `round` and `/` guarantee rounding to the *nearest* unit in
;; the least significant digit, but more generally the guarantee is within
;; one unit of the least significant digit.
;;
;; Examples:
;;
;;     (/ 200 3 5)    ->  66.667        5 significant digits
;;     (/ 200 3 -1)   ->  66.7          10⁻¹ is least significant place
;;     (/ 200 3 "+0") ->  67            10⁰ is least significant place
;;     (/ 200 3 "+1") ->  70
;;     (/ 200 3 "+2") -> 100            rounding to nearest 10²
;;     (/ 200 3 "+3") ->   0            rounding to nearest 10³


(require "core.scm")
(require "math0.scm")
(require "math1.scm")
(require "math2.scm")


(define (ln x ?p)
  (fp2d (fp-ln (d2fp x) (prec-to-pod p))))


;; Return non-nil if either A or B are not simple integers: "-"? DIGIT+
;; Assumes that neither A nor B are nil.
;;
(define `(non-integers? a b)
  (subst "9-0" nil 9 nil 1 nil 0 nil (.. 9 a 9 b)))


;; Round X
;;
;; X is decimal (raw input from client).
;; POD is in internal format (0% => place, else decimal)
;; DIR = one of {ROUND-TRUNC, -FLOOR, -NEAR, -CEIL}
;;
;; Result is decimal.
;;
(define (raw-round x pod dir)
  (fp2d (fp-round (d2fp x) pod dir)))


;;--------------------------------
;; Operators
;;--------------------------------

;; `op` dispatches to computed function names after validating the
;; input numbers.
;;     (uMODE UX UY ?POD) --> U  : valid U integers (no "." or "E...")
;;     (fMODE FX FY ?POD) --> F  : valid FP values


(define (u-mod x y)
  (if (findstring "-" (.. x y))
      (fp2u (f-mod (u2fp x) (u2fp y)))
      (or (u-divx x y ROUND-REMAINDER)
          NaN)))


(define (f-atan2 y x pod)
  (fp-round (fp-atan2 y x pod) pod ROUND-NEAR))


(define `(fp>=0? x)
  (or (findstring "+" x)
      (not (findstring 1 (fp.uf x)))))


;; currently defined only for non-negative integer y
(define (f^ x y)
  (if (fp>=0? y)
      (let ((yuf (fp.uf y))
            (len (u2d (fp.xpo y)))
            (x x))
        (and (n>0? len)
             (not (findstring 1 (rest (nth-rest len yuf))))
             (fp-u-pwr x (uf-fix yuf len))))))


(define (u^ x y)
  (fp2u (fp-u-pwr (u2fp x) y)))


;; forward u->f
(define (u-pow x y pod)   (fp2u (f-pow (u2fp x) (u2fp y) pod)))
(define (u*~ x y pod)     (fp2u (f*~ (u2fp x) (u2fp y) pod)))
(define (u/ x y pod)      (fp2u (f/ (u2fp x) (u2fp y) pod)))
(define (u-log x b pod)   (fp2u (f-log (u2fp x) (u2fp b) pod)))
(define (u-atan2 y x pod) (fp2u (f-atan2 (u2fp y) (u2fp x) pod)))


;; NAME = suffix for constructing `uNAME` and `fNAME` functions
;; X, Y = decimal numbers
;; POD = (prec-to-pod ...) output
;; IS-CMP => pass nil values to f<NAME>, do not call convert result to decimal.
;;
(define `(op-macro name x y pod is-cmp)
  (declare (u x y))
  (declare (f x y))
  (define `ufn (.. (native-name u) name))
  (define `ffn (.. (native-name f) name))
  (define `(d2u-macro d)
    (subst 1 "01" 2 "011" 4 "31" 5 "311"
           7 "61" 9 "81" 8 "611" 6 "3111" 3 "0111" d))

  (define `uresult
   (foreach (pair (d2u-macro (subst " " "?" (.. x ":" y))))
     (define `ux (filter-out ":%" (subst ":" " :" pair)))
     (define `uy (word 2 (subst ":" ": " pair)))
     ;; At least one digit, and no non-digit characters except leading "-"?
     (if (filter 99 (subst ":-" ":" ":0" 9 1 nil 0 nil (.. ":" pair)))
         ;; unsigned integer
         (native-call ufn ux uy pod)
         ;; not simple unsigned integers
         (if is-cmp
             (native-call ffn (u2fp ux) (u2fp uy) pod)
             (fp2u
              (let ((fx (u2fp ux))
                    (fy (u2fp uy))
                    (ffn ffn)
                    (pod pod))
                (if (and fx fy)
                   (native-call ffn fx fy pod))))))))
  (if is-cmp
      uresult
      (u2d uresult)))


;; The compiler optimizes this function body for the non-cmp case.
(define (op name x y ?pod)
  (op-macro name x y pod nil))

(define `(op-cmp name x y pod)
  (op-macro name x y pod 1))


(define `(check-dir dir)
  (or (if dir
          (filter [ROUND-FLOOR ROUND-CEIL ROUND-TRUNC]
                  (subst "-" ROUND-FLOOR "+" ROUND-CEIL "|" ROUND-TRUNC dir)))
      ROUND-NEAR))


(define (reduce opfn v)
  (if (word 2 v)
      (native-call opfn (first v) (reduce opfn (rest v)))
      (first v)))


(define (u-mul x y) (u* x y))
(define (u-add x y) (u+ x y))
(define (fp-add x y) (f+ x y))
(define (fp-mul x y) (f* x y))
(define (fp-mulp x y p) (f*~ x y p))


;; Use `fp-NAME` or `u-NAME` to add/multiply numbers in a vector.
;;
(define (vecop name uv ident)
  (declare (u-))
  (declare (fp-))
  (define `fpvec
    (foreach (u uv)
      [ (u2fp u) ]))

  (if uv
      (u2d
       (if (non-digit? (smash uv))
           (fp2u (reduce (.. (native-name fp-) name) fpvec))
           (reduce (.. (native-name u-) name) uv)))
      ident))


(define `(addv uv)
  (vecop "add" uv 0))


(define `(mulv uv)
  (vecop "mul" uv 1))


(define (sum-vec v ?prev)
  (if (findstring "!" v)
      (if (findstring prev v)
          "NaN"
          (sum-vec (strip (promote v)) v))
      (if v
          (addv (d2u v))
          0)))


(define (cmp x y)
  (op-cmp "-cmp" x y nil))


;;--------------------------------
;; Exports
;;--------------------------------


;; Return the sum of all arguments.
;;
(define `(+ x y ...others)
  &public
  (if others
      (addv (d2u (._. x y others)))
      (op "+" x y)))


;; Return X - Y.
;;
(define `(- x y)
  &public
  (op "-" x y))


;; Return the product of all arguments.
;;
(define `(* x y ...others)
  &public
  (if others
      (mulv (d2u (._. x y others)))
      (op "*" x y)))


;; Return floor(X / Y): the largest integer less than or equal to X/Y.
;;
(define `(// x y)
  &public
  (op "//" x y))


;; Return X / Y to the [precision specified by P](#precision).  The answer
;; will be rounded to the *nearest* unit in the least significant digit.
;;
(define `(/ x y ?p)
  &public
  (op "/" x y (prec-to-pod p)))


;; Compute X*Y to the [precision specified by P](#precision).  The result
;; will be within one unit of the least significant digit.
;;
(define `(*~ x y ?p)
  &public
  (op "*~" x y (prec-to-pod p)))


;; Raise X to the power of Y.  Y must be an non-negative integer in "simple"
;; format (without a decimal point or E-notation).
;;
(define `(^ x y)
  &public
  (op "^" x y))


;; Return the remainder of floor(X/Y).
;;
;; `(mod X Y)` is equal to `(- X (* (// X Y) Y))`.
;;
(define `(mod x y)
  &public
  (op "-mod" x y))


;; Calculate the logarithm of X to the [precision given by P](#precision).
;;
;; B, if given, is the base; if nil, the natural logarithm of X will be
;; returned.
;;
;; X and B must be be greater than 0.
;;
(define (log x ?b ?p)
  &public
  (if b
      (op "-log" x b (prec-to-pod p))
      (ln x p)))


;; Calculate eˣ to the [precision given by P](#precision).
;;
(define (exp x ?p)
  &public
  (u2d (fp2u (fp-exp (u2fp (d2u x)) (prec-to-pod p)))))


;; Compute xʸ to the [precision given by P](#precision).
;;
;; X must be non-negative.
;;
(define (pow x y ?p)
  &public
  (op "-pow" x y (prec-to-pod p)))


;; Return the angle between the X axis and the line from the origin to the
;; point (X,Y).  (**Note** that Y is the first argument and X is the
;; second.)
;;
;; The result is in the range (-π,π).
;;
;; The [precision is given by P](#precision).
;;
(define (atan2 y x ?p)
  &public
  (op "-atan2" y x (prec-to-pod p)))


;; Return the greatest integer less than or equal to X.
;;
(define `(floor x)
  &public
  (raw-round x 0 ROUND-FLOOR))


;; Return the smallest integer greater than or equal to X.
;;
(define `(ceil x)
  &public
  (raw-round x 0 ROUND-CEIL))


;; Return the integer portion of X (rounding towards zero).
;;
(define `(trunc x)
  &public
  (raw-round x 0 ROUND-TRUNC))


;; Round X to the [precision given by P](#precision).
;;
;; **Note:** P defaults to `"+0"`, unlike other functions accepting
;; precision values.
;;
;; DIR is one of the following:
;;  - `"+"` ⇒ round up (ceiling)
;;  - `"-"` ⇒ round down (floor)
;;  - `"|"` ⇒ round towards zero (truncate)
;;
(define (round x ?p ?dir)
  &public
  (raw-round x (if p (prec-to-pod p) 0) (check-dir dir)))


;; Return 1 if X > Y, nil otherwise.
;;
(define `(> x y)
  &public
  (findstring 1 (cmp x y)))


;; Return 1 if X < Y, nil otherwise.
;;
(define `(< x y)
  &public
  (> y x))


;; Return 1 if X != Y, nil otherwise.
;;
(define `(!= x y)
  &public
  (if (cmp x y) 1))


;; Return 1 if X = Y, nil otherwise.
;;
(define (= x y)
  &public
  (or (eq? x y)
      (not (cmp x y))))


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


;; Sum all numbers in ARGS.  Each argument may be a number, or a vector of
;; numbers, or a vector of vectors of numbers, and so on.
;;
(define (sum ...args)
  &public
  (sum-vec args))


;; Negate a number.  This function assumes that X is a valid number; it does
;; not validate or canonicalize X.
;;
(define `(0- x)
  &public
  (subst "--" "" (.. "-" x)))


;; Absolute value of a number.  This function assumes that X is a valid
;; number; it does not validate or canonicalize X.
;;
(define `(abs x)
  &public
  (patsubst "-%" "%" x))


;; Return the larger of X or Y.
;;
(define (max x y)
  &public
  (if (< x y)
      y
      x))


;; Return the smaller of X or Y.
;;
(define (min x y)
  &public
  (if (> x y)
      y
      x))


;; Compute the sine of X to the [precision given by P](#precision).
;;
(define `(sin x ?p)
  &public
  (xsin 1 x p))


;; Compute the cosine of X to the [precision given by P](#precision).
;;
(define `(cos x ?p)
  &public
  (xsin nil x p))


;; Compute π to the [precision given by P](#precision).
;;
(define (get-pi ?p)
  &public
  (or (foreach (pod (prec-to-pod p))
        (define `count
          (if (pod-is-place? pod)
              (u-zeros (u-add-ones pod 1))
              (repeat-words 0 pod)))

        (or (patsubst "31%" "3.1%"
                      (u2d (smash (uf-trim-tz (const-pi count)))))
            0))

      "NaN:P"))


;; Return the angle between the X axis and the line from the origin to the
;; point (X,Y).  (**Note** that Y is the first argument and X is the
;; second.)
;;
;; The result is in the range (-π,π).
;;
;; The [precision is given by P](#precision).
;;
(define (atan2 y x ?p)
  &public
  (or (foreach (pod (prec-to-pod p))
        (fp2d (fp-round (fp-atan2 (d2fp y) (d2fp x) pod)
                        (or result-pod pod) ROUND-NEAR)))
      "NaN:P"))


;; Return the arctangent of M to the [precision given by P](#precision).
;;
;; The result is in the range (-π/2,π/2).
;;
(define (atan m ?p)
  &public
  (atan2 m 1 p))


;; Return the fraction and exponent portions of X.
;;
;; Result = [M E] where X = M * 10ᴱ and E is an integer.
;;  - When X ≠ 0, 0.1 ≤ abs(M) < 1.
;;  - When X = 0, Result is [0 0].
;;  - When X is not a number, Result is nil.
;;
(define (frexp10 x)
  &public
  (let ((fx (uf-trim-tz (d2fp x))))
    (if (word 3 fx)
        (u2d (.. (findstring "-" (fp.sign fx))
                 (.. "0." (smash (fp.uf fx)))
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

  (wordlist (u2d-digit (.. skip-start skip-end 1))
            (words lst)
            (._. (subst 0 nil 1 "1 " skip-end) lst)))


;; MIN and MAX are unsigned UV numbers.
;;
(define (uv-range min max)
  (define `(uv/10 x)
    (filter-out "%x" (.. x "x")))

  (define `(x10 lst)
    (foreach (n lst)
      (.. n "0 " n "1 " n "2 " n "3 " n "4 "
          n "5 "n "6 " n "7 " n "8 " n "9 ")))

  (if (u-lt? max min)
      nil
      (uv-trim (lastword min)
               (subst (lastword max) "0" U9)
               (.. (if (not (word 2 min))
                       "0 1 2 3 4 5 6 7 8 9 ")
                   (x10 (uv-range (or (uv/10 min) 01) (uv/10 max)))))))


;; UA, UB = absolute value of A and B (UV numbers)
;; A<0, B<0 = truthy when A, B are negative
;;
(define (uv-sign-range ua ub a<0 b<0)
  (..
   ;; Negative range: A ... min(B,-1)
   (if a<0 (.. (addprefix "-" (reverse (uv-range (if b<0 ub 1) ua))) " "))
   ;; Non-negative range: max(A,0) ... B
   (if b<0 nil (uv-range (if a<0 0 ua) ub))))


(define `(fp-range a b)
  nil)


(define (raw-range a b)
  ;; Remove sign and extraneous leading zeros.
  (define `(u-prep u)
    (or (native-strip
         (subst "0" " 0" (filter "01%" (subst "-" nil "01" " 01" u))))
        0))

  (strip
   (if (non-integers? a b)
       (fp-range (u2fp a) (u2fp b))
       (uv-sign-range (u-prep a) (u-prep b) (u<0? a) (u<0? b)))))


;; Return a vector of numbers ranging from X to Y.  X and Y must be integers
;; in "simple" format (without a decimal point or E-notation).
;;
(define (range x y)
  &public
  (raw-range (d2u x) (d2u y)))


;;--------------------------------
;; format-fixed
;;--------------------------------


;; Convert X to a fixed-point representation.
;;
;; MIN-WIDTH = Minimum field width (nil => 0).  Padding with spaces on the
;;     left will be added as necessary.\
;; DECIMALS = Number of digits to the right of the decimal; nil => 0.  If
;;     less than or equal to zero, no decimal point is included,.
;;
(define (format-fixed x ?min-width ?decimals)
  &public
  (foreach (fmt (d2u (.. (or min-width 0) ":" (or decimals 0))))
    (define `udec (word 2 (subst ":" " " fmt)))
    (if (filter ":" (subst 0 nil 1 nil fmt))
        (fp-fix (fp-round (d2fp x) udec ROUND-NEAR)
                min-width
                decimals
                udec)
        "[INVALID_FORMAT]")))


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
  (subst (.. 9 U9) "9909"
         9 U9
         (.. (subst 1 nil 0 9 u) u)))


(define (lex-exp u)
  (if (n>0? u)
      (patsubst "0111111111%" "%" (lex-uns u))
      (u-complement-digits (lex-uns (subst "-" nil u)))))


(define (fp-lex n)
  (if (fp<0? n)
      (.. "-" (u-complement-digits (fp-lex (fp-negate n))) ":")
      (if (findstring 1 (fp.uf n))
          (.. (lex-exp (fp.xpo n)) (fp.uf n))
          "0")))


;; Convert a number to a string.  The *lexical* sort order of multiple
;; results corresponds to the *numeric* sort order of the numbers.  In other
;; words, for two numbers X and Y, LX = `(num-lex X)`, and LY = `(num-lex
;; Y)`, then:
;;
;;     (< X Y)  <==>  (sort [LX LY]) == [LX LY]
;;
;; This can be used with `sort-by` to obtain numeric sort order.  E.g.:
;;
;;     (sort-by (lambda (i) (num-lex (nth 2 i))) ...)
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
    (foreach (elem v)
      (.. (num-lex (word 1 (subst "!" " " elem)))
          "!#"
          elem)))
  (filter-out "%!#" (subst "!#" "!# " (sort prefixed-v))))
