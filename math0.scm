;;----------------------------------------------------------------
;; math0 : operations on U and UF values
;;----------------------------------------------------------------

(require "core.scm")


(define `(+_+ a b)
  (concat a " " b))


;; Convert decimal digits to unary digits.
;;
(define `(d2u-macro d)
  (subst 1 "01" 2 "011" 4 "31" 5 "311"
         7 "61" 9 "81" 8 "611" 6 "3111" 3 "0111" (or d "?")))


;; Convert unary digits to decimal digits.
;;
(define `(u2d-macro u)
  (subst "0111" 3 "3111" 6 "611" 8 "81" 9 "61" 7
         "311" 5 "31" 4 "011" 2 "01" 1 u))


(define (d2u d)
  (d2u-macro d))


(define (u2d u)
  (u2d-macro u))


(define `(spread u)
  (subst 0 " 0" u))


(define `(smash uv)
  (subst " " nil uv))


(define `(u2uv u)
  (.strip (spread u)))


(define `U1 "01")
(define `U2 "011")
(define `U3 "0111")
(define `U4 "01111")
(define `U5 "011111")
(define `U6 "0111111")
(define `U7 "01111111")
(define `U8 "011111111")
(define `U9 "0111111111")
(define `U10 "01111111111")
(define `T9 "111111111")
(define `T10 "1111111111")


;; Non-nil if X > 0.  X may be in decimal *or* (perhaps signed) U format.
;;
(define `(n>0? x)
  (subst 0 nil (filter-out "-%" x)))


(define `(u<0? u)
  (findstring 1 (filter "-%" u)))


;; This is exported later in math.scm
(define `(0- x)
  (subst "--" "" (concat "-" x)))


;; Get leading zeros in UF value U.
;;
(define (uf-get-lz u)
  (subst ":" " " (filter-out "01%" (subst " " ":" ":01" " 01" u))))


;; Trim leading zeros from UF value U.
;;
(define (uf-trim-lz u)
  (subst ":" " " (filter "01%" (subst " " ":" ":01" " 01" u))))


;; Get trailing zeros in UF value U.
;;
(define `(uf-get-tz u)
  (subst "." " " (filter "%0" (subst "0 " "0." u))))


;; Trim trailing zeros from UF value U.
;;
(define `(uf-trim-tz u)
  (subst "." " " (filter-out "%0" (subst "0 " "0." u))))


(define `(uf-ends-in-0? u)
  (filter "09" (concat u "9")))


(define (u-carry-fn u nines zeros)
  (subst (concat nines 1) (concat 1 zeros)
         (if (findstring (concat nines nines 1) u)
             (u-carry-fn u (concat nines nines) (concat zeros zeros))
             u)))


;; Return a well-formed U value (with digit values 0...9) that has the same
;; numeric value as U.  U may have digits with values from 0 to 27, but MUST
;; already have enough digits to hold the resulting value.  If carry is
;; propagated from the top digit, a mal-formed result beginning with "1"
;; (not "01") will result.
;;
(define `(u-carry u)
  (foreach w (subst "01111111111" "10" u)
           ;; Now digits in W are <= 18.
           (if (findstring "01111111111" w)
               (u-carry-fn w U9 0)
               w)))


;; Return a well-formed UF value (with digit values 0...9) that has the same
;; numeric value of N, which may have digits with values from 0 to 27.
;;
(define `(uf-carry uf)
  (filter "0%" (spread (u-carry (smash uf)))))


;; Add A to B.  A, B, and result are UF-encoded.  Result is modulo 1.
;;
(define `(uf-add a b)
  (uf-carry (subst "10" "1" "00" 0 (join a b))))


;; Remove extraneous leading zeros from a U-encoded non-negative integer.
;;
(define `(u-norm-uns u)
  (concat 0 (smash (rest (subst "01" "0 1" u)))))


(define `(neg-digits u)
  (subst "1" "~" u))


(define `(neg-swap u)
  (subst "~" "*" 1 "~" "*" 1 u))


;; Combine 1's with adjacent ~'s (anti-1's).
;;
(define `(neg-rreduce u)
  (subst "111111~~~~~~" nil
         "111~~~" nil
         "11~~" nil
         "1~" nil
         u))


;; Return the 9's-complement of UV value.
;;
(define `(uv-complement-digits uv)
  (subst "1110111" 0
         "11011" 0
         "101" 0
         (patsubst "0%" "%0111111111"
                   (patsubst "0111111%" "%101111"
                             uv))))


;; Return 10's complement of UF.  The result is possibly not well-formed
;; (it may contain a digit with value 10).  The result is equivalent
;; to (1 - UF) mod 1.
;;
(define `(uf-complement uf)
  (concat (uv-complement-digits uf) 1))


;; Assert: at most one "~" in each digit
(define (borrow-n u zeros)
  (if (findstring (concat zeros "~") u)
      (subst (concat zeros "~") (concat "~" (subst 0 U9 zeros))
             "1~" nil
             (borrow-n u (concat zeros zeros)))
      u))


;; Zero-extend X on the left to have numdigits(X) + numdigits(Y) digits
;;
(define `(lzpad x y)
  (spread (concat (subst 1 "" y) x)))


;; Eliminate "~" digits by borrowing from 1's in more significant places.
;; On entry, digits in U may contain all 1's or all ~'s (up to 9 of them).
;; If there are no 1's to borrow from, one or more "~" digits may remain in
;; the result.
;;
(define `(borrow-macro u)
  (borrow-n (neg-rreduce (subst "0~" "~0111111111" u)) "0"))


(define (u-sub-unsigned a b)
  ;; add "unary negative" B to A
  (define `x
    (subst "10" "1" "00" 0
           (join (lzpad a b)
                 (lzpad (neg-digits b) a))))

  ;; Normalize but retain "-" if present as initial word.
  (define `(u-norm-x u)
    (patsubst "-0" 0 (smash (patsubst "0%" 0 (subst "01" "0 1" u)))))

  (foreach u (smash (neg-rreduce x))
           (if (findstring "~" u)
               (u-norm-x
                (borrow-macro
                 (if (filter "~%" (subst 0 nil u))
                     ;; negative
                     (concat "- " (neg-swap u))
                     ;; positive
                     u)))
               (u-norm-uns u))))


;; A and B must be non-negative, unsigned integers.
;;
;; A, B, and result are U strings containing only digits.
;;
(define `(u-add-unsigned a b)
  (u-norm-uns
   (u-carry
    (smash
     (subst "10" "1" "00" "0"
            (join (lzpad b a) (lzpad a b)))))))


(define (u-add-unsigned-fn a b)
  (u-add-unsigned a b))


;; Add two U-values, returning a normalized U-value (no extraneous leading
;; zeros, and no "-0" value).
;;
(define (u-add a b)
  (define `ua (subst "-" nil a))
  (define `ub (subst "-" nil b))

  (if (findstring "-" (concat a b))
      (if (findstring 1 (concat a b))
          (if (findstring "-" a)
              ;; a is negative
              (if (findstring "-" b)
                  ;; both negative
                  (concat "-" (u-add-unsigned-fn ua ub))
                  ;; b non-negative
                  (u-sub-unsigned b ua))
              ;; a non-negative
              (u-sub-unsigned a ub))
          0)
      (u-add-unsigned a b)))


(define `(u-sub a b)
  (u-add a (0- b)))


;; Subtract B from A.  Result is modulo 1.
;; E.g.:  0.1 - 0.2  -->  0.9
;;
(define `(uf-sub a b)
  ;; Assert: A and B are normalized numbers
  (uf-add a (uf-complement b)))


;; Combine 1's with ~'s after a join.  For performance, we minimize the
;; number of subst calls *and* the number of times that substitutions are
;; made within a string during the subst calls.
;;
(define `(cmp-reduce u)
  (subst "111110~~~~~" 0     ;; max remaining reduction is "11110~~~~"
         "10~" nil           ;; max remaining reduction is "111~~~"
         "11~~" nil
         "1~" nil
         0 nil
         u))


(define `(u-cmp-unsigned a b)
  (word 1 (cmp-reduce (join (lzpad a b) (lzpad (neg-digits b) a)))))


;; Compare A to B.  A and B can be U-numbers *or* UV-numbers.
;;
;;   If A>B, return "1..."  (word consisting of 1's)
;;   If A=B, return nil
;;   If A<B, return "~..."  (word consisting of ~'s)
;;
(define (u-cmp a b)
  (if (findstring "-" (concat a b))
      (if (findstring 1 (concat a b))
          (if (findstring "-" a)
              (if (findstring "-" b)
                  (u-cmp (subst "-" nil b) (subst "-" nil a))
                  "~")
              1))
      (u-cmp-unsigned a b)))


(define `(u-lt? a b)
  (findstring 1 (u-cmp b a)))


;; Compare A to B.  The return value is as for `u-cmp`.
;; A and B may contain extraneous spaces.
;;
(define (uf-cmp a b)
  (word 1 (cmp-reduce (join a (neg-digits b)))))


(define `(uf-lt? a b)
  (findstring 1 (uf-cmp b a)))


;; Subtract B from A, returning [SIGN ...RESULT].
;;   SIGN = "+" or "-"
;;   RESULT = UF-encoded number [0...1)
;;
(define (uf-sign-sub a b negate?)
  ;; expect normalized numbers
  (define `prefix+ (concat (if negate? "-" "+") " "))
  (define `prefix- (concat (if negate? "+" "-") " "))

  (define `(sub-reduce u)
    (subst "111110~~~~~" 0     ;; max remaining reduction is "11110~~~~"
           "10~" nil           ;; max remaining reduction is "111~~~"
           "11~~" nil
           "1~" nil
           "10" 1
           "00" 0
           u))

  (foreach u (smash (sub-reduce (join a (neg-digits b))))
           (if (findstring "~" u)
               (u2uv
                (borrow-macro
                 (if (filter "~%" (subst 0 nil u))
                     ;; negative
                     (concat prefix- (neg-swap u))
                     ;; positive
                     (concat prefix+ u))))
               (concat prefix+ (u2uv u)))))


;; Propagate carry until all digits are proper (9 or less).
;;
(define (u-carry-all u)
  (if (findstring "01111111111" u)
      (u-carry-all
       (patsubst "-1%" "-01%"
                 (patsubst "1%" "01%" (subst T10 "X0" "0X" 1 u))))
      u))


;; Increment U by one for each "1" in ONES.  ONES is a string containing
;; an arbitrary number of "1" characters.
;;
(define (u-add-ones e ones)
  (or (filter-out "-% %1111111111" (concat e ones))
      (u-add e (u-carry-all (concat "0" ones)))))


;; Subtract 1 from a U-value (possibly include a "-" sign).
;;
(define (u-1 e)
  (or (patsubst "%1" "%" (filter-out "-% %0" e))
      (filter-out "0% %1111111111" (concat e 1))
      (u-add e "-01")))


(define `(u+1 u)
  (or (filter-out "-% %1111111111" (concat u 1))
      (u-add u "01")))


;;--------------------------------
;; Multiplication
;;--------------------------------


;; Propagate carry from each digit to the next higher digit (without
;; cascading).
;;
(define `(uf-xcarry u)
  (subst T10 "X 0" " 0X" "1" u))


;; Reduce maximum digit value from 19 (or less) to 10.  Reduce maximum digit
;; by 9 when maximum digit is 19 or greater.
;;
(define `(uf-carry_19_10 u)
  (subst " 01111111111" "1 0" u))


;; Reduce maximum digit value from 27 to 10.
;;
(define `(uf-carry_27_10 u)
  (uf-carry_19_10 (uf-carry_19_10 u)))


;; Reduce maximum digit value from 45 to 27.
;;
(define `(uf-carry_45_27 u)
  (subst " 011111111111111111111" "11 0" u))


;; Reduce maximum digit value from 81 to 10.
;;
(define `(uf-carry_81_10 u)
  (uf-carry_19_10 (uf-xcarry u)))


(define `(>>1 uf)
  (concat "0 " uf))

(define `(j>>1 a b)
  (join a (concat "0 " b)))

(define `(j>>2 a b)
  (join a (concat "0 0 " b)))

(define `(j>>4 a b)
  (join a (concat "0 0 0 0 " b)))


;; Add two values without performing a carry operation (leaving large digit
;; values).
;;
(define `(uf-add-x a b)
  (subst "10" "1" "00" 0 (join a b)))


;; After one or more `join` operations, remove extraneous '0' characters and
;; propagate overflow from each digit to next higher digit (non-cascading).
;;
(define `(merge-and-carry u)
  (define `(space-carry u)
    (subst T10 "X " " X" "1" u))

  (concat 0 (subst " " " 0" (space-carry (subst 0 nil u)))))


;; True when A's length > B's length.
;;
(define `(longer? a b)
  (word (words (concat "1 " b)) a))


;; Multiply by single digit (UF values in & out), and do not carry.
;; Leaves digits with values up to 81.
;;
(define `(uf*digit_81 a digit)
  (>>1 (subst "1" (subst "0" nil digit) a)))


;; Multiply by single digit (UF values in & out).
;;
(define `(uf*digit a digit)
  (uf-carry (uf-xcarry (uf*digit_81 a digit))))


;; (uf*CONSTANT u)

(define `(uf*2 u)
  (uf-carry_19_10 (subst 1 11 u)))


(define `(uf*4 u)
  ;; Versus "(subst 1 1111 u) + carry20 + carry10", this is slightly smaller
  ;; and faster for large numbers.
  (subst " 011111" "x 0"
         "1" "xx"
         " 0xxxxx" "1 0"
         "x" "11" u))


(define `(uf*0.1 u)  (concat "0 " u))
(define `(uf*0.2 u)  (uf*2 (concat "0 " u)))
(define `(uf*0.4 u)  (uf*4 (concat "0 " u)))
(define `(uf*0.5 a)  (subst "11" "2" "1 0" " 011111" "2" 1 (concat a " 0")))

(define `(uf*0.3_10 u) (uf-carry_27_10 (>>1 (subst 1 111 u))))
(define `(uf*0.7_10 u) (uf-carry_81_10 (>>1 (subst 1 1111111 u))))
(define `(uf*0.9_10 u) (uf-carry_81_10 (>>1 (subst 1 111111111 u))))


;;----------------------------------------------------------------
;; `vmul` precomputes products for digits of the multiplier, and handles 9
;; digits per iteration.
;;
;; The pre-computed products are in improper form, with digit values up to
;; 11 (in the case of A*6).
;;
;; Times are surprisingly linear with the size of B:
;;    (len(B)+9) * (len(A)+80) * 0.135
;;----------------------------------------------------------------

;; VOODOO: Implement `native-var` before officially supported.
(declare (native-var name) &native)

;; Each function using `native-var` must be fixed-up using this:
(define (fix-native-var fname)
  (set-native-fn fname (subst "$(call native-var," "$(" (value fname))))


;; Multiply by a digit, given precomputed results:

(define (A*0 b u1 u2 u3 u4 u5 u7 u9) nil)
(define (A*01 b u1 u2 u3 u4 u5 u7 u9) u1)
(define (A*011 b u1 u2 u3 u4 u5 u7 u9) u2)
(define (A*0111 b u1 u2 u3 u4 u5 u7 u9) u3)
(define (A*01111 b u1 u2 u3 u4 u5 u7 u9) u4)
(define (A*011111 b u1 u2 u3 u4 u5 u7 u9) u5)
(define (A*0111111 b u1 u2 u3 u4 u5 u7 u9) (uf*2 u3))
(define (A*01111111 b u1 u2 u3 u4 u5 u7 u9) u7)
(define (A*011111111 b u1 u2 u3 u4 u5 u7 u9) (uf*2 u4))
(define (A*0111111111 b u1 u2 u3 u4 u5 u7 u9) u9)


(define (vmul-9 b u1 u2 u3 u4 u5 u7 u9)
  (declare (A*))
  (define `(d n)
    (native-var (concat (native-name A*) (word n b))))

  ;; max digit = 9*11=99 --> 9+9=18
  (merge-and-carry
   (j>>4 (j>>2 (j>>1 (d 1) (d 2))
               (j>>1 (d 3) (d 4)))
         (j>>2 (j>>1 (d 5) (d 6))
               (j>>1 (d 7) (j>>1 (d 8) (d 9)))))))


;; Multiply A by B.  U1, U3, etc., hold the values of A*1, A*3, etc.
;;
(define (vmul-loop b u1 u2 u3 u4 u5 u7 u9)
  (define `(>>9 u)
    (concat "0 0 0 0 0 0 0 0 0 " u))

  (if (word 10 b)
      (uf-carry_45_27
       (uf-add (>>9 (vmul-loop (nth-rest 10 b) u1 u2 u3 u4 u5 u7 u9))
               (native-var (native-name vmul-9))))
      (native-var (native-name vmul-9))))


(fix-native-var (native-name vmul-loop))
(fix-native-var (native-name vmul-9))

(declare (vmul a b))


;; (uf-mul A B) must produce len(A)+len(B) digits.  vmul does that only when
;; B ends in a non-zero digit, so we strip trailing zeros before calling it.
;;
(define (vmul-0 a b)
  (if (findstring 1 b)
      (concat (vmul a (uf-trim-tz b)) " " (uf-get-tz b))
      (concat (patsubst "%" 0 a) " " b)))


(define (vmul a b)
  (if (uf-ends-in-0? b)
      (vmul-0 a b)
      (vmul-loop b (uf*0.1 a) (uf*0.2 a) (uf*0.3_10 a) (uf*0.4 a)
                 (uf*0.5 a) (uf*0.7_10 a) (uf*0.9_10 a))))


;;----------------------------------------------------------------
;; uf-mul-fixed
;;
;; These functions are tailored to the size of the multiplier.  Each returns
;; numbers with maximum digit values of 27.
;;----------------------------------------------------------------

;; Multiply A by B if B has 1..8 digits.  Return nil otherwise.
;;
(define `(uf-mul-fixed a b)
  (declare (uf*b a b))
  (native-var (concat (native-name uf*b) (words b))))


(define (uf*b1 a b)
  (uf-xcarry (uf*digit_81 a b)))


(define (uf*b2 a b)
  (define `(d n)
    (uf*digit_81 a (word n b)))

  ;; max digit size:  81 + 81 = 172  -->  16 + 9 = 25
  (merge-and-carry
   (j>>1 (d 1) (d 2))))


;; B may hold more than three digits.
;; ETC, if given, will also be added to  the result (max. digit = 27).
;;
(define (uf*b3 a b ?etc)
  (define `(d n)
    (uf*digit_81 a (word n b)))

  ;; max digit size:  81 + 81 + 81 + 27 = 270 -->  9 + 27 = 36  --> 27
  (uf-carry_19_10
   (merge-and-carry
    (j>>2 (j>>1 (d 1) (d 2))
          (j>>1 (d 3) etc)))))


(define (uf*b4 a b) (uf*b3 a b (uf*b1 a (word 4 b))))
(define (uf*b5 a b) (uf*b3 a b (uf*b2 a (nth-rest 4 b))))
(define (uf*b6 a b) (uf*b3 a b (uf*b3 a (nth-rest 4 b))))
(define (uf*b7 a b) (uf*b3 a b (uf*b4 a (nth-rest 4 b))))
(define (uf*b8 a b) (uf*b3 a b (uf*b5 a (nth-rest 4 b))))


;; Multiply two UF values.  The result will have exactly (words A) + (words
;; B) digits.
;;
(define (uf-mul a b)
  (if (longer? b a)
      (uf-mul b a)
      (uf-carry
       (or (uf-mul-fixed a b)
           (vmul a b)))))


(fix-native-var (native-name uf-mul))


;;--------------------------------
;; Division
;;--------------------------------


(define `(merge2 w1 w2)
  (concat (subst 1 T10 w1) w2))


;; Compute second argument for `guess-digit`.
;;
(define `(tally2 u)
  (subst 0 "" (merge2 (word 1 u) (word 2 u))))


;; Guess the first digit of A/B using the first 3 digits of A and the
;; first 2 digits of B.  We assume A < B.  A & B are UF numbers.
;;
;;  E.g.  A = 0.123xxx     "01 011 0111 ..."
;;        B = 0.23xxx      "011 0111 ..."
;;   result = 0.4          "01111"
;;
;; In: A = dividend
;;     BHI = (tally2 B)
;;
(define `(guess-digit a bhi)
  (define `ahi (subst 0 nil (merge2 (merge2 (word 1 a) (word 2 a)) (word 3 a))))
  ;; min(floor(ahi/bhi),9)
  (define `q (subst bhi "x" 1 nil "x" 1 ahi))
  (concat 0 (subst T10 T9 q)))


(define `DIV-TRUNCATE  1)
(define `DIV-NEAREST   2)
(define `DIV-CEILING   3)
(define `DIV-REMAINDER 4)
(define `DIV-FLOOR     9)  ;; only for signed values


;; (div-post POST Q REM B) : Called with results of (uf-div a b num-digits).
;;
;;    Q/10 = the first N digits of A/B, prefixed with "0 "
;;    REM = remainder (always less than B)
;;    A = B*IQ + REM*10^(-N)
;;
(define (div-post mode q/10 rem b)
  ;; Round last digit up when REM>B/2 or REM==B/2 and last digit is odd
  (define `round-up
    (if (filter [DIV-NEAREST DIV-CEILING] mode)
        (findstring 1 (if (filter DIV-CEILING mode)
                          rem
                          (or (uf-cmp rem (uf*0.5 b))
                              (subst "11" nil (lastword q/10)))))))

  (if round-up
      (uf-carry (concat q/10 1))
      (if (filter DIV-REMAINDER mode)
          rem
          ;; TRUNCATE or round down
          q/10)))


;; In: ZA = [Z ...A]  (Z is first digit, A is in remaining digits)
;;     A, B are UF-encoded
;;     B >= 0.1
;;     A < B
;;     Z is 0 unless previous subtraction overflowed
;;
;; We choose a guess for the next digit, D, using the initial
;; digits of A and B:
;;   (1) Atop = A mod 0.001  [0.000 .. 0.999] Af = A - Atop
;;   (2) Btop = B mod 0.01   [0.10 .. 0.99];  Bf = B - Btop
;;   (3) D = floor(10*Atop / Btop)
;;   (4) 10*Atop - D*Btop <= Btop - 0.01                 [3,2]
;;   (5) 10*Atop - (D*B - D*Bf) <= Btop - 0.01           [4,2]
;;   (6) 10*Atop - D*B < Btop - 0.01 - D*Bf              [5]
;;   (7) 10*A - D*B < BTop - 0.01 - D*Bf + 10*Af         [6,1]
;;   (8) 10*A - D*B < B                                  [7,1,2]
;;
;; When D is 10 we substitute 9, since we know:
;;   (9) 10*A - 9*B < B                                  [A < B]
;;
(define (div-loop za b bhi num-digits mode digits)
  (define `a (rest za))
  (define `z (word 1 za))

  (cond
   ;; Z is non-zero only when the previous quotient digit was too large by
   ;; one; we adjust for this and continue...  [4% of the time]
   ((findstring "1" z)
    ;; Assert: Z == U9
    (div-loop (>>1 (uf-add a b))
              b bhi num-digits mode
              (subst "1x" "" (concat digits "x"))))

   ;; done?
   ((word num-digits digits)
    (div-post mode (>>1 digits) a b))

   ;; calculate next digit
   (else
    ;; 'foreach' is a fast 'let' when the value is exactly one word
    (foreach
        d (guess-digit a bhi)
        ;; initial digit of 10*A-D*B is 0 if no overflow, 9 otherwise
        (define `next-za
          (uf-sub a (uf*digit b d)))
        (div-loop next-za b bhi num-digits mode
                  (concat digits (if digits " ") d))))))


;; uf-div-long: handle arbitrarily long divisors
;;
(define `(uf-div-long a b n mode)
  (div-loop (>>1 a) b (tally2 b) n mode nil))


;;--------------------------------
;; uf-div1
;;--------------------------------

;; For uf-div1 and uf-div2 we compute X/Y via X*(1/Y).  When Y has have
;; prime factors other than 2 and 5, 1/Y is a repeating decimal.  For those,
;; we compute the repeating portion, multiply it by X, and then use
;; power-seq-1 to generate enough digits of precision.
;;
;; In order to have certainty about the initial digits, we need to assurance
;; that the exact value does not contain indefinitely long strings of 0s.
;; (Otherwise, an arbitrarily small error in our approximation from the
;; exact value could affect the digits of interest.)
;;
;; We know that in the repeating portion of the quotient (the portion after
;; which the dividend digits have been "consumed" in long division) there
;; are never more than len(Y)-1 consecutive 0s (unless all remaining zeros
;; are zero).  [Consider what partial remainder in long division could
;; generate as many zeros as the size of the divisor...]
;;
;; Therefore, we compute N + 1 + len(Y) digits of the power sequence, unless
;; X is longer than N.


(define `(is-odd? digit)
  (findstring 1 (subst "11" nil digit)))


;; Divide X by two, when last digit in X is even.
;;
(define `(uf/2 x)
  (subst "11" "2" "1 0" " 011111" "2" 1 x))


(define (uf*3 u)
  (uf-carry (subst 1 111 u)))


;; Add two UF numbers, returning a non-well-formed UF number (with digit
;; values up to 10).
;;
(define `(uf-add_10 x y)
  (uf-carry_19_10 (uf-add-x x y)))


(define `(uf*0.25 x)
  (subst "11" "2"
         "1 0" " 0112"
         "22" 1
         "2 0" " 011111"
         (concat x " 0 0")))


(define `(uf*1.5 x)
  (uf-carry (subst "11" "x"
                   "1 0" " 0xxxxx"
                   "x" "111" (concat x " " 0))))


;; Add X to Y, returning a non-canonical value (digits up to 10).
;;
(define `(uf-add_10 x y)
  (uf-carry_19_10 (uf-add-x x y)))


;; Compute A * (1 + X + X^2 + X^3 + ...) in special cases wherein X can be
;; described as 1>>N.  ZEROS = string of N "0" words.
;;
(define (power-seq-1 a zeros n0 n1)
  (if (word n1 (nth-rest n0 zeros))
      ;; All remaining terms approach Residue:  0 < Residue <= 1>>N
      ;; Exact  = A + Residue
      ;; Result = A + 1>>N
      ;;        = Exact + 1>>N - Residue
      (wordlist 1 (words zeros) (uf-add a (concat zeros 1)))
      (power-seq-1 (uf-add_10 a (concat zeros " " a))
                   (concat zeros " " zeros)
                   n0 n1)))


;; Round U to the nearest N digits, returning U/10.
;; N is in decimal.  N > 0.
;;
(define (uf-round n mode u)
  (define `q/10
    (>>1 (wordlist 1 n u)))

  (define `last-digit
    (word n u))

  (define `next-digit
    (word 2 (nth-rest n u)))

  (define `remaining-digits
    (nth-rest 3 (nth-rest n u)))
  (define `nearest-up?
    (findstring U6 (or (filter-out U5 (or next-digit 0))
                       (if (or (findstring 1 remaining-digits)
                               (is-odd? last-digit))
                           U6))))

  (define `round-up?
    (if (filter DIV-NEAREST mode)
        nearest-up?
        (if (filter DIV-CEILING mode)
            ;; Find any non-zero digit following the last digit.
            (findstring " 01" (nth-rest n u)))))

  (if round-up?
      (uf-carry (concat q/10 1))
      q/10))


(define `(rdiv a b n zs m)
  ;; XMIN = x0+x1-1 = x0 + len(B) + 2
  ;; This conservative definition for x0 is faster to compute than
  ;; the exact value, which would be given by:
  ;;    (if (word 4 (nth-rest n a))
  ;;        (words (nth-rest 3 a))
  ;;        n))
  (define `x0
    (if (word n a) (words a) n))

  (define `x1
    (words (concat b " 1 1 1")))

  (power-seq-1 m zs x0 x1))


;; uf-div1: single-digit divisors
;;
(define (uf-div1 a b n mode)
  (define `(uf*1.42857 x)
    (rest (uf-mul x [U1 U4 U2 U8 U5 U7])))

  (define `product
    (cond
     ((filter [U3 U6 U7 U9] b)
      ;; repeating decimals
      (rdiv a b n
            (if (filter U7 b)
                "0 0 0 0 0 0"
                "0")
            (if (filter [U3 U6] b)
                (if (filter U3 b)
                    (uf*3 a)         ;; 3
                    (uf*1.5 a))      ;; 6
                (if (filter U7 b)
                    (uf*1.42857 a)   ;; 7
                    a))))            ;; 9

     ((filter [U1 U2 U4 U8] b)
      (rest
       (if (findstring U4 b)
           (uf*0.25 (if (filter U4 b) a (uf*0.5 a)))    ;; 4 & 8
           (if (filter U2 b) (uf*0.5 a) a))))           ;; 2 & 1

     (else
      (uf*2 a))))  ;; 5

  (uf-round n mode product))


;;--------------------------------
;; uf-div2
;;--------------------------------


(define (recip-loop divisor r)
  (define `next-r
    (subst divisor "x" (subst "x" nil 1 T10 r)))

  (concat (subst 1 nil r)
          (if (filter "1 %x1" r)
              nil
              (concat " " (recip-loop divisor next-r)))))


;; Calculate a repeating decimal reciprocal for a 2-digit number that is
;; relatively prime to 10.  Result is a UF-number holding 0.1/X.
;; E.g.:  When X = 0.13, the result is 0.76923.
;;
;; note: this gets memoized
;;
(define (recip2 x)
  (.strip (subst "x" 1 " " " 0" (recip-loop (tally2 x) T10))))


(memoize (native-name recip2))


(define (recip-div2 x y n r)
  (rest (rdiv x y n (concat "0 " (patsubst "%" 0 r)) (uf-mul x r))))


(define (uf-div2 x y n mode)
  ;;(expect 2 (words y))
  (cond
   ;; Note: while uf-div requires Y>=0.1, uf-div2 does not.  This may happen
   ;; after divisors that are multiples of 2 or 5 are reduced, below.
   ((filter 0 (word 1 y))
    (uf-div1 (rest x) (rest y) n mode))

   ((filter [0 U2 U4 U5 U6 U8] (word 2 y))
    ;; multiple of 2 or 5
    (if (filter [0 U5] (word 2 y))
        (if (filter U5 (word 2 y))
            (uf-div2 (uf*0.2 x) (wordlist 1 2 (uf*0.2 y)) n mode)
            (uf-div1 x (word 1 y) n mode))
        (uf-div2 (uf*0.5 x) (uf/2 y) n mode)))

   (else
    (uf-round n mode (recip-div2 x y n (recip2 y))))))


;;--------------------------------
;; uf-div3
;;--------------------------------

;; We convert the divisor Y to a string of tally marks -- a string of 1's
;; whose length is the numeric value of Y<<len(Y) -- and we convert the
;; first len(Y) dividend digits to tally marks.  Each `(subst YT ...)`
;; precisely yields the next quotient digit and a remainder for computing
;; subsequent digits.
;;
;;   YT = divisor as a string of tally marks (Y<<len(Y))
;;   XT = initial digits of dividend as tally marks (0's indicating the
;;       previous quotient digit; 1's for partial remainder)
;;   X = remaining UF digits in dividend (0 <= X < 1)
;;   Q = quotient digits computed so far
;;   N = number of digits required on return
;;   MODE = DIV-NEAREST/CEILING/TRUNCATE
;;
;; sdiv-loop uses an unusual representation for its results.  Digits have no
;; "0" prefix; exactly one space delimits each digit; both 0's and 1's
;; designate 1's.  It is more efficient to correct this after the loop.

(define (sdiv-done yt xt x n)
  ;; Include next digit and then one more non-zero digit if
  ;; any value remains in X or XT
  (concat (subst "01" " " (concat 0 xt 1)) (findstring 1 x)))


(define (sdiv-loop yt xt x n ?zeros)
  (define `xstep
    ;; (subst 0 nil ...) removes 0's from X and 0's denoting Y in XT
    (subst 0 nil (concat (subst 1 T10 xt) (word 1 x))))

  (concat
   (concat (subst 1 nil xt)) " "
   (call (if (word n zeros)
             (native-name sdiv-done)
             (native-name sdiv-loop))
         yt (subst yt 0 xstep) (rest x) n (concat zeros " 0"))))


(define `(tally3 x)
  (subst 0 nil (merge2 (merge2 (word 1 x) (word 2 x))
                       (word 3 x))))


(define (uf-div3 x y n mode)
  (define `raw
    (sdiv-loop (tally3 y) (tally3 x) (nth-rest 4 x) n))

  (uf-round n mode (.strip (subst 0 1 " " " 0" raw))))


(declare (uf-div a b n mode))


;; Compute remainder using DIV-TRUNCATE and subtraction.
;;
(define (uf-remainder a b n)
  (define `(<<n u)
    (or (rest (nth-rest n u)) 0))

  (<<n (uf-sub a (uf-mul b (rest (uf-div a b n DIV-TRUNCATE))))))


;; Divide A by B
;;
;; In:  A & B are UF-encoded numbers (0 - 0.999...)
;;        A < B
;;        B >= 0.1
;;      N = number of digits of quotient to compute (in *DECIMAL*)
;;      MODE = [see out, below]
;;
;; Out: The returned value (always UF-encoded) depends on MODE:
;;
;;     DIV-TRUNCATE  =>  Qt/10
;;     DIV-NEAREST   =>  Qn/10   [round to nearest, ties to odd]
;;     DIV-CEILING   =>  Qc/10
;;     DIV-REMAINDER =>  REM
;;
;;   where Qt, Qn, Qc, and REM are defined as:
;;
;;     Qt = floor(A/B << N) >> N
;;     Qc = Qt + (REM>0 ? 1>>N : 0)
;;     Qn = Qt + (REM>0.5 or (REM=0.5 and is-odd(floor(Q<<N))) ? 1>>N : 0)
;;     REM = (A - Qt*B)<<N
;;
;;   ["X>>N" means X*10^-N and "X<<N" means X*10^N.]
;;
;;   Note that Qn or Qc may be equal to 1, which cannot be represented in a
;;   UF-number.  This is the reason why Q{x}/10 is returned instead of Q{x}.
;;
(define (uf-div a b n mode)
  ;;(assert (findstring 1 (word 1 b)))
  (cond
   ((uf-ends-in-0? b)
    (uf-div a (uf-trim-tz b) n mode))

   ((filter "0 -%" n)
    (div-post mode 0 a b))

   ((not (findstring 1 a))
    0)

   ((word 4 b)
    (uf-div-long a b n mode))

   ((filter DIV-REMAINDER mode)
    (uf-remainder a b n))

   (else
    ;; call uf-div1, uf-div2, or uf-div3
    (call (concat (native-name uf-div) (words b)) a b n mode))))


;; Divide UA by UB, rounding down to the nearest integer.  If UB==0, nil is
;; returned.
;;
;; UA, UB, and the result are (non-negative) U-strings.
;;
;; MODE = DIV-TRUNCATE => quotient (truncated)
;;        DIV-REMAINDER => remainder
;;
(define (u-fdiv ua ub mode)
  (cond
   ;; extraneous leading zeros
   ((filter "00%" (concat ua " " ub))
    (u-fdiv (patsubst "00%" "0%" ua) (patsubst "00%" "0%" ub) mode))

   ((and ua (findstring 1 ub))
    (define `_a (spread ua))  ;; has initial extraneous space
    (define `_b (spread ub))  ;; has initial extraneous space
    (define `uresult
      (foreach
          ;; num-digits = max(0, (len A) - (len B) + 1)
          num-digits (words (nth-rest (words _b) _a))
          (if (filter 0 num-digits)
              ;; B is longer than A
              (if (filter DIV-REMAINDER mode)
                  ua
                  0)
              (uf-div (concat "0" _a) (.strip _b) num-digits mode))))
    (u-norm-uns (smash uresult)))))
