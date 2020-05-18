;;----------------------------------------------------------------
;; math2 : Transcendental functions
;;----------------------------------------------------------------

(require "core.scm")
(require "math0.scm" &private)
(require "math1.scm" &private)


;;----------------------------------------------------------------
;; Utilities (to reconcile with mcore.scm & math.scm)
;;----------------------------------------------------------------


;; Remove the initial (words B) words from A
;;
(define `(wsub a b)
  (nth-rest (words (>>1 b)) a))


;; Return a list with UX words, all "0".  UX may have digits larger than 9.
;;
(define (u-zeros ux)
  (if (filter-out "-%" ux)
      (subst 1 " 0"
             (if (findstring "10" ux)
                 (tally (subst "0" " 0" ux))
                 (subst 0 nil ux)))))


;; Return first N digits of C, rounded to NEAREST unit in the last digit, or
;; NIL if there are not enough digits in C to determine the rounding at N.
;;
;; C = a UF value accurate to ± one unit in last digit.
;; COUNT = a list with N words
;;
(define (round-uf-const c count)
  ;; If C[N+1...] is empty or matches "49*" or "50*", we need more digits.
  ;; If C[N+1] ∈ {5,6,7,8,9}, return C[1..N] + 1
  ;; Otherwise, return C[1..N].

  (foreach (r (smash (wsub c count)))
      (define `not50*
        (subst ".011111" nil "0" nil (.. "." r)))
      (define `not49*
        (subst ".01111" nil U9 nil (.. "." r)))

      (and not49*
           not50*
           (if (filter "011111%" r)
               (uf-carry (.. (wordlist 1 (words count) c) 1))
               (wordlist 1 (words count) c)))))


;; Return first N digits of a constant, rounded to the nearest unit in the
;; last digit.  If necessary, call FNAME to obtain more digits.
;;
;; FNAME = the name of a function that, when called as (FNAME COUNT),
;;    returns a UF value accurate to within 10⁻ᴺ when rounded to N digits.
;; COUNT = a list of N words
;;
(define (get-uf-const fname count)
  (define `value-var (.. fname "/v"))
  (define `count-var (.. fname "/c"))
  (define `(compute-value nw)
    (rest (uf-round (words nw) DIV-NEAREST (native-call fname nw))))

  (or (round-uf-const (native-var value-var) count)
      (if (word 1 count)
          (begin
            (set-native count-var
                        (patsubst "%" 0 (._. "0 0 0 0 0"
                                             (join count (native-var count-var)))))
            (set-native value-var (compute-value (native-var count-var)))
            (get-uf-const fname count)))))


;; Multipy A x B using up to N digits of each, returning up to N digits.
;; May recalculate N.
;;
(define `(uf-mulp-raw a b n)
  (wordlist 1 n (uf-mul (or (wordlist 1 n a) 0)
                        (or (wordlist 1 n b) 0))))


;; Multipy A x B using up to N digits of each, returning up to N digits.
;;
(define `(uf-mulp a b n-expr)
  (foreach (n n-expr)
    (uf-mulp-raw a b n)))


;; Add or subtract.
;;
(define (uf-addsub a b is-sub?)
  (if b
      (uf-add a (if is-sub? (uf-complement b) b))
      a))


;; Divide fraction UF by integer U, returning at least N digits.
;;
(define `(uf-divu uf u n)
  ;; (uf-div A B) requires A < B.
  (define `q
    (uf-div (>>1 uf) (u2uv u) n DIV-NEAREST))
  (nth-rest 3 (.. (subst 1 nil 0 "0 " u) q)))


;; uf-atan-loop(x,a,b) = ∑ [n ≥ 0] x * aⁿ / (2n + b)
;;
;; arctan(x)  = uf-atan-loop(x, -x², 1)
;; arctanh(x) = uf-atan-loop(x, x², 1)
;;
;; X = UF number
;; ALZ = leading zeros in A
;; ANZ = digits following leading zeros in A
;; IS-SUB = when non-nil, subtract alternate terms
;; COUNT = list of words, one per digit of precision required
;;
(define (uf-atan-loop x alz anz is-sub b count)
  (define `b+2 (u-add-ones b 11))
  (define `x/b (uf-divu x b (words (>>1 count))))

  (define `x*anz
    (uf-mulp anz x (words (wsub count alz))))

  ;; The remaining terms...
  (define `remainder
    (if (word (words (>>1 alz)) count)
        ;; uf-add accepts extra spaces and/or nil
        (._. alz (uf-atan-loop x*anz alz anz is-sub b+2 (wsub count alz)))))

  (if count
      (if (filter 0 (word 1 x))
          (if (filter 0 (word 2 x))
              (._. "0 0" (uf-atan-loop (nth-rest 3 x) alz anz is-sub b
                                       (nth-rest 3 count)))
              (>>1 (uf-atan-loop (rest x) alz anz is-sub b (rest count))))
          (uf-addsub x/b remainder is-sub))))


(define (uf-atanh-3 xnz alz anz count)
  ;; We handle the first iteration of the loop here, mostly to improve
  ;; accuracy (no truncation in the initial X/1 term).
  (uf-add (._. alz (uf-atan-loop (uf-mulp xnz anz (words count))
                                 alz anz nil U3
                                 (wsub count alz)))
          xnz))


(define (uf-atanh-2 xlz xnz x² count)
  (if xnz
      (native-strip (._. xlz (uf-atanh-3 xnz (uf-get-lz x²) (uf-trim-lz x²)
                                   (wsub count xlz))))
      0))


;; Compute arctanh(X).
;;
;; X must be less than (e²-1)/(e²+1) ≈ ~0.761594 or else the result will
;; overflow the UF range [0,1).  Performance is best with much smaller
;; values of X; the closer to zero the better.
;;
;; X = UF value
;; DIGITS = decimal
;;
;; Return value is a UF value.
;;
;; arctanh(x) = ∑ [n ≥ 0] x²ⁿ⁺¹ / (2n+1)
;;            = ∑ [n ≥ 0] x * x²ⁿ / (2n+1)
;;
(define (uf-atanh x count)
  (uf-atanh-2 (uf-get-lz x) (uf-trim-lz x) (uf-mul x x) count))


;; Compute log(X) using:
;;
;;   log(x) = 2 * arctanh((x-1)/(x+1))
;;
;; A = X-1
;; B = (X+1)/10     Note: A < B
;; Result = log(X)
;;
(define (uf-log a b digit-zeros)
  ;;(assert (uf-lt? a b))
  (uf*2 (uf-atanh (uf-div (or a 0) b (words digit-zeros) DIV-NEAREST)
                  digit-zeros)))


;; Result = log(X)  [UF]
;; X-1 may be nil (=> 0).
;; Assert: 0 ≤ X-1 < 0.2  ==>  0 ≤ Result < ~0.18
;;
(define `(uf-log-pos x-1 digit-zeros)
  (uf-log x-1              ;; x-1        < 0.2
          (._. "011" x-1)  ;; (x+1)/10   > 0.2
          digit-zeros))


;; Result = -log(X)  [UF]
;; X may be nil (=> 0).
;; Assert: 0.82 < X < 1  ==>  0 ≤ Result < 0.22
;;
(define `(uf-log-neg x digit-zeros)
  (define `(uf-invert x)
    (uf-carry (uf-complement x)))

  (uf-log (uf-invert x)  ;; 1-x       < 0.18
          (._. "01" x)   ;; (1+x)/10  > 0.182
          digit-zeros))


;; UM is a multiplier used to obtain a value close to 1 to in both uf-log-fr
;; and uf-exp-med.
;;
(define `um-frac "0 011")               ;; M = 1.02
(define `um/10 (._. "01" um-frac))
(define `um-top/10 "01 0 01")           ;; Top = (1+M)/2


(define (log-M count)
  (uf-log-pos um-frac (._. "0 0" count)))


(define `(const-log-M count)
  (get-uf-const (native-name log-M) count))


;; A vector [U₁ U₂ ...] where Uⱼ = Mʲ/10 and Mʲ < 10.
;;
(declare m-powers)

(define (calc-m-powers x/100 v)
  (if (findstring 1 (word 1 x/100))
      (set m-powers v v)
      (calc-m-powers (rest (uf-mul x/100 um/10))
                     (.. v (if v " ")
                         (patsubst "00%" "0%" (smash x/100))))))

(define `cached-m-powers
  (or m-powers (calc-m-powers (>>1 um/10) nil)))


;; A vector [R₁ R₂ ...] where Rⱼ = Top/Mʲ
;;
(declare m-powers-r)

(define (calc-m-powers-r)
  (set m-powers-r
       (patsubst
        "00%" "0%"
        (foreach (u cached-m-powers)
          (smash (uf-div um-top/10
                         (wordlist 1 6 (spread u))
                         4 DIV-NEAREST)))))
  m-powers-r)


(define `cached-m-powers-r
  (or m-powers-r (calc-m-powers-r)))


;; Count number of values in VALUES that are greater than UX.
;;
;; VALUES = list of U-encoded fractions in *descending* order
;; UX = a UF-encoded fraction
;;
(define (psrch ux values valuesgt)
  (if (eq? values valuesgt)
      ;; done
      (words valuesgt)
      ;; pick midpoint
      (foreach (ndx (words (subst "0 0" 0 (patsubst "%" 0 (._. values
                                                               valuesgt)))))
        (if (uf-lt? ux (spread (word ndx values)))
            ;; midpoint > ux
            (psrch ux values (wordlist 1 ndx values))
            ;; midpoint <= ux
            (psrch ux (wordlist 2 ndx (.. "0 " values)) valuesgt)))))


(define (uf-log-fr2 a p digit-zeros)
  (define `a-mod-1 (rest a))
  (define `p-len (words p))
  (define `m-digit-words (._. digit-zeros 0 p))

  (define `p*logm/10
    (nth-rest p-len (uf-mul (const-log-M m-digit-words) p)))

  (define `count
    (.. digit-zeros " 0 " (word 75 digit-zeros)))

  (if (filter "01%" (word 1 a))  ;; a>=1
      (uf-sub p*logm/10 (>>1 (uf-log-pos a-mod-1 count)))
      (uf-add p*logm/10 (>>1 (uf-log-neg a-mod-1 count)))))


;; Calculate logarithm for 0.1 ≤ X < 1.
;;
;; X is UF-encoded.
;; DIGITS-ZEROS is a list of "0" words, one per digit of accuracy.
;; Result = -log(X)/10 as UF number. 0 ≤ Y < ~0.23.
;;
;; We first find `a` closest to 1 where:
;;   a = x*Mᵖ
;;   x = a / Mᵖ
;;   log(x) = log(a) - log(M)*p
;;
;; We then compute log(a) using uf-log-pos/neg.  log(M) is cached.
;;
(define (uf-log-fr x digit-zeros)
  ;; DP = P where UX*Mᵖ is closest to 1.
  (foreach (dp (psrch x cached-m-powers-r nil))
    (define `Mᵖ
      (u2uv (word dp cached-m-powers)))

    (define `a
      (if (filter 0 dp)
          (>>1 x)
          (uf-mulp Mᵖ x (words (._. "0 0" digit-zeros)))))

    (uf-log-fr2 a (u2uv (d2u dp)) digit-zeros)))


(define (log-10 count)
  (uf-log-fr U1 (._. "0 0" count)))

(define `(const-log-10 count)
  (get-uf-const (native-name log-10) count))


;; When non-nil, override POD for the final rounding step in log, exp, pow,
;; sin, cos, atan, atan2.  [Used in testing.]
;;
(declare result-pod)


;; See log-size.
;; EXP and UF are the exponent and the fractional part of FX.
;; Result = U value with a single digit (may need carry propagation).
;;
(define (log-size-x exp uf)
  (cond
   ((filter "-01111 -0111 -011 -01 011 0111 01111 011111" exp)
    ;; 0.00001 ≤ x < 0.1  or  10 ≤ x < 100000
    "01")

   ((filter 01 exp)
    ;; 1 ≤ X < 10
    (if (findstring "11" (wordlist 1 2 uf))  ;; uf ≥ 1.2?
        ;; x >= 1.2
        "0"
        ;; X < 1.2 : return number of zeros after "1.", after replacing
        ;; "0010" with "0001" insize UF.
        (begin
          (define `lz
            (word 1 (subst " " nil "001000" "000100" "01" " " (.. uf "000"))))
          (.. "-0" (subst 0 1 lz)))))

   ((filter 0 exp)
    ;; 0.1 ≤ X < 1
    (if (findstring "111" (word 1 uf))  ;; uf ≥ 0.3?
        ;; 0.3 ≤ X < 1
        (if (findstring T9 (word 1 uf))
            ;; Count number of 9's after "0.".
            (.. "-0" (word 1 (subst U9 1  " 1" 1 uf)))
            ;; 0.3 ≤ x < 0.9
            "0")
        ;; X < 0.3  [X < 0.368 would be closer, but requires more code]
        "01"))

   (else
    ;; exp >= 6 or exp <= -5
    ;; result = 1 + digitsin(-exp/5) = digitsin(exp*2)
    (.. 0 (subst 1 nil 0 1 (patsubst "011111%" "00%" (subst "-" nil exp)))))))


;; Estimate the exponent of log(FX), or 1+floor(log₁₀(logₑ(FX))).  The
;; estimate is either the exact value or one less.
;;
;; As FX approaches 0 or ∞, the exponent approaches ∞.
;; As FX approaches 1, the exponent approaches -∞.
;; If FX==1, results are undefined.
;;
;; FX is an FP value.
;; Result = U-value.
;;
(define `(log-size fx)
  (u-carry-all (log-size-x (fp.xpo fx) (fp.uf fx))))


;; COUNT = the number of digits to the right of the decimal point
;;
(define (fp-log2 exp uf count)
  ;; Calculate using:  log(z * 10ᵖ) = log(z) + p*log(10)

  ;; 0 .. ~0.23
  (define `log-frac*0.1
    (uf-log-fr uf count))

  ;; log(10)*0.1 ≈ 0.23.  This will be multiplied by the exponent, so add as
  ;; many digits as in the exponent.
  (define `log-10*0.1
    (const-log-10 (._. count "0 0" (spread (abs exp)))))

  (if (uf-lt? um/10 uf)
      ;; 0.102 < X < 1
      (fp-add (if (findstring 1 exp)
                  (fp-mul (.. "01 + " log-10*0.1)
                          (u2fp exp))
                  FP0)
              (.. "01 - " (log-frac*0.1)))
      ;; 0.1 <= X <= 0.102
      (fp-add (fp-mul (.. "01 + " log-10*0.1)
                      (u2fp (u-sub exp "01")))
              (.. "0 + " (uf-log-pos (rest uf) (>>1 count))))))


;; Count digits to the right of the decimal point, given a starting place
;; and a number of digits.
;;
;; COUNT = list of N words, where N is the number of digits.
;; PLACE = a single-digit U value describing the starting place: 10^PLACE
;; Result = a list of NR words, where NR = no. of digits right of the decimal.
;;
(define (adjust-digit-count count place)
  (if (filter "-%" place)
      ;; PLACE is negative: add -SIZE words to COUNT
      (.. count (subst "-0" nil 1 " 1" place))
      ;; PLACE is positive: remove that many words from COUNT
      (nth-rest (words (subst 1 " 1" place)) count)))


;; Compute log(FX).
;;
(define (fp-log fx pod)
  (define `count
    (if (pod-is-place? pod)
        ;; POD = place: U value (negative => left of decimal; non-negative => right)
        (u-zeros pod)
        ;; POD = decimal number of digits.
        (adjust-digit-count (nzeros pod) (log-size-x (fp.xpo fx) (fp.uf fx)))))

  (and (fp>0? fx)
       pod
       (fp-round (fp-log2 (fp.xpo fx) (fp.uf fx) count)
                 (or result-pod pod) DIV-NEAREST)))


;; Compute the logarithm of FX in base FB.
;;
(define (fp-log-x-b fx fb pod)
  ;; DIV-COUNT is a count (word list) representing the number of significant
  ;; digits needed for the arguments to fp-div in order to obtain a result
  ;; with precision as given by POD.  A-PLACE and B-PLACE given the place of
  ;; the most significant digit in A and B.
  (define `(div-count a-place b-place)
    (if (pod-is-place? pod)
        ;; POD = place: U value (negative => left of decimal; non-negative => right)
        (adjust-digit-count
         (adjust-digit-count (._. "0 0" (u-zeros pod)) (0- a-place))
         b-place)
        ;; POD = decimal number of digits
        (._. "0 " (nzeros pod))))

  (define `result
    (foreach (x-size (log-size-x (fp.xpo fx) (fp.uf fx)))
      (foreach (b-size (log-size-x (fp.xpo fb) (fp.uf fb)))
        (let ((count-pre (div-count x-size b-size))
              (fx fx)
              (fb fb)
              (pod pod))

          (fp-div (fp-log2 (fp.xpo fx) (fp.uf fx)
                           (adjust-digit-count count-pre x-size))
                  (fp-log2 (fp.xpo fb) (fp.uf fb)
                           (adjust-digit-count count-pre b-size))
                  (or result-pod pod)
                  1)))))

  (and pod
       (fp>0? fx)
       (fp>0? fb)
       result))


;;--------------------------------
;; Exponentiation
;;--------------------------------


;; eˣ = ∑ [k ≥ 0] xᵏ / k!
;;    = ∑ [k ≥ 0] t(k),  t(0) = 1,  t(k+1) = t(k) * x / (k+1)
;;
;; COUNT = list of N words, where N is number of digits of accuracy
;;
(define (uf-exp-loop t x k+1 count)
  (define `(>>2 u) (._. "0 0" u))
  (define `k+2 (u-add-ones k+1 1))

  ;; The remaining terms...
  (define `remainder
    (foreach (n (words count))
        (if (filter-out 0 n)
            ;; uf-add accepts extra spaces and/or nil
            (uf-exp-loop (uf-divu (uf-mulp-raw x t n) k+1 n)
                         x k+2 count))))

  (if count
      (if (findstring 1 (word 1 t))
          (uf-add remainder (>>1 t))
          (if (filter 0 (word 2 t))
              (>>2 (uf-exp-loop (nth-rest 3 t) x k+1 (nth-rest 3 count)))
              (>>1 (uf-exp-loop (rest t) x k+1 (rest count)))))))


;; Calculate eᵘ.  Fast only when U is close to 0.
;;
;; U = UF-number (0 - 0.999....)
;; COUNT = list of N words, where N is the number of digits requested
;; Result = eᵘ/10    (0 - 0.2718281828...)
;;
(define (uf-exp-small u count)
  (uf-add U1 (uf-exp-loop u u U2 count)))


;; LM = log(M)
(define (uf-exp-med-2 u count lm)
  (define `(uv-to-word uv)
    (patsubst "0:%" "%" (patsubst "0:0:%" "%" (subst " " ":" uv))))

  ;; assume LM < 0.1    ==>  EXPM <= 1.1
  ;; assume U/100 < LM  ==>  EXPM > ~1.024
  (foreach (wp (uv-to-word (uf-div (>>1 u) (rest lm) 3 DIV-TRUNCATE)))
      (define `p (subst ":" " " wp))

      (if (findstring 1 wp)
          (foreach (wm (word (u2d (subst ":" nil wp)) cached-m-powers))
            (define `mᵖ/10
              (wordlist 1 (words (>>1 count)) (spread wm)))
            (define `a
              (uf-sub u (nth-rest (words p) (uf-mul lm p))))
            (define `a-count
              (.. count (if (filter "0111%" wm) " 0")))

            (uf-mul (uf-exp-small (rest a) a-count)
                    mᵖ/10))

          ;; U*10 < LM
          (>>1 (uf-exp-small (rest u) count)))))


;; Calculate eˣ where 0 ≤ X ≤ ~2.30585.
;;
;; UF = UF value of X/10  (0 ... ~0.230585)
;; COUNT = a list of N words, where N = number of digits
;; Result = e¹⁰ᵘ/100 ∈ (0.01,0.1)  => e¹⁰ᵘ ∈ (1,~10)
;;
;; We find the smallest non-negative `a` such that:
;;   x = log(M)*p + a
;;   eˣ = Mᵖ * eᵃ
;;
;; We compute eᵃ using uf-exp-small.  log(M) and powers of M are cached.
;;
(define `(uf-exp-med uf count)
  (uf-exp-med-2 uf count (const-log-M (._. "0 0" count))))


;;----------------------------------------------------------------
;; exp: Compute eˣ for any x.
;;
;; We first find the smallest non-negative `a` such that:
;;    x = a + i*log(10) ;  i ∈ Integers
;; We then compute:
;;    eˣ = eᵃ * 10ⁱ
;; ... using uf-exp-med for eᵃ.
;;
;; Notation for errors (calculated values vs. exact values):
;;
;;   Eₐ[expr] = maximum potential absolute error in expression `expr`
;;   Eᵣ[expr] = maximum potential relative error in expression `expr`
;;
;; `(exp x)` apmlifies the relative error by x:
;;     exp(x*(1+err)) = exp(x) * exp(x*err)
;;                    ≈ exp(x) * (1 + x*err)   [for small err]
;; Alternatively:
;;     Eᵣ[f(x)] ≈ Eᵣ[x] * f'(x) * x/f(x)
;;     Eᵣ[eˣ] ≈ x*Eᵣ[x]
;;
;; Values KL, Km, and N that govern the precision of intermediate values,
;; and must be chosen to yield adequate precision of the result.
;;
;; (exp x PREC) -> out where:
;;    L10 = (log-10 KL)        Eₐ[L10] = 5>>KL
;;    i = x // L10
;;    a = x - i*L10            Eₐ[a] = abs(i) * Eₐ[L10]
;;    m = (exp-med a Km)         Eᵣ[m] = 1>>Km + Eₐ[a]*a/eᵃ
;;    out = m * 10ⁱ            Eᵣ[out] = Eᵣ[m]
;;
;; When PREC is given as a place, we convert it to a number of digits:
;;   Eₐ[out] < 1>>P
;;   Eᵣ[out] < 1>>(P + log₁₀(out))
;;          < 1>>N    where N = P + max(0, x/2.3)
;;
;; Given a number of digits N:
;;   0.5>>N > Eᵣ[out]       [note: rounding will add another 0.5>>N]
;;          > Eᵣ[m]
;;          > 1>>Km + a/eᵃ*Eₐ[a]
;;          > 1>>Km + log(10)/10 * Eₐ[a]        max[a/eᵃ] for 0 ≤ a ≤ log(10)
;;          > 1>>Km + 0.5*abs(x)>>KL
;;          > 1>>Km + 0.5>>(KL - log₁₀(abs(x)))
;;   e.g. Km = N+1,  KL = N+2+(exponent x)
;;
;;----------------------------------------------------------------


;; Convert FX to a signed U-encoded integer, truncating to the nearest
;; integer.  (Get digits left of the decimal point.)
;;
(define (fp2su fx)
  (if (n>0? (fp.xpo fx))
      (.. (findstring "-" (fp.sign fx))
          (smash (uf-fix (fp.uf fx) (u2d (fp.xpo fx)))))
      0))


;; Convert FP to UF-encoded value U, where FP mod 10ᵖ = U * 10ᵖ.
;; (Get digits right of the decimal point.)
;;
;; Note: This does not handle *very* large exponents.  For example, (fp2uf
;;       (FP 1e99) (U 1)) should return "0", but will crash instead.
;;
(define (fp2uf fx e)
  (or
   (foreach (pos (if (filter 0 e)
                     (fp.xpo fx)
                     (u-sub (fp.xpo fx) e)))

     (if (n>0? pos)
         ;; Avoid using POS as word index if it's too large
         ;; (word 1 LIST) == (word 4294967297 LIST) in some `make` builds
         (if (word 9 (spread pos))
             nil
             (nth-rest (u2d pos) (nth-rest 4 fx)))
         (native-strip (._. (if (findstring 1 pos)
                                (u-zeros (abs pos)))
                            (fp.uf fx)))))
   0))


;; FI = floor(X/log(10))
;;
(define (exp-4 fx count fL10 fi)
  (define `exp
    (u-add-ones (fp2su fi) 11))
  (define `ua
    (fp2uf (fp-sub fx (fp-mul fi fL10)) "01"))

  ;; The largest result we expect from uf-exp-med is ~10.  The exact value
  ;; should always be less than 10, but rounding of the result and
  ;; imprecision in A could yield 10 or greater.
  (make-fp exp "+" (uf-exp-med ua (>>1 count))))


(define (exp-3 fx count fL10)
  ;; Compute i = x // log(10))
  (exp-4 fx count fL10 (fp-div fx fL10 "0" nil)))


;; COUNT = list of N words
;;
(define (exp-2 fx count)
  ;; number of digits needed for log(10)
  (define `KL
    (._. count "0 0" (u-zeros (fp.xpo fx))))

  (if count
      (exp-3 fx count (make-fp "01" "+" (const-log-10 KL)))
      FP0))


;; POD = place (0N, -0N) or digits (U)
;;
(define (fp-exp fx pod)
  ;; Approximation FIA of I=floor(X/10); FIA >= I
  (define `fia
    (fp-div fx "01 + 011 0111" 0 nil))

  (define `count
    (if (pod-is-place? pod)
        ;; U place (negated)
        (>>1 (u-zeros (u-add pod (fp2su fia))))
        ;; decimal number of digits
        (nzeros pod)))

  (and fx
       pod
       (fp-round (exp-2 fx count) (or result-pod pod) DIV-NEAREST)))


;;----------------------------------------------------------------
;; For pow, we need to compute the precision for (log x PREC) that will
;; yield the appropriate precision for (pow x y).
;;
;;  (pow x y) -> out where:
;;     z = log(x)
;;     out = exp(y*z)
;;
;; Let NZ be the number of significant digits we request from `log`.
;;    Eᵣ[z] = 1>>NZ,
;;    Eᵣ[out] = y*z * Eᵣ[y*z]
;;            = y*z * 1>>NZ
;;            = y * log(x) * 1>>NZ
;;            = 1>>(NZ - log₁₀(y * log(x)))
;;
;; When a number of significant digits N is specified by PREC:
;;   Eᵣ[out] <= 1>>N
;;   NZ >= N + log₁₀(y * log(x)))
;;      >= N + log₁₀(y) + log₁₀(logₑ(x))
;;      >= N + (exponent y) + (log-size x)
;;
;; When a place P is specified by PREC:
;;   N = P + (exponent out)
;;     = P + 1 + floor(log₁₀(out))
;;     = P + 1 + floor(log₁₀(exp(y*z)))
;;     = P + 1 + floor(y*log(x)/2.3)
;;     = P + 1 + floor(y*log₁₀(x))
;;----------------------------------------------------------------

;; Compute xʸ, defined as exp(y * log(x)).
;;
(define (fp-pow fx fy pod)
  ;; Note: this assumes normalized numbers (as returned by u2fp)
  (define `(fp!=0? f)
    (findstring 1 (word 3 f)))

  ;; Convert PLACE to number of digits, if necessary.
  (define `u-digits
    (if (pod-is-place? pod)
        ;; POD = place as U-value (positive = right of decimal)
        (u-add (u+1 pod) (fp2su (fp-mul fy (u2fp (u+1 (fp.xpo fx))))))
        ;; POD = number of digits (decimal)
        (d2u pod)))

  (define `(compute udigits)
    ;; ZPOD = precision needed for (log FX)
    (define `zpod
      (u2d (u-add udigits
                  (u-add (log-size fx) (fp.xpo fy)))))

    (define `fz
      (let-global ((result-pod nil))
        (fp-log fx zpod)))

    (if (filter "-% 0" udigits)
        FP0
        (fp-exp (fp-mul fy fz) pod)))

  (define `x-and-y-non-zero?
    (findstring 101 (.. (word 3 fx) (word 3 fy))))

  (and fx
       fy
       pod
       (if x-and-y-non-zero?
           (foreach (ud u-digits)
             (compute ud))
           (if (fp!=0? fx)
               FP1
               (if (fp!=0? fy)
                   FP0)))))


;; Compute arctan(X).
;;
;; X = UF value
;; COUNT = list of N words, where N = number of digits of precision
;; Result is a UF value.
;;
;; arctan(x) = ∑ [n ≥ 0] (-1)ⁿ x²ⁿ⁺¹ / (2n+1)
;;           = ∑ [n ≥ 0] (-1)ⁿ x * x²ⁿ / (2n+1)
;;
(define (uf-atan x count)
  (let ((xlz (uf-get-lz x))
        (xnz (uf-trim-lz x))
        (x² (uf-mul x x))
        (count count))
    (if xnz
        (native-strip (._. xlz (uf-atan-loop xnz (uf-get-lz x²) (uf-trim-lz x²)
                                             "-" U1 (wsub count xlz))))
        0)))


;; Return π/10 in UF format.
;;
(define (uf-pi count)
  ;; 1/239 = 0.(0041841)
  (define `uf1/239 (extend-fn nil (words count)
                              " 0 0 01111 01 011111111 01111 01"))
  (define `4*acot5 (uf*4 (uf-atan U2 count)))
  (define `acot239 (uf-atan uf1/239 count))

  ;; π/4 = 4*atan(1/5) - atan(1/239)
  (uf*4 (>>1 (uf-sub 4*acot5 acot239))))


;; Return N digits of π, caching results.
;;
(define `(const-pi count)
  (get-uf-const (native-name uf-pi) count))


;;================================================================
;; sin & cos
;;================================================================

;; Result = ∑ [n≥0] (-m)ⁿ * x / (2n+k+1)!
;;        = x - x*m/k/(k+1) + x*m²/k/(k+1)/(k+2)/(k+3) - ...
;;
(define (uf-sin-loop k x m count)
  (define `x*m/k/k+1
    (foreach (n (words (._. "0" count)))
      (uf-divu (uf-divu (uf-mul x m) k n) (u-add-ones k 1) n)))

  (if (findstring 1 (word 1 x))
      (uf-sub x (uf-sin-loop (u-add-ones k 11) x*m/k/k+1 m count))
      (if count
          (>>1 (uf-sin-loop k (rest x) m (rest count))))))


;; Result = sin(U)/10
;;
(define `(uf-sin-small u count)
  (>>1 (uf-sin-loop U2 u (uf-mulp u u (words count)) count)))


;; Result = cos(U)/10
;;
(define `(uf-cos-small u count)
  (let ((u² (uf-mulp u u (words count))))
    (uf-sub U1 (>>1 (uf-sin-loop U3 (uf*0.5 u²) u² count)))))


;; Find equivalent [FX IS-SIN IS-NEG] where 0 ≤ FX ≤ 0.8.
;;
;; IS-SIN : if true, compute `sin`, else compute `cos`
;; IS-NEG : if result is negative
;; COUNT = word list of length N where the required accuracy is 0.5>>N
;; KNAME = name of continuation function to call:
;;     (K FX IS-SIN IS-NEG COUNT ARG1 ARG2 ARG3)
;;
(define (xsin-reduce fx is-sin is-neg count kname arg1 arg2 arg3)
  (define `(recur a b c)
    (xsin-reduce a b c count kname arg1 arg2 arg3))

  (define `F0.8 "0 + 011111111")
  (define `F2.36 "01 + 011 0111 0111111")
  (define `F5.5 "01 + 011111 011111")
  (define `π (make-fp "01" "+" (const-pi (>>1 count))))
  (define `π/2 (make-fp "01" "+" (uf*0.5 (const-pi (>>1 count)))))
  (define `π*2
    (make-fp "01" "+" (uf*2 (const-pi
                             (._. (u-zeros (fp.xpo fx)) 0 count)))))

  (cond
   ;; x < 0 :  sin -x = -sin(x) ;  cos x = cos(x)
   ((findstring " -" fx)
    (recur (fp-abs fx) is-sin (xor is-sin is-neg)))

   ;; x ≤ ~π/4 : use series
   ((fp-lt? fx F0.8)
    (native-call kname fx is-sin is-neg count arg1 arg2 arg3))

   ;; π/4 < x ≤ π*3/4:  sin/cos x = cos/sin (π/2 - x)
   ((fp-lt? fx F2.36)
    (recur (fp-sub π/2 fx) (not is-sin) is-neg))

   ;; π*3/4 < x ≤ π*7/4: sin/cos x = -sin/cos(x - π)
   ((fp-lt? fx F5.5)
    (recur (fp-sub fx π) is-sin (not is-neg)))

   ;; x > π*7/4 : sin/cos x = sin/cos(x - n*2π)
   (else
    (recur (fp-norm (fp-sub (fp-mod (fp-add F0.8 fx) π*2) F0.8))
           is-sin
           is-neg))))


;; Compute sin(x) of cos(x) where x < ~π/4.
;;
;; COUNT = list of N words, where N is the number of digits to the right of
;;      the decimal required in the result (*and* with which FX has been
;;      reduced).
;; N = if non-nil, number of significant digits required (decimal)
;;
(define (xsin-small fx is-sin is-neg count fx-orig is-sin-orig n)
  ;; sin(FX) will require more significant digits if FX is so close to zero
  ;; that COUNT < N + NLZ + 1, where NLZ = leading zeros in FX.
  (define `get-more-digits
    (define `count-extra
      ;; (words COUNT) - N
      (words (nth-rest 2 (nth-rest n count))))

    (define `xlz
      (if (fp!=0? fx)
          count                    ;; ensure progress
          (uf-get-lz (fp2uf fx 0))))

    (and n
         is-sin
         (not (findstring 1 (wordlist 1 count-extra (fp2uf fx 0))))
         (xsin-reduce fx-orig is-sin-orig nil
                      (._. xlz 0 (wordlist 1 n count))
                      (native-name xsin-small) fx-orig is-sin-orig n)))

  (or get-more-digits
      (make-fp "01"
               (if is-neg "-" "+")
               (if is-sin
                   (uf-sin-small (fp2uf fx 0) count)
                   (uf-cos-small (fp2uf fx 0) count)))))


;; Conver POD to count of digits after the decimal place, assuming
;; the result is >= 0.1.
;;
(define `(pod-to-count pod)
  (>>1 (if (pod-is-place? pod)
           (u-zeros pod)
           (nzeros pod))))


(define `(pod-is-digits? pod)
  (filter-out "0% -0%" pod))


;; Compute sin(FX) or cos(FX)
;;
(define (fp-xsin is-sin fx pod)
  (define `result
    (if (fp!=0? fx)
        ;; Reduce FX to the range 0...~π/4, then call xsin-small
        (xsin-reduce fx is-sin nil (pod-to-count pod) (native-name xsin-small)
                     fx is-sin (pod-is-digits? pod))
        ;; sin(0) and cos(0) have exact answers.
        (if is-sin
            FP0
            FP1)))

  (and fx
       pod
       (fp-round result (or result-pod pod) DIV-NEAREST)))


;; Compute sin(X) or cos(X)
;;
(define (xsin is-sin x prec)
  (fp2d (fp-xsin is-sin (d2fp x) (prec-to-pod prec))))


;;----------------
;; arctangent
;;----------------


;; Apply transformations to theta to reverse the transformations
;; applied to X & Y.
;;
(define (fp-atan-unflip theta flips count count-lz)
  (define `small-count
    (._. count (if count-lz (uf-get-lz (fp2uf theta 0)))))

  (define `is-sub (filter "| /" flips))
  (define `is-add-π (filter "|" flips))

  (if flips
      (fp-add (if is-sub
                  (subst " +" " -" theta)
                  theta)
              (make-fp "01" "+"
                       (if is-add-π
                           (const-pi small-count)
                           (uf*0.5 (const-pi small-count)))))
      theta))


(define `(pod+1 pod)
  (if (pod-is-place? pod)
      (u+1 pod)
      (1+ pod)))


;; Compute atan(UFM)
;;
(define (fp-atan-tiny ufm count count-lz)
  (define `small-count
    (._. count (if count-lz (uf-get-lz ufm))))

  (make-fp "0" "+" (uf-atan ufm small-count)))


;; Compute atan(FM) for FM <= ~1
;;
;; FLIPS describes lines around which we have reflected coordinates to
;;    arrive at FM: "/" => X=Y diagonal, "|" => Y axis.
;;
;; Note:  atan(X) = atan(1) + atan(A)   where A = (X-1)/(X+1)
;;        atan(X) = π/4 + atan(A)   where A = (X-1)/(X+1)
;;
;;
(define (fp-atan-small fm flips pod count)
  (define `FP0.42 "0 + 01111 011")

  (define `theta
    (if (fp-lt? fm FP0.42)
        (fp-atan-tiny (fp2uf fm 0) count (pod-is-digits? pod))
        ;; atan(x) = π/4 - atan(a) where a = (1-x)/(x+1))
        ;; Here we know the result will not have leading zeros, so
        ;; we can ignore the pod-is-digits case.
        (let& ((π/4 (make-fp "01" "+" (uf*0.25 (const-pi count))))
               (a (fp-div (fp-sub FP1 fm)
                          (fp-add fm FP1)
                          (pod+1 pod)
                          DIV-NEAREST)))
          (fp-sub π/4 (fp-atan-tiny (fp2uf a 0) count nil)))))

  (fp-atan-unflip theta flips count (pod-is-digits? pod)))


(define (fp-atan2 fy fx pod ?flip)
;;  (printf "y=%s x=%s %s" (fp2d fy) (fp2d fx) flip)

  (cond
   ((findstring " -" fy)
    (fp-negate (fp-atan2 (fp-abs fy) fx pod)))

   ((fp<0? fx)
    (fp-atan2 fy (fp-abs fx) pod "|"))

   ((fp-lt? fx fy)
    (fp-atan2 fx fy pod (.. flip "/")))

   ((and (fp!=0? fx) fx fy)
    (fp-atan-small (fp-div fy fx (pod+1 pod) DIV-NEAREST) flip pod
                   (pod-to-count pod)))))
