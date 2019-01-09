(require "core.scm")
(require "mcore.scm" &private)


(define `U1 01)
(define `U18 (concat U9 T9))
(define `U27 (concat U18 T9))

(define (repeat-words lst len)
  (if (subst 0 nil len)
      (if (word len lst)
          (wordlist 1 len lst)
          (repeat-words (concat lst " " lst) len))))

;; Decimal digits to U:  (U 123) --> "01 011 0111"
;;
(define (U d)
  (subst "A" U10 "I" U18 "R" U27 (d2u d)))

;; Decimal digits to UV:  (UV "123") --> "01 011 0111"
;;
(define (UV d)
  (strip (spread (U d))))

;; Digit sizes to UV:  (UD 1 2) --> "01 011"
;;
(define (UD ...values)
  (for n values
       (concat 0 (smash (repeat-words "1 1 1 1 1 1" n)))))

(define (FP n)
  (u2fp (U n)))

;; Construct an FP number from decimal without normalizing
;;
(define (DFP exp m)
  (make-fp (U exp)
           (or (findstring "-" m) "+")
           (UV (subst "-" nil m))))


;; Normalize FP-encoded number N.
;;
(define (fp-norm n)
  (if (findstring "0 0" (wordlist 3 4 n))
      (fp-norm (make-fp (u-1 (fp-exp n)) (fp-sign n) (rest (fp-uf n))))
      n))

(expect "0111 + 01 011" (fp-norm "01111 + 0 01 011"))
