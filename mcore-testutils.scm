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



;; Convert an FP number to decimal.
;;
(define (fp2d x)
  (u2d (fp2u x)))
