
;; Typical (?) arguments for integer use cases (number of items in a vector,
;; iteration, etc.).  Biased toward small & positive numbers.
;;
(define (binop-int-40 +)
  (+ 1 1) (+ 3 2) (+ 5 3) (+ 7 4) (+ 9 5)
  (+ 0 4) (+ 8 3) (+ 2 5) (+ -1 7) (+ 9 -6)
  (+ 1 23) (+ 2 36) (+ 53 3) (+ 4 47) (+ 9 99)
  (+ 23 7) (+ 54 6) (+ 18 -5) (+ -33 4) (+ -90 -3)
  (+ 34 30) (+ 45 52) (+ 34 56) (+ 24 67) (+ 57 89)
  (+ 123 1) (+ 305 2) (+ 567 3) (+ 789 4) (+ 890 5)
  (+ -512 1634) (+ 2226 -2345) (+ 1111 111) (+ -5678 -8) (+ 999 9999)
  (+ 9631 427) (+ 41 1958 ) (+ 98000671 31) (+ 12345678 31) (+ 87654321 12345678))

(define (unop-int-40 f)
  (f 1) (f 2) (f 3) (f 4) (f 5) (f 6) (f 7) (f 8) (f 9) (f 0)
  (f 1) (f 2) (f 3) (f 4) (f -5) (f -6) (f -7) (f -8) (f -9) (f -0)
  (f -12) (f -23) (f -34) (f -45) (f -56) (f 67) (f 78) (f 89) (f 90) (f 10)
  (f 123) (f 4567) (f 6789) (f 12345) (f 1234567)
  (f 12345678) (f 123456789) (f 1234567890) (f 12345678901))


(define (binop-intsmall-40 +)
  (+ 1 1) (+ 3 2) (+ 5 3) (+ 7 4) (+ 9 5)
  (+ 0 4) (+ 8 3) (+ 2 5) (+ -1 7) (+ 9 -6)
  (+ 1 23) (+ 2 36) (+ 53 3) (+ 4 47) (+ 9 99)
  (+ 23 7) (+ 54 6) (+ 18 -5) (+ -33 4) (+ -90 -3)
  (+ 34 30) (+ 45 52) (+ 34 56) (+ 24 67) (+ 57 89)
  (+ 123 1) (+ 305 2) (+ 567 3) (+ 789 4) (+ 890 5)
  (+ 34 30) (+ 45 52) (+ 34 56) (+ 24 67) (+ 57 89)
  (+ 123 1) (+ 305 2) (+ 567 3) (+ 789 4) (+ 890 5))

;; Typical (?) arguments for floating point use cases.
;;
(define (binop-fp-20 +)
  (+ 1.25 9) (+ 23.04 7) (+ 5.00001 1e-2) (+ -0.001234e23 -54) (+ 1.0e10 1.0e-10)
  (+ 1 1.0e2) (+ 7 23.04) (+ 1e-2 5.00001) (+ -54 -0.001234e23 ) (+ 1.0e-10 1.0e10)
  (+ 1.234567890123456e5 100) (+ 1.034e-3 7e4) (+ -1.2e7 2.33e7)
  (+ 10000e-7 0.71) (+ 0.0001e+12 0.7)
  (+ 223 0.01885927374984717)  (+ 0.01885927374984717 456)
  (+ 1.001885927374984717 3.568721917962554)  (+ 10000e-7 0.71)  (+ 0.0001e+12 0.7))


(define (unop-fp-10 f)
  (f 1.25) (f 23.04) (f 5.00001) (f -0.001234e23) (f -1.0e10) (f 1.1)
  (f 1234.1) (f 1.23467890123456e5) (f 1.0001885927374984717)
  (f 23467890123456e5))


(define (clock-int + - * <)
  (binop-intsmall-40 +)
  (binop-int-40 +)
  (binop-int-40 -)
  (binop-int-40 *)
  (binop-int-40 <))


(define (clock-fp + - * <)
  (binop-fp-20 +)
  (binop-fp-20 -)
  (binop-fp-20 *)
  (binop-fp-20 <))


;;----------------------------------------------------------------

(require "core")
(require "../math.scm")
(require "../perf/clocker.scm")


;;----------------------------------------------------------------
;; Micro-benchmarks

(require "../mcore.scm" &private)

(define (time-fp name nums)
  (define Us (foreach x nums (d2u x)))
  (clk-show (concat "u2fp " name) (foreach u Us (u2fp u)))
  (define FPs (for u Us (u2fp u)))
  (clk-show (concat "fp2u " name) (for x FPs (fp2u x)))
  (clk-show (concat "d2d " name) (for x nums (u2d (fp2u (u2fp (d2u x))))))
  nil)


;;----------------------------------------------------------------

(define (run-composite name f+ f- f* f/ f< f^)
  (printf "%s:" name)
  (define bi (clk-show "  bi [200]" (clock-int f+ f- f* f<)))
  (define bf (clk-show "  bf [ 80]" (clock-fp f+ f- f* f<)))
  (define df (clk-show "  df [ 20]" (binop-fp-20 f/)))
  (define ei (clk-show "  ei [ 20]" (binop-intsmall-40 f^)))

  (define all [ bi bf df ei])
  (printf "   overall: %s  (%s)\n" (sum all) all)
  nil)


(define d100 1.234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890e-9)

(define (main argv)
  (time-fp "lrg" [d100 d100 d100])
  (time-fp "0.x" [0.1234 0.01234 0.0001234])
  (time-fp "1.x" [1.1234 1.01234 1.0001234])
  (time-fp "int" [11234 101234 10001234])
  (run-composite "math    " + - * / < ^)
  nil)
