(require "core")
(require "clocker")
(require "num")


;; Categories & weighting:
;;   m1: 1000 * add/sub/mul small natural numbers
;;   m2:  100 * small/med
;;   m3:   60 * negative add/sub/mul
;;   m4:   30 * decimals
;;   m5:   20 * scientific
;;   m6:   10 * divide
;;   m7:    1 * bignum

;; 14-jun-2018:  score = ~400  (MBP)


(define (clock-m1-add)
  (+ 1 1) (+ 3 2) (+ 5 3) (+ 7 4) (+ 9 5)
  (+ 34 1) (+ 45 2) (+ 3 56) (+ 4 67) (+ 57 89))

(define (clock-m1-mul)
  (* 2 5) (* 4 4) (* 6 3) (* 8 2) (* 10 1)
  (* 2 35) (* 4 45) (* 67 3) (* 8 72) (* 57 89))

(define (clock-m1-sub)
  (- 1 1) (- 3 2) (- 5 3) (- 27 4) (- 97 59))

(define (clock-m2)
  (+ 123 1) (+ 345 2) (+ 567 3) (+ 789 4) (+ 890 5)
  (+ 1234 545) (+ 2345 286) (+ 3456 7) (+ 5678 18) (+ 6789 1229)
  (* 123 1) (* 345 2) (* 567 3) (* 789 4) (* 890 5)
  (* 1234 545) (* 2345 286) (* 3456 7) (* 5678 18) (* 6789 119)
  (+ 1 123) (+ 2 345) (+ 3 567) (+ 4 789) (+ 5 890)
  (+ 5 1234) (+ 6 2345) (+ 7 3456) (+ 8 5678) (+ 9 6789)
  (* 1 123) (* 2 345) (* 6783 567) (* 4 789) (* 5 890)
  (* 5 1234) (* 2226 2345) (* 3457 3456) (* 8 5678) (* 9 6789))

(define (clock-m3)
  (+ 1 -1) (+ 3 -2) (+ 3 -5) (+ -8 1) (+ -9 -2)
  (+ 45 -1) (+ 4 -25) (+ 3 -555) (+ -587 1) (+ -349 -2)
  (- 45 -1) (- 4 -25) (- 321 -555) (- -587 1) (- -349 -2)
  (* 2 -35) (* 35 -3) (* -67 2) (* -45 -34) (* -34 -34))

(define (clock-m4)
  (+ 1.01 10) (+ 23.04 1) (+ 5.00001 1) (+ 6.001234 54) (+ 0.0065 23)
  (* 0.001 100) (* 1.034 7) (* 1.2 2.3) (* 2 0.71) (* 0.04531 0.7))

(define (clock-m5)
  (+ 1.0e2 1) (+ 23.04 7) (+ 5.00001 1e-2) (+ -0.001234e-23 -54) (+ 1.0e10 1.0e-10)
  (* 1.23e5 100) (* 1.034e-3 7e4) (* -1.2e7 2.33e7) (* 10000e-7 0.71) (* 0.0000e+12 0.7))

(define (clock-m6)
  (/ 2 1) (/ 100 10) (/ 85 3) (/ 34567 54321) (/ 123e7 7 28))

(define (clock-m7)
  (^ 17 303))


(define (main argv)
  (print "num module scoring")

  (and (first argv)
       (clk-show "m1-add [10]" (clock-m1-add))
       (clk-show "m1-mul [10]" (clock-m1-mul))
       (clk-show "m1-sub  [5]" (clock-m1-sub)))

  (define m1 (clk-show "m1 [25]" (begin (clock-m1-add)
                                        (clock-m1-mul)
                                        (clock-m1-sub))))
  (define m2 (clk-show "m2 [40]" (clock-m2)))
  (define m3 (clk-show "m3 [20]" (clock-m3)))
  (define m4 (clk-show "m4 [10]" (clock-m4)))
  (define m5 (clk-show "m5 [10]" (clock-m5)))
  (define m6 (clk-show "m6  [5]" (clock-m6)))
  (define m7 (clk-show "m6  [1]" (clock-m7)))

  (define all
    [ (* 40 m1)
      (* 2.5 m2)
      (* 3 m3)
      (* 3 m4)
      (* 2 m5)
      (* 2 m6)
      m7 ])

  (printf "Score: %s  (%s)" (sum all) all)

  nil)
