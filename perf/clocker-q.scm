(require "num")
(require "core")
(require "clocker.scm" &private)

(set default-duration 50)

(define t1 (clk-time (* 1234 9876)))
(define t2 (clk-time (+ (* 1234 9876) (* 1234 9876))))

(assert (> t2 (* t1 2)))
