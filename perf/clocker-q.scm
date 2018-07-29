(require "num")
(require "core")
(require "io")
(require "clocker.scm" &private)

(expect 1 (not (not (file-exists? timems-exe))))
(expect 1 (words (get-time-ms)))

(set default-duration 50)

(define t1 (clk-time (* 1234 9876)))
(define t2 (clk-time (+ (* 1234 9876) (* 1234 9876))))

(assert (> t2 (* t1 2)))
