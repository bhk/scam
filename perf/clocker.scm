;;--------------------------------
;; Clocker: Benchmarking functions
;;--------------------------------

(require "core")
(require "num")
(require "io")

;; Default duration in milliseconds
(define default-duration
  &public
  250)

;; Build .out/timems if it isn't there...
(or (wildcard ".out/timems")
    (begin
      (shell! "mkdir -p .out/")
      (assert (not (shell! "cc -o .out/timems timems.c 2>&1")))))


(define (get-time-ms)
  &public
  (subst "." "" (shell ".out/timems")))


(define (floor n)
  (word 1 (subst "." " " n)))


(define (clk-time-foreach fn lst)
  (let ((t0 (get-time-ms))
        (_ (foreach n lst (fn)))
        (t1 (get-time-ms)))
    (- t1 t0)))


(define (clk-make-list reps)
  (patsubst "%" "." (range 1 reps)))


(define (clk-time-iter fn reps)
  (clk-time-foreach fn (clk-make-list reps)))


;; Iteration overhead for lambda call & foreach (~0.01ms)
(define t-nil nil)
;; Time consumed by get-time-ms
(define t-time
  (let ((a (get-time-ms))
        (b (get-time-ms)))
    (- b a)))


(define (clk-time-loop fn duration reps)
  (let ((t-reps (clk-time-iter fn reps)))
    (if (>= t-reps duration)
        [reps t-reps]
        ;; -1 for resolution inaccuracy; *1.5 for t-time duration
        (let& ((t-min (- t-reps (+ (* t-time 1.5) 1)))
               (max-mul (/ (+ duration 20) t-min))
               (mul (cond ((< t-reps 5) 20)
                          ((< t-min 10) 6)
                          ((< t-min 20) 3)
                          ;; too much of a reach => shoot for 10%
                          ((> max-mul 20) (* 0.1 max-mul))
                          ;; larger mulitples => less accuracy
                          ;; smaller t-min => less accuracy
                          (else (* max-mul
                                   (* (+ 1 (* max-mul 0.007))
                                      (+ 1 (/ 2 t-min))))))))
          (clk-time-loop fn duration (floor (+ 1 (* reps mul))))))))


(define (calibrate ?duration)
  &public
  (define `durn (or duration default-duration))
  (define `(.reps o) (word 1 o))
  (define `(.time o) (word 2 o))

  ;; get this warm in the cache
  (get-time-ms)

  ;; These initial 'reps' values shave a little time off
  (let ((cgt (clk-time-loop (lambda () (get-time-ms)) durn 2)))
    ;; there is one get-time-ms worth of overhead, hence reps+1
    (set t-time (/ (.time cgt) (+ 1 (.reps cgt)) 5))
    (let ((cnil (clk-time-loop (lambda () nil nil) durn 1000)))
      (set t-nil (/ (- (.time cnil) t-time) (.reps cnil) 5)))))


;; Compute calibrated per-iteration time in milliseconds
;;
(define (clk-result reps-time)
  (define `time (word 2 reps-time))
  (define `reps (word 1 reps-time))
  (define `t-per (- (/ (- time t-time) reps 4) t-nil))
  ;; limit to 4 significant digits
  (/ t-per 1 4))


;; Compute time consumed by calling FN.
;;
(define (clk-time-fn fn ?duration ?reps)
  &public
  (or t-nil
      (calibrate))
  (clk-result (clk-time-loop fn
                             (or duration default-duration)
                             (or reps 1))))


;; Compute time consumed by evaluating EXPR.
;;
(define `(clk-time expr ?duration ?reps)
  &public
  (clk-time-fn (lambda () expr nil) duration reps))


;; Compute and print time consumed by evaluating EXPR.
;;
(define `(clk-show name expr ?duration ?reps)
  &public
  (let ((time (clk-time-fn (lambda () expr nil) duration reps)))
    (printf "%s: %s" name time)
    time))
