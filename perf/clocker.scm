;;--------------------------------
;; Clocker: Benchmarking functions
;;--------------------------------

(require "core")
(require "math")
(require "io")


;; Default duration in milliseconds
(define default-duration
  &public
  200)


(define `timems.c
  "#include <stdio.h>
#include <sys/time.h>

int main(int argc, char **argv)
{
   struct timeval tv;
   gettimeofday(&tv, (struct timezone *) NULL);
   printf(\"%lu.%03lu\\n\", tv.tv_sec, tv.tv_usec / 1000LU);
}
")


(define *timems* nil)


;; Build timems if it isn't there...
(define (build-timems)
  (define timems-loc
    (concat (get-tmp-dir) "timems"))

  (define `echo-cmd
    (concat "printf '%b' " (quote-sh-arg (subst "\\" "\\\\" "\n" "\\n" timems.c))))

  (set *timems*
       (or (wildcard timems-loc)
           (begin
             (expect "" (mkdir-p (dir timems-loc)))
             (expect 0 (first (pipe (concat "cc -o " timems-loc " -x c -")
                                    timems.c)))
             timems-loc)))
  *timems*)


(define `timems-exe
  (or *timems*
      (build-timems)))


(define (get-time-ms)
  &public
  (define `output
    ;; bypass shell invocation, doubling performance
    (declare SHELL &native)
    (let-global ((SHELL timems-exe))
      ;; Provide non-empty argument or else `shell` does nothing
      (shell "-")))

  ;; remove the decimal point => return milliseconds
  (subst "." "" output))


(define (floor n)
  (word 1 (subst "." " " n)))


(define (clk-time-once fn)
  (let ((t0 (get-time-ms))
        (_ (fn))
        (t1 (get-time-ms)))
    (- t1 t0)))


(define (clk-time-iter fn reps)
  (define `fnx
    (subst " " "" "." fn (patsubst "%" "." (range 1 reps))))
  (if fn
      (clk-time-once fnx)
      0))


;; Iteration overhead for lambda call & foreach (~0.01ms)
(define t-nil nil)
;; Time consumed by get-time-ms
(define t-time 0)


(declare (clk-time-loop fn duration reps))

(define (clk-time-loop-next fn duration reps time)
  (if (or (>= time duration)
          (> reps 100000))
      [reps time]
      ;; t-low = lowest estimate for actual time taken
      ;;  -1 for resolution inaccuracy; *1.5 for t-time duration
      (let& ((t-low (- time (+ (* t-time 1.5) 1)))
             (max-mul (/ (+ duration 20) t-low))
             (mul (cond ((< time 5) 20)
                        ((< t-low 10) 6)
                        ((< t-low 20) 3)
                        ;; too much of a reach => shoot for 15%
                        ((> max-mul 10) (* 0.15 max-mul))
                        ;; larger mulitples => less accuracy
                        ;; smaller t-low => less accuracy
                        (else (* max-mul
                                 (* (+ 1 (* max-mul 0.007))
                                    (+ 1 (/ 2 t-low))))))))
        (clk-time-loop fn duration (floor (+ 1 (* reps mul)))))))

(define (clk-time-loop fn duration reps)
  (clk-time-loop-next fn duration reps (clk-time-iter fn reps)))


(define (calibrate ?duration)
  &public
  (define `durn (or duration default-duration))
  (define `(.reps o) (word 1 o))
  (define `(.time o) (word 2 o))

  ;; build timems and get it warm in the cache
  (get-time-ms)
  (set t-time (clk-time-once (lambda () nil)))

  ;; These initial 'reps' values shave a little time off
  (let ((cgt (clk-time-loop (lambda () (get-time-ms)) durn 2)))
    ;; there is one get-time-ms worth of overhead, hence reps+1
    (set t-time (/ (.time cgt) (+ 1 (.reps cgt)) 5))
    (let ((cnil (clk-time-loop (lambda () (and nil nil)) durn 1000)))
      (set t-nil (/ (- (.time cnil) t-time) (.reps cnil) 5)))))


;; Compute calibrated per-iteration time in milliseconds.
;;
(define (clk-result reps-time)
  (define `time (word 2 reps-time))
  (define `reps (word 1 reps-time))
  (define `t-per (- (/ (- time t-time) reps 4) t-nil))
  ;; limit to 4 significant digits
  (/ t-per 1 4))


;; Compute time (milliseconds) consumed by calling FN.
;;
(define (clk-time-fn fn ?duration ?reps)
  &public
  (or t-nil
      (calibrate))
  (clk-result (clk-time-loop fn
                             (or duration default-duration)
                             (or reps 1))))


;; Compute time (milliseconds) consumed by evaluating EXPR.
;;
(define `(clk-time expr ?duration ?reps)
  &public
  (clk-time-fn (lambda () (and expr nil)) duration reps))


;; Compute and print time (milliseconds) consumed by evaluating EXPR.
;;
(define `(clk-show name expr ?duration ?reps)
  &public
  (let ((time (clk-time-fn (lambda () (and expr nil)) duration reps)))
    (printf "%s: %s" name time)
    time))
