;; Euler Project solutions in SCAM
;; see http://projecteuler.net/problem=1

(require "core")
(require "math")

;;----------------------------------------------------------------
;; Problem 1
;;----------------------------------------------------------------

;; Solution A: brute force

(define (isX3or5 n)
  (or (eq? 0 (mod n 3))
      (eq? 0 (mod n 5))))

(define (ep1a min max)
  (sum (select-words isX3or5 (range min max))))

;; test
(expect 23 (ep1a 1 9))


;; Solution B: analytical

;; return the sum of a sequence of numbers (proof via induction)
;;   first = first number in sequence
;;   step = difference between successive numbers
;;   len = length of sequence
(define (sum-seq first step len)
  (+ (* first len)
     (* step (/ (* len (- len 1)) 2 40))))

;; test
(expect 6 (sum-seq 1 1 3))   ;; 1+2+3
(expect 7 (sum-seq 2 3 2))   ;; 2+5
(expect 0 (sum-seq 2 3 0))   ;; empty

;; round x up to the next multiple of n (result >= x)
(define (round-up x n)
  (+ x (- n (1+ (mod (- x 1) n)))))

;; test
(expect 14 (round-up 11 7))


(define (sum-multiples min max n)
  (let ((a (round-up min n))       ;; multiple >= min
        (b (round-up (1+ max) n))) ;; multiple > max
    (sum-seq a n (/ (- b a) n))))

;; test
(expect 6 (sum-multiples 1 3 1))    ;; 1+2+3
(expect 18 (sum-multiples 1 10 3))  ;; 3+6+9


(define (ep1b min max)
  (- (+ (sum-multiples min max 3)
        (sum-multiples min max 5))
     (sum-multiples min max 15)))

;; test
(expect 23 (ep1b 1 9))
(expect 233168 (ep1b 1 999))
(assert (= 233333333333166666666668 (ep1b 1 999999999999)))

;; main

;; time execution of `fn` (a lambda value)
(define (get-time)
  (shell "date +%s"))

(define (time msg fn)
  (let ((t0 (get-time))
        (result (fn))
        (t1 (get-time)))
    (printf "%s: %s seconds" msg (- t1 t0))
    result))

(define (main)
  ;; contrast execution times...
  (print (time "ep1b" (lambda () (ep1b 1 9999))))
  (print (time "ep1a" (lambda () (ep1a 1 9999)))))
