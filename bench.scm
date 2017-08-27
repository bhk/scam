(require "core")
(require "fp")

;;----------------  Timings
;; num.scm: 4488 pairs
;;    1378 add  (10 reps)
;;    1946 sub
;;    2146 mul
;;      15 range
;;
;; fp.scm: 4488 pairs
;;    1040 add  (10 reps)
;;    1430 sub
;;    1662 mul
;;    4538 range
;;      11 range

;;(define `(+ a b) (i+ a b))

(define (get-time-ms)
  (subst "." "" (shell ".out/timems")))

(define (rep-words n lst)
  (if (subst "0" "" n)
      (if (word n lst)
          (wordlist 1 n lst)
          (rep-words n (concat lst " " lst " " lst)))))

(define (time fn reps label)
  (let ((rep-words (rep-words reps "1 1 1 1"))
        (fn fn)
        (label label)
        (t0 (get-time-ms)))
    (foreach r rep-words
             (if (fn) nil))
    (print (/ (- (get-time-ms) t0) reps) " " label)))


(define `(big-num n)
  (subst "1" "x" "9" "1" "x" "9" " " "" (range 1 n)))

(define pairs
  (append
   (foreach a (range 1 99)
            (foreach b (range 9 19)
                     (concat a ";" b " "
                             0 ";" a " "
                             1 ";" a " "
                             2 ";" a " ")))
   (foreach a (range 1 11)
            (foreach b (range 48 59)
                     (concat (big-num a) ";" (big-num b))))))

(define `(perform op pairs)
  (define `(field n w)
    (word n (subst ";" " " w)))

  (foreach p pairs
           (if (op (field 1 p) (field 2 p)) nil)))

(print (words pairs) " pairs")
;;(time (lambda () (findstring "-" "+")) 10000 "findstring")
;;(time (lambda () (filter "-" "+")) 10000 "filter")
(time (lambda () (perform + pairs)) 1 "add")
(time (lambda () (perform - pairs)) 1 "sub")
(time (lambda () (perform * pairs)) 1 "mul")

(time (lambda () (range 56 1523)) 10 "range")
