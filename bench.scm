(require "core")
(require "num")

;;----------------  Timings
;; (old) num.scm
;;   1457 add
;;   2055 sub
;;   2191 mul
;;   2529 mod
;;   201 exp
;;   7 sum
;;   15 range
;;
;; fp.scm
;;   821 add
;;   1244 sub
;;   995 mul
;;   819 mod
;;   47.8 exp
;;   1.73 sum[i]
;;   10.1 range

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

(define mod-pairs
  (foreach a (range 1 100)
           (foreach b (append [1 2 2 2 2 3 3 3 4 5 6 7 8 9] (range 1 17))
                    (concat a ";" b))))
(time (lambda () (perform mod mod-pairs)) 1 "mod")

(time (lambda () (^ 131 131)) 10 "exp")

(define irange (range 1 20))
(time (lambda () (sum irange)) 100 "sum[i]")

(time (lambda () (range 56 1523)) 10 "range")

(define frange (foreach n irange (concat n "." n)))
(time (lambda () (sum frange)) 100 "sum[f]")
