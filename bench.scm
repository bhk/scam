(require "core")
(require "num")
(require "string")
(require "io")

;; Build .out/timems if it isn't there...
(or (wildcard ".out/timems")
    (shell "cc -o .out/timems misc/timems.c"))

(define (get-time-ms)
  (subst "." "" (shell ".out/timems")))

(define (rep-words n lst)
  (if (subst "0" "" n)
      (if (word n lst)
          (wordlist 1 n lst)
          (rep-words n (concat lst " " lst " " lst)))))

(define `(pad-for str size)
  (string-repeat " " (- size (string-len str))))

(define `(myformat n size-left size-right)
  (define `left (word 1 (subst "." " " n)))
  (define `right (word 2 (subst "." " ." (concat 0 n))))
  (concat (pad-for left size-left) left right (pad-for right size-right)))

(define (show-result n)
  (print (myformat n 6 6)))

(define (time fn reps label)
  (printn (pad-for label 20) label " (ms) : ")
  (let ((rep-words (rep-words reps "1 1 1 1"))
        (fn fn)
        (label label)
        (t0 (get-time-ms)))
    (foreach r rep-words
             (if (fn) nil))
    (show-result (/ (- (get-time-ms) t0) reps 4))))


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

;;----------------  Timings


;;-------- parse

(require "parse")
(define (test-parse arg)
  (define *text* nil)
  (time (lambda () nil) 5 "nil")
  (for file (or arg (error "supply file name as first argument"))
       (set *text* (or (read-file file) (concat "file " arg " not found")))
       (time (lambda () (parse-text *text*) nil) 2 (concat "parse " file))))


(define (main arg)
  (test-parse arg)
  0)
