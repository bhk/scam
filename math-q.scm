(require "core.scm")
(require "math0.scm")
(require "math1.scm")
(require "math2.scm")
(require "math0-q.scm" &private)  ;; for (U ...)
(require "math.scm" &private)


;; non-integers?

(foreach (n ["-" "--1" " -1" "- 1" "0-1" "1-0" "?"])
  (assert (non-integers? n "01"))
  (assert (non-integers? "01" n)))
(assert (not (non-integers? "-0" "01")))

;; d-round

(expect "NaN" (d-round nil 0 ROUND-NEAR))
(expect 120 (d-round 123 -01 ROUND-NEAR))
(expect 130 (d-round 123 -01 ROUND-CEIL))

;; xlat-mode

(expect ROUND-FLOOR (xlat-mode "-"))
(expect ROUND-CEIL (xlat-mode "+"))
(expect ROUND-TRUNC (xlat-mode "|"))
(expect ROUND-AWAY (xlat-mode "<>"))
(expect ROUND-NEAR (xlat-mode ""))

;; 0-

(expect -1 (0- 1))
(expect 1 (0- -1))

;; abs

(expect 1.0 (abs -1.0))
(expect 1.0 (abs 1.0))


;;--------------------------------
;; Binary Operators
;;--------------------------------

;; `op` error cases

(define (u-ec x y) "011")
(define (f-ec x y) (and x y "0 + 011"))
(define (ec x y) (op "-ec" x y))

(expect NaN (ec NaN 1))
(expect NaN (ec nil 1))
(expect NaN (ec 1 nil))
(expect NaN (ec "-" 1))
(expect NaN (ec "--" 1))
(expect NaN (ec "- 0" 1))
(expect NaN (ec "1 " 1))
(expect NaN (ec "01" " 01"))
(expect NaN (ec "01" "01 "))
(expect NaN (ec "01" "- 01"))
(expect NaN (ec " 01" "01"))
(expect NaN (ec "01 " "01"))
(expect NaN (ec "- 01" "01"))

;; verify computed function names

(foreach (type "u f")
  (foreach (mode "+ - * ^ *~ / -cmp -mod -log -atan2")
    (define `name (.. (subst "op" type (native-name op)) mode))
    (expect name (if (native-bound? name) name))))


;; +

(expect 3 (+ 1 2))
(expect -1 (+ 1 -2))
(expect 4 (+ 1.9 2.1))

;; -

(expect 3 (- 5 2))
(expect 7 (- 5 -2))
(expect 3.01 (- 5.01 2))

;; *

(expect 6 (* 2 3))
(expect -6 (* 2 -3))
(expect 6.02 (* 2 3.01))

;; //

(expect 3 (// 7 2))
(expect -4 (// 7 -2))
(expect 333 (// 1e3 3))

;; mod

(expect 5 (mod 103 7))
(expect 1 (mod -7 2))
(expect -1 (mod 7 -2))
(expect 5.1 (mod 103.1 7))

;; ^

(expect 2048 (^ 2 11))
(expect 10000 (^ 1e2 2))
(expect 9.261 (^ 2.1 3))
(expect 8.5741877 (^ 2 3.1 8))
(expect 8 (^ 2.0 3))

;; *~

(expect 12220 (*~ 1111 11 4))
(expect 12220 (*~ 111.1 110 4))

;; /

(expect 0.1429 (/ 1 7 4))
(expect -0.1429 (/ 1 -7 4))
(expect 0.1429 (/ 1.0 7 4))
(expect NaN (/ 1.2 3 "x"))
(expect 0.1428571428571429 (/ 1 7))
(expect 0.1428 (/ 1 7 4 "-"))   ;; mode
(expect -0.1429 (/ -1 7 4 "-")) ;; mode
(expect -0.1428 (/ -1 7 4 "+")) ;; mode

;; log

(expect 3.3 (log 10 2 2))
(expect 3.3 (log 1e1 2 2))
(expect 2.3 (log 10 nil 2))

;; atan2

(expect 1.57 (atan2 1 0 3))
(expect 1.57 (atan2 1.0 0 3))
(expect (atan2 0 2) 0)
(expect (atan2 0 -2.0 3) 3.14)
(expect "NaN" (atan2 0 0))
(expect "NaN" (atan2 "x" 1))
(expect "NaN" (atan2 0 0 "x"))


;; cmp

(expect 1 (cmp 1 0))
(expect 1 (cmp 1 -1))
(expect "~" (cmp -1 1))
(expect "~" (cmp 0 1))
(expect 1 (cmp 1.0 0))
(assert (findstring 1 (cmp 12.3 1.32)))
(assert (findstring 1 (cmp 2 -3)))
(assert (findstring 1 (cmp 2 1)))
(expect 1 (cmp 1 nil))
(expect "~" (cmp nil 1))


;;--------------------------------
;; relational operators, min, max
;; <, >, =, !=, <=, >=
;;--------------------------------

(expect 1   (< 1 2))
(expect nil (< 2 2))
(expect nil (< 3 2))

(expect nil (> 1 2))
(expect nil (> 2 2))
(expect 1   (> 3 2))

(expect nil (= 1 2))
(expect 1   (= 2 2))
(expect nil (= 3 2))

(expect 1   (!= 1 2))
(expect nil (!= 2 2))
(expect 1   (!= 3 2))

(expect 1   (<= 1 2))
(expect 1   (<= 2 2))
(expect nil (<= 3 2))

(expect nil (>= 1 2))
(expect 1   (>= 2 2))
(expect 1   (>= 3 2))

;; min

(expect 1.0 (min 1.0 3.0))
(expect nil (min 3.0 nil))
(expect "NaN" (min 3.0 "NaN"))

;; max

(expect 3.0 (max 1.0 3.0))
(expect 3.0 (max 3.0 nil))
(expect 3.0 (max 3.0 "NaN"))


;;--------------------------------
;; operations on a single number
;;--------------------------------

(expect "NaN" (round "x"))
(expect "NaN" (exp nil 1))
(expect "NaN" (exp "X" 1))

;; floor

(expect 3 (floor 3.001))
(expect -4 (floor -3.001))

;; ceil

(expect 4 (ceil 3.001))
(expect -3 (ceil -3.001))

;; trunc

(expect 3 (trunc 3.001))
(expect -3 (trunc -3.001))

;; round

(expect 120 (round 123 2))
(expect 123 (round 123.456))
(expect 123.46 (round 123.456 -2))
(expect 123.45 (round 123.456 -2 "-"))
(expect 123.45 (round 123.456 -2 "|"))
(expect 123.46 (round 123.451 -2 "+"))
(expect -123.46 (round -123.456 -2 "-"))
(expect -123.45 (round -123.456 -2 "|"))
(expect -123.45 (round -123.456 -2 "+"))

;; exp

(expect 2.718281828459045 (exp 1))
(expect 0.36788 (exp -1 5))
(expect 1.1051709 (exp 0.1 8))

;; sin, cos, atan

(expect 0.8414709848078965 (sin 1))
(expect -0.841 (sin -1.0 3))
(expect 0.5403 (cos 1 -4))
(expect 0.7853981633974483 (atan 1))
(expect -0.7854 (atan -1.0 4))
(expect "NaN" (atan 1 "x"))
(expect "NaN" (atan "x"))
(expect "NaN" (atan nil))
(expect "NaN" (atan 1 "x"))


;;--------------------------------
;; vec-op: sum & multi-arg +, *
;;--------------------------------

(expect 9 (+ 3 2 4))
(expect 24 (* 3 2 4))

;; sum

(expect 0 (sum))
(expect 1 (sum 1.0))
(expect 3.1 (sum 1 2.1))
(expect 7.1 (sum [ [1 2.1] [] ] 4))

;; frexp10

(expect [0 0] (frexp10 0e10))
(expect [0 0] (frexp10 0.00e10))
(expect [0.1 1013] (frexp10 1.00e1012))
(expect [-0.123 -1] (frexp10 -1.23e-2))


;;--------------------------------
;; range
;;--------------------------------

;; uv-trim

(expect (uv-trim "0" "0" "1 2 3") "1 2 3")
(expect (uv-trim "01" "0" "1 2 3") "2 3")
(expect (uv-trim "01" "01" "1 2 3") "2")
(expect (uv-trim "011" "01" "1 2 3") "")

;; uv-range

(expect "1 2 3" (strip (uv-range (UV 1) (UV 3))))
(expect "0 1 2 3" (strip (uv-range (UV 0) (UV 3))))
(expect "8 9 10" (strip (uv-range (UV 8) (UV 10))))

;; range

(expect "1 2 3" (range 1 3))
(expect "0 1 2 3" (range 0 3))
(expect "0 1 2 3" (range -0 3))
(expect "-2 -1 0 1" (range -2 1))
(expect "-2 -1 0" (range -2 0))
(expect "-2 -1 0" (range -2 -0))
(expect "0" (range 0 0))
(expect "0" (range -0 -0))
(expect "0" (range -0 0))
(expect "0" (range 0 -0))
(expect "8 9 10" (range 8 10))
(expect "98 99 100" (range 98 100))
(expect nil (range "NaN" 100))
(expect nil (range nil 100))
(expect nil (range 1.0 2.0))  ;; not yet supported
(expect nil (range 1 1e2))    ;; not yet supported


;;--------------------------------
;; format-fixed
;;--------------------------------

;; format-fixed (see also f-fix tests)

;; error cases
(expect (format-fixed nil nil nil) "NaN")
(expect (format-fixed nil 5 nil) "  NaN")
(expect (format-fixed 1 "x" nil) "[INVALID_FORMAT]")
(expect (format-fixed 1 nil "x") "[INVALID_FORMAT]")
;; with/without min-width/decimals
(expect (format-fixed 1.3 nil nil) "1")
(expect (format-fixed 1.2 3   nil) "  1")
(expect (format-fixed 1e3 nil 2)   "1000.00")
(expect (format-fixed 1e3 10  2)   "   1000.00")
;; rounding
(expect (format-fixed 1.9996 3 3) "2.000")
(expect (format-fixed 1.999 3 3) "1.999")


;;--------------------------------
;; num-lex, num-sort
;;--------------------------------

;; lex-exp

(define `(elt n)
  (u2d (lex-exp (d2u n))))

(expect "1"     (elt 1))
(expect "8"     (elt 8))
(expect "909"   (elt 9))
(expect "910"   (elt 10))
(expect "989"   (elt 89))
(expect "99090" (elt 90))
(expect "09"   (elt 0))
(expect "08"   (elt -1))
(expect "01"   (elt -8))
(expect "0090" (elt -9))

;; num-lex

(expect 0 (num-lex 0))
(expect 11 (num-lex 1))
(expect 11 (num-lex 01))
(expect 18 (num-lex 8))
(expect 1911 (num-lex 9.11))
(expect "091" (num-lex 0.1))
(expect "099" (num-lex 0.9))
(expect "0991" (num-lex 0.91))
(expect "-900:" (num-lex -0.9))
(expect "-9008:" (num-lex -0.91))

;; num-sort

(define `sort-test-vec
  { -2: "a", -0.91: "b", -0.01: "c", 0.9: "d", 2: "e", 10: "f"})
(expect sort-test-vec (num-sort (sort sort-test-vec)))


;;--------------------------------
;; Stress Tests
;;--------------------------------

(define naturals
  [ 1 2 3 9 10 23 99 199 299 10000001 99999999
    21356736517905213567365179051 ])

(define decimals
  (append naturals
          (foreach (s ".1 .9999 .0000001 .0000555531")
            (addsuffix s naturals))))

(define num-args
  (append 0
          decimals
          (addprefix "-" decimals)
          (foreach (e "e1 e-3 e5 e20 e-100")
            (addsuffix e [2 5.000001 9.9999999]))))


(define (num-eq? x y)
  (or (eq? x y)
      (= x y)))


;; Validate arithmetic identities
;;
(define (stress-test x-args y-args)
  (foreach (x x-args)
    (expect 1 (<= (floor x) x))
    (expect 1 (>= (ceil x) x)))

  (foreach (x x-args)
    (print "x = " x)
    (foreach (y (filter-out "0 -0" y-args))

      (define `(check-eq a b)
        (if (not (num-eq? a b))
            (begin
              (print "x = " x ", y = " y)
              (expect a b))))

      (check-eq x (+ (- x y) y))
      (check-eq x (+ (* y (// x y)) (mod x y)))
      (check-eq x (/ (* x y) y 16))
      nil)))


(define (main argv)
  (if argv
      (stress-test num-args num-args))

  ;; success
  nil)
