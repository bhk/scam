(require "core")
(require "num" &private)

;;---------------- Utilities

;; nwords
(expect "" (nwords 0 ":"))
(expect ":" (nwords 1 ":"))
(expect ": : :" (nwords 3 ":"))
(expect ": : : :" (nwords 4 ":"))

;; nzeros
(expect "" (nzeros 0))
(expect "000" (nzeros 3))

;; abs
(expect 0 (abs -0))
(expect 1e-20 (abs 1e-20))
(expect 1 (abs -1))

;; tail
(expect nil (tail 0 "1 2 3"))
(expect "3" (tail 1 "1 2 3"))
(expect "1 2 3" (tail 3 "1 2 3"))
(expect "1 2 3" (tail 5 "1 2 3"))

;;---------------- U encoding

;; u-enc
(expect ":iii :ii :i" (u-enc "123"))
(expect ":" (u-enc "0"))
(expect ":i" (u-enc "1"))

;; u+
(define `(i-u+ a b) (u-dec-norm (u+ (u-enc a) (u-enc b))))
(expect 5 (i-u+ 2 3))
(expect 198 (i-u+ 99 99))

;; u*
(define `(i-u* a b) (u-dec-norm (u* (u-enc a) (u-enc b))))
(expect 6 (i-u* 2 3))
(expect 998001 (i-u* 999 999))

;; u-negate
(expect ": : :" (u-extend-ms ":" 3))

;; u-negate
(expect ":iiiiiiiiii" (u-negate ":"))
(expect ":i" (u-negate ":iiiiiiiii"))
(expect ":iiiiiiiii :iiiiiiiii :" (u-negate ":i : :iiiiiiiii"))

;; u-carry
(expect ":ii :i" (u-carry ":iiiiiiiiiiii :"))

;; u/2
(expect ":i" (u/2 ":ii"))
(expect ":ii" (u/2 ":iiiii"))
(expect ":ii" (u/2 ":iiiii"))
(expect ":iiiiiii" (u/2 ":iiiii :i"))


;;---------------- SU & I encoding

;; su+
(expect "- :i" (su+ "-" ":ii" "" ":i"))
(expect ":i" (su+ "-" ":ii" "" ":iii"))

;; su-sub, su-dec
(expect 1 (su-dec (su-sub ":iiii" ":iii")))
(expect -1 (su-dec (su-sub ":iii" ":iiii")))
(expect 0 (su-dec (su-sub ":ii" ":ii")))
(expect 1 (su-dec (su-sub ":i" nil)))
(expect -1 (su-dec (su-sub nil ":i")))

;; i+
(expect 3 (i+ 2 1))
(expect 3 (i+ "+2" 1))
(expect -3 (i+ -2 -1))
(expect 99 (i+ 100 -1))
(expect -99 (i+ 1 -100))
(expect -99 (i+ -100 1))
(expect 99 (i+ -1 100))
(expect 0 (i+ -1 1))
(expect 0 (i+ -0 0))
(expect 0 (i+ 0 -0))
(expect 0 (i+ -0 -0))

;; i-
(expect 1 (i- 2 1))
(expect -2 (i- 1 3))
(expect 4 (i- 1 -3))

;; i*
(expect 6 (i* "+2" "+3"))
(expect -6 (i* -2 "+3"))
(expect -6 (i* "+2" -3))
(expect 6 (i* -2 -3))


;;----------------  F encoding

;; f-enc
(expect "2 + :ii"  (f-enc "2e2"))
(expect "2 + :ii"  (f-enc "2e+2"))
(expect "0 + : : :ii"  (f-enc "200"))
(expect "-2 - :i" (f-enc "-1e-2"))
(expect "2 + :iii :ii :i" (f-enc "1.23e4"))
(expect "-1 + :iii :ii :i" (f-enc "1.23e1"))

;; trim
;;(expect "0.01005" (frac-trim 000.0100500))
;;(expect "0" (frac-trim "0."))
;;(expect "0" (frac-trim ".0"))

;; f-enc & f-dec
(expect "undefined" (f-dec "0 N"))
(expect "20" (f-dec "1 + :ii"))
(expect "0.02" (f-dec "-2 + :ii"))

(define (f-norm n) (f-dec (f-enc n)))
(expect 1 (f-norm 1))
(expect 10 (f-norm 100e-1))
(expect 100 (f-norm 1e2))
(expect 12.3 (f-norm 1230e-2))
(expect -0.012 (f-norm -1200e-5))
(expect "1e+23" (f-norm 1000e20))
(expect "1.234e+23" (f-norm 1234e20))
(expect 1.234e-17 (f-norm 1234e-20))
(expect 101 (f-norm 1.01e2))
(expect 100.1 (f-norm 1.001e2))
(expect 10 (f-norm 10.000))
(expect 10 (f-norm 10.))

(expect 0.000001 (f-norm 1e-6))
(expect 1e-7 (f-norm 1e-7))
(expect 100000000000000000000 (f-norm 1e20))
(expect "1e+21" (f-norm 1e21))

;; fp+
(expect 3 (fnum+ 1 2))
(expect 300 (fnum+ 1e2 2e2))
(expect 102 (fnum+ 1e2 2e0))
(expect 200.01 (fnum+ 2e2 1e-2))
(expect 100.02 (fnum+ 2e-2 1e2))
(expect 1000.0001 (fnum+ 1000 0.0001))
(expect -3 (fnum+ -1 -2))
(expect -2 (fnum+ 1 -3))

;; fp*
(expect 1.0201 (fnum* 1.01 1.01))
(expect 21.21 (fnum* 1.01 21))
(expect 12.12 (fnum* 12 1.01))

;; fp/

(expect (subst " " "" (nwords 123 "i"))
        (ticks ":iii :ii :i"))
(expect "iii" (tick-div "iiiiiiiiiiiii" "iiii"))

(expect ":i :ii" (u-div1 ":iiiii" ":iii"))
(expect ":iii :i :i" (u-div1 ":i : :i" ": :iii"))
;; 1080 / 109 = 9, remainder 99
(expect ":iiiiiiiii :iiiiiiiii :iiiiiiiii"
        (u-div1 ": :iiiiiiii : :i" ":iiiiiiiii : :i"))

(define `(n-divx a afrac b prec)
  (let ((o (u-divx (u-enc a) (reverse (u-enc afrac)) (u-enc b) prec)))
    (for u o (u-dec u))))

;; Return PREC digits
(expect [0200 nil nil] (n-divx 12 nil 60 4))
(expect [5000 nil nil] (n-divx 60 nil 12 4))
(expect [12 340 nil] (n-divx 1234 nil 1000 2))
;; Use AFRAC
(expect [12345 6 nil] (n-divx 12 3456 10 5))

;; divide by zero
(expect "undefined" (fnum/ 12 0 12))
;; digits in B < digits in A > prec
(expect 110 (fnum/ 1234 11 2))
;; digits in B < digits in A < prec (rounds down)
(expect 112.18 (fnum/ 1234 11 5))
;; digits in B > digits in A < prec
(expect 0.00218962 (fnum/ 10 4567 6))
;; top digits of A < top digits of B
(expect 0.12281 (fnum/ 112 912 5))
;; top digits of A > top digits of B
(expect 9.99 (fnum/ 999 100 5))
;; signs...
(expect 2 (fnum/ -4 -2 4))
(expect -2 (fnum/ -4 2 4))
(expect -2 (fnum/ 4 -2 4))
;; Round to even (up or down)) at 0.5
(expect 2 (fnum/ 15 10 1))
(expect 2 (fnum/ 25 10 1))

;; f-cmp
(expect nil (fnum-cmp 0 0))
(expect 1 (fnum-cmp 2 1))
(expect 2 (fnum-cmp 1 2))
(expect 2 (fnum-cmp 0001 2))
(expect nil (fnum-cmp 00 -0))

;;----------------  N encoding (ordinary decimal)

;; +
(expect 3 (+ 1 2))
(expect 1.01 (+ 1 0.01))
(expect 101 (+ 1 1e2))

;; -
(expect 3 (- 1 -2))
(expect 3.1 (- 1 -2.1))

;; *
(expect 6.03 (* 3 2.01))
(expect 6 (* 2 3))
(expect 999999998000000001 (* 999999999 999999999))

;; /
(expect 0.3333 (/ 1 3 4))


;; <, >
(expect 1 (< 1 2))
(expect nil (< 1 1))
(expect nil (< 2 1))
(expect 1 (< -2 1))
(expect 1 (< -2 -1))
;;TODO(expect nil (< -0 0))

(expect 1 (> 2 1))

;; <=, >=
(expect 1 (<= 1 2))
(expect 1 (<= 1 1))
(expect nil (<= 2 1))
(expect 1 (>= 2 1))
(expect 1 (>= 2 2))
(expect nil (>= 2 3))

;; =
(expect 1 (= 1 01))

;; min
(expect 1 (min 1 2))
(expect 1 (min 2 1))
(expect -2 (min -2 1))
(expect -1 (min -1 2))
(expect -2 (min -1 -2))

;; max
(expect 2 (max 1 2))
(expect 2 (max 2 1))
(expect 1 (max -2 1))
(expect 2 (max -1 2))
(expect -1 (max -1 -2))

;; ^
(expect 1 (^ 1 4))
(expect 1 (^ 9 0))
(expect 9 (^ 9 1))
(expect -1 (^ -1 3))
(expect 1024 (^ 2 10))

(expect 53792001073667580245500273166285089017012060180755370359117322123911064689253300016046634383708875787122634149387932959495834838306961830702021956443351096163277412670082451426348639402180745719769416191088953523045772459285730772627505878175485432413517902786553519786770528853189477801462217 (^ 137 137))

;; mod
(expect 0 (mod 0 7))
(expect "undefined" (mod 0 0))
(expect 2 (mod 2 7))
(expect -2 (mod -2 7))
(expect 6 (mod 20 7))
(expect 3 (mod 80 7))
(expect 0 (mod 123 1))
(expect 1 (mod 123 2))
(expect 0 (mod 123 3))
(expect 3 (mod 123 4))
(expect 3 (mod 123 5))
(expect 5 (mod 125 6))
(expect 6 (mod 125 7))
(expect 3 (mod 123 8))
(expect 2 (mod 110 12))
(expect 0 (mod 120 12))
(expect 10 (mod 130 12))

;; sum
(expect 0 (sum []))
(expect 1 (sum [1.0]))
(expect 1 (sum [0.9 0.1]))
(expect 9 (sum [9]))
(expect 6 (sum [1 2 3]))
(expect -6 (sum [-1 -2 -3]))

;; num-format
(expect "-05" (pad-and-sign "__5" "0" "-"))
(expect " -5" (pad-and-sign "__5" " " "-"))
(expect "005" (pad-and-sign "__5" "0" ""))
(expect "  5" (pad-and-sign "__5" " " ""))

(expect "??.?" (num-format 123 2 nil 1))
(expect "9" (num-format 9 1))
(expect "" (num-format -1 0))
(expect "0123." (num-format 123 4 0 0))
(expect "123.0" (num-format 123 4 nil 1))
(expect "  1" (num-format 1 3 " "))
(expect "-00005" (num-format -5 6 0))
(expect "    -5" (num-format -5 6 " "))
(expect "   -123456789" (num-format -123456789.0 13 " "))
(expect 00000000000000000123 (num-format 123 20 0))


;;----------------  Other functions

(expect 1 (i-norm 0001))
(expect 1 (i-norm "+0001"))
(expect -10200 (i-norm -00010200))
(expect 0 (i-norm -0))
(expect 0 (i-norm 0))

;; range
(expect "3 4 5 6" (range 3 6))
(expect "-2 -1 0 1 2" (range -2 2))
(expect "-2 -1 0" (range -2 0))
(expect "-2 -1 0" (range -2 -0))
(expect "-2 -1" (range -2 -1))
(expect "0 1" (range -0 1))
(expect "0" (range -0 0))
(expect "0 1" (range 000 001))
(expect "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20" (range 0 20))
