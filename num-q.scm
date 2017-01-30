(require "core")
(require "num" &private)

(define (E n) (uencode n))
(define (D n) (udecode n nil))  ;; only positive values

(expect ".i . .ii" (E 201))
(expect ".i . .ii" (E -201))
(expect ". ." (E 00))

(expect 0 (D ""))
(expect 0 (D ". . ."))
(expect 3 (D ".iii"))
(expect 3 (D ".iii"))
(expect 101 (D ".i . .i . ."))

(expect ". .i" (nnorm ". .i . ."))

(expect ".i .i . ." (u+ (E 5) (E 006)))

(expect ". .i" (u- (E 333) (E 323)))
(expect ". .iiiiiiiii .iiiiiiiii .-" (u- (E 333) (E 343)))
(expect ". .iiiiiiiii .iiiiiiiii .-" (u- (E 333) (E 343)))
(expect "" (u- (E 0) (E 0)))

(expect 3 (+ 1 2))
(expect 200 (+ 199 00001))

(expect -3 (+ -1 -2))

(expect 199  (+ 000200 -1))
(expect 19   (+ 20 -00001))
(expect -199 (+ -200 1))
(expect 0    (+ -2 2))
(expect -99  (+ -100 1))

(expect 99 (- 100 000001))
(expect 100 (- 200 100))

(expect 998001 (* 999 999))
(expect 6 (* 3 2))
(expect -6 (* 3 -2))
(expect -6 (* -3 2))


(expect "i" (nodd ".iii"))
(expect ""  (nodd ".ii"))

(expect ".i" (u/2 ".ii"))
(expect ".iiiii" (u/2 ". .i"))
(expect ".iiiiiiiii" (u/2 " .iiiiiiiii .i"))

(expect 1 (ucmp ".iii" ".ii"))
(expect 2 (ucmp ".ii" ".iii"))
(expect "" (ucmp ".ii" ".ii"))
(expect 2 (ucmp ".iii .i" ".i .iii"))
(expect 1 (ucmp ".iiiii" "."))
(expect 2 (ucmp "." ".iiiii"))

(expect 1 (cmp 1 -1))
(expect 2 (cmp -1 1))
(expect "" (cmp 0 -0))
(expect "" (cmp -0 0))

(expect 2  (cmp 1 2))
(expect "" (cmp 2 2))
(expect 1  (cmp 3 2))
(expect 1  (cmp -1 -2))
(expect "" (cmp -2 -2))
(expect 2  (cmp -3 -2))
(expect 2  (cmp 100 101))
(expect "" (cmp 100 00100))
(expect 1  (cmp 100 9))


(foreach n "1 2 3 4 5 6 7 8 9 10 21 12 13 14 15 16 17 18 19 20"
         (foreach m "1 2 3 5 6 9 10"
                  (begin
                    ;(print n "%" m " = " (mod n m))
                    (expect n (+ (* (/ n m) m) (mod n m))))))


(define (simple-range min max)
  (if (eq? min max)
      min
      (concat min " " (simple-range (1+ min) max))))

(expect "9 10 11 12"  (range 9 12))
(expect (simple-range 0 197) (range 0 197))

(expect "-2 -1 0" (range -2 0))
(expect "-1 0 1 2" (range -1 2))
(expect "-4 -3 -2" (range -4 -2))
(expect "" (range -4 -5))
(expect "" (range 5 4))

(expect 1 (> 5 1))
(expect 1 (> 5 0))
(expect 1 (>= 1 1))
(expect nil (> 0 0))

(expect 1 (min 1 2))
(expect 1 (min 2 1))

(expect 2 (max 1 2))
(expect 2 (max 2 1))

(expect 5 (abs 5))
(expect 5 (abs -5))

(expect 0 (sum ""))
(expect "6" (sum "1 2 3"))
(expect 5050 (sum (range 1 100)))

(expect 9 (num-pad 9 1 0))
(expect 123 (num-pad 123 1 0))
(expect 0123 (num-pad 123 4 0))
(expect "-00005" (num-pad -5 6 0))
(expect "    -5" (num-pad -5 6))
(expect " -123456789.0" (num-pad -123456789.0 13 " "))
(expect 00000000000000000123 (num-pad 123 20 0))

;;(time "sum" (lambda () (sum (range 1 5000))))
