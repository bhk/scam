(require "core.scm")
(require "peg.scm" &private)

;; lex & unlex
(expect 5 (words (lex ";; Test\n" [";" " " "\n" "(" ")"])))
(for (str [";; Test\n(ABC)"])
  (expect str (un-lex (lex str [";" " " "\n" "(" ")"]))))

;; peg-empty
(expect ((peg-empty "cap") "a b c" 1) (Yes 1 "cap"))

;; peg-p
(define `px (peg-p "x" nil "X"))
(define `py (peg-p "y" nil "Y"))
(expect (px "x y" 1)  (Yes 2 "X"))
(expect (px "y z" 1)  nil)
(expect ((peg-p "x") "x y" 1)  (Yes 2))
(expect ((peg-p "x%" "%y") "x xy y" 1)  (Yes 2))
(expect ((peg-p "x%" "%y") "xy y" 1)    nil)

;; peg-or
(expect ((peg-or px py) "x z" 1)  (Yes 2 "X"))
(expect ((peg-or px py) "y z" 1)  (Yes 2 "Y"))
(expect ((peg-or px py) "z z" 1)  nil)

;; peg-and
(expect ((peg-and px py) "x y z" 1) (Yes 3 "X Y"))
(expect ((peg-and px py) "x x y" 1) nil)

;; peg-*
(expect ((peg-* px) "x x y" 1) (Yes 3 "X X"))
(expect ((peg-* px) "y y x" 1) (Yes 1))

;; peg-not
(expect ((peg-not px) "y z" 1) (Yes 1))
(expect ((peg-not px) "x y" 1) nil)

;; peg-?
(expect ((peg-? px) "x y z" 1) (Yes 2 "X"))
(expect ((peg-? (peg-p "Z")) "x y z" 1) (Yes 1))

;; peg-at
(expect ((peg-not px) "y z" 1) (Yes 1))
(expect ((peg-not px) "x y" 1) nil)

;; peg-+
(expect ((peg-+ px) "x x y" 1) (Yes 3 "X X"))
(expect ((peg-+ px) "y y x" 1) nil)

;; peg-c
(expect ((peg-c "name" px) "x y" 1)  (Yes 2 (append "X" {name: "x"})))
