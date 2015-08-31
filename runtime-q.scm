;; runtime-q runs in a special environment (same as the environment for
;; runtime) in which the runtime is not an implicit requirement.
(require "runtime")

;; runtime-test
;;
;; Note: we do not `require` runtime.scm because it is already loaded.  It
;; must be in place before any SCAM module can execute.
;;
;; Many of the runtime functions are tested by calling the "manifest
;; functions" that expose their functionality.  For example, "set-global" makes
;; use of `^set`.


(define (expect-x o i file-line)
  (if (findstring (concat o 1) (findstring (concat i 1) (concat o 1)))
      ""
      (error
       (print file-line ": error: assertion failed"
              "\nA: '" o "'"
              "\nB: '" i "'\n"))))

(define `(expect o i)
  (expect-x o i (current-file-line)))


;; ^u
;; ^d

(expect "a !b!0\t\nc" (promote (word 1 (demote "a !b!0\t\nc"))))

;; ^n

(expect "a b" (nth 2 (concat "1 " (demote "a b") " 3")))

;; ^set

(set-global ".v" "$$")
(expect (value ".v") "$$")
(expect (call ".v") "$$")

;; ^fset

(set-rglobal ".f" "$$")
(expect (value ".f") "$$")
(expect (call ".f") "$")

;; *args*

(expect ( (lambda () *args*) 1 2 "" "3 4" "\n")
        [1 2 "" "3 4" "\n"] )

(expect ( (lambda () *args*) "" "")
        [] )

;; apply

(define (rev a b c d e f g h i j k)
  (concat k j i h g f e d c b a))

(expect "321" (apply rev [1 2 3]))
(expect "11 10 321" (apply rev [ 1 2 3 "" "" "" "" "" "" "10 " "11 "]))

(define (indexarg n)
  (nth n *args*))

(expect "w" (apply indexarg
                   "24 a b c d e f g h i j k l m n o p q r s t u v w x y z"))

;; esc-LHS

(declare (esc-LHS))
(expect (esc-LHS "a= c ")
        "$(if ,,a= c )")
(expect (esc-LHS "a\nb")
        "$(if ,,a$!b)")
(expect (esc-LHS ")$(")
        "$(if ,,$]$$$[)")


;; ^ed

(declare (^ed n w))
(expect "$" (^ed 1))
(expect "$$$$" (^ed 4))

;; ^e

(declare (^es s))

(expect (^es "a b c")
        "a b c")

(expect (^es "a,b")
        "$(if ,,a,b)")

(expect (^e "abc")
        "abc")

(expect (^e ",")
        "$(if ,,,)")

(expect (^e "$")
        "$$")

(expect (^e "$" 2)
        "$$$$")

(expect (^e ")")
           "$]")

(expect (^e ")" 2)
           "$$]")

(define (TE str)
  (expect ((^e str))
             str))

(TE "a")
(TE "a b")
(TE "a$")
(TE "a$1")
(TE "a$2")
(TE ",")
(TE "x\ny")
(TE "$(")
(TE "$)")
(TE "a,b")
(TE "x), (a")
(TE " a ")

;; misc. macros

(expect "" nil)

(expect "1" (not nil))
(expect nil (not "x"))

(expect nil (bound? "_xya13"))
(expect "1" (bound? (global-name ^require)))

(expect "4 5" (nth-rest 4 "1 2 3 4 5"))

(expect "! 1" (first ["! 1" 2]))

(expect "b c" (rest "a   b c  "))

(expect "3 4" (rrest "1 2 3 4"))

;; hooks

(define (test) "X")
(add-hook "foo" (global-name test))
(expect "X" (run-hooks "foo"))


(info "runtime ok")
