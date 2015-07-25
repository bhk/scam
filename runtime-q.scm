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

(define (rtrim-vec v prev)
  (if (filter-out "!." v)
      (concat prev (word 1 v) (rtrim-vec (wordlist 2 99999 v) " "))))


(expect (rtrim-vec ((lambda () *args*) 1 2 "" "3 4" "\n"))
        [1 2 "" "3 4" "\n"] )

(expect (rtrim-vec ((lambda () *args*)))
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

;; ^set-RHS  -- change from macro to function to test
;; (declare (^set-RHS))
;; (expect (^set-RHS "\n$#") "$(\\n)$$$(\\H)")

;; ^set-LHS

(declare (^set-LHS))
(expect (^set-LHS "a= c ")
        "$(if ,,a= c )")
(expect (^set-LHS "a\nb")
        "$(if ,,a$(\\n)b)")
(expect (^set-LHS ")$(")
        "$(if ,,$(\\R)$$$(\\L))")


;; ^ed

(declare (^ed n w))
(expect "$" (^ed 1))
(expect "$$$$" (^ed 4))

;; ^e

(declare (^e val lvl))
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
           "$(\\R)")

(expect (^e ")" 2)
           "$$(\\R)")

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


(info "runtime ok")
