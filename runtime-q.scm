;; runtime-q

;; It is unusual to require "runtime", and not ordinarily supported, since
;; it must be loaded before `require` can be called.  runtime-q needs
;; private symbols, so it requires runtime explicitly.  We set *require* to
;; prevent runtime from being eval'ed again.
(declare *required*)
(set *required* "'runtime")
(require "runtime" &private)

;; runtime-test
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

;; ...

(expect ( (lambda (...x) x) 1 2 "" "3 4" "\n" "")
        [1 2 "" "3 4" "\n"] )
(expect ( (lambda (...x) x) 1 2 3 4 5 6 7 8 9 10 11 "")
        [1 2 3 4 5 6 7 8 9 10 11])
(expect (.foreach "N" 2 (call "^v" 1 2 3))
        [2 3])

;; apply

(define (rev a b c d e f g h i j k)
  (concat k j i h g f e d c b a))

(expect "321" (apply rev [1 2 3]))
(expect "11 10 321" (apply rev [ 1 2 3 "" "" "" "" "" "" "10 " "11 "]))

(define (indexarg n ...args)
  (nth n args))

(expect "x" (apply indexarg
                   "24 a b c d e f g h i j k l m n o p q r s t u v w x y z"))

;; esc-LHS

(declare (esc-LHS str))
(expect (esc-LHS "a= c ")
        "$(if ,,a= c )")
(expect (esc-LHS "a\nb")
        "$(if ,,a$'b)")
(expect (esc-LHS ")$(")
        "$(if ,,$]$$$[)")


;; ^E

(expect (^E "$,)")
        "$(if ,,$`,$])")

(expect (^E "$" "`")
        "$`(if ,,$``)")

(define `(TE str)
  (expect ((^E str))
          str)
  ;; escape twice, expand twice
  (expect (((^E str "`")))
          str))

(TE " ")
(TE "$,)(")
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

;; trace bootstrap

(set *required* (concat *required* " " "'trace"))
(define (trace-ext spec)
  (concat "traced: " spec))

(expect "traced: x" (trace "x"))

;; atexits

(define at-exit-worked nil)
(at-exit (lambda () (set at-exit-worked 1)))
(run-at-exits)
(expect 1 at-exit-worked)

(info "runtime ok")
