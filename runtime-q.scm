;; runtime-q

;; It is unusual to require "runtime", and not ordinarily supported, since
;; it must be loaded before `require` can be called.  runtime-q needs
;; private symbols, so it requires runtime explicitly.  We set *RM* to
;; prevent runtime from being eval'ed again.
(declare *RM*)
(set *RM* "runtime")
(require "runtime.scm" &private)


;; Many of the runtime functions are tested by calling the "manifest
;; functions" that expose their functionality.  For example, "set-native" makes
;; use of `^S`.

(define (expect-x o i file-line)
  (if (findstring (.. o 1) (findstring (.. i 1) (.. o 1)))
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

(expect "a b" (nth 2 (.. "1 " (demote "a b") " 3")))

;; ^S

(define `(test-set value)
  (set-native ".v" value)
  (expect (native-value ".v") value)
  (expect (native-var ".v") value))
(test-set "a$b$$b\\#")
(test-set "\\")
(test-set "\\\\")

;; ^SF

(define `(test-fset code value)
  (set-native ".v" value)
  (expect (native-call ".v" "A" "B") value))
(test-fset "$1$$\\#" "A$\\#")
(test-fset "\\" "\\")
(test-fset "\\\\" "\\\\")

;; ...

(expect ( (lambda (...x) x) 1 2 "" "3 4" "\n" "")
        [1 2 "" "3 4" "\n"] )
(expect ( (lambda (...x) x) 1 2 3 4 5 6 7 8 9 10 11 "")
        [1 2 3 4 5 6 7 8 9 10 11 ""])
(expect ( (lambda (...x) x) )
        [])

;; apply

(define (rev a b c d e f g h i j k)
  (.. k j i h g f e d c b a))

(expect "321" (apply rev [1 2 3]))
(expect "11 10 321" (apply rev [ 1 2 3 "" "" "" "" "" "" "10 " "11 "]))

(define (indexarg n ...args)
  (nth n args))

(expect "x" (apply indexarg
                   "24 a b c d e f g h i j k l m n o p q r s t u v w x y z"))

;; name-apply

(expect "11 10 321" (name-apply (native-name rev)
                                [ 1 2 3 "" "" "" "" "" "" "10 " "11 "]))


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

(expect (^E " $ " "`")
        "$`(if ,, $`` )")

(expect (^E "a($)b")
        "a$[$`$]b")

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

;; misc. macros and functions

(expect "" nil)

(expect "1" (not nil))
(expect nil (not "x"))

(expect nil (native-bound? "_xya13"))
(expect "1" (native-bound? (native-name ^R)))

(expect "4 5" (nth-rest 4 "1 2 3 4 5"))

(expect "! 1" (first ["! 1" 2]))

(expect "b c" (rest "a   b c  "))

(expect "3 4" (rrest "1 2 3 4"))

;; atexits

(define at-exit-worked nil)
(at-exit (lambda () (set at-exit-worked 1)))
(^OE)
(expect 1 at-exit-worked)


;; ^t

(define *log* nil)
(define (logln str)
  (set *log* (.. *log* str "\n")))
(define (hijack-info fnbody)
  (subst "(info " (.. "(call " (native-name logln) ",") fnbody))
(set E? (hijack-info E?))
(set R? (hijack-info R?))

(define (f ...args)
  args)

(expect [1 2 3 4 5 6 7 8 9 10 11]
        (^t (native-name f) 1 2 3 4 5 6 7 8 9 10 11))
