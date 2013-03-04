;; runtime-test

;; NOTE THE PROBLEM WITH RUNTIME TESTING:
;;
;; The runtime is not an ordinary SCAM module. A runtime must be loaded
;; before any compiled SCAM code runs. It is never loaded via require
;; *during* program execution, since loading a new version of the runtime
;; woud interfere unpredictably with the version already loaded.
;;
;; As a result, the ordinary "*-q.scm" convention cannot be used to test it
;; while the compiler is being built. Instead we test it first thing after
;; a compiler is built with it (by including it from core-q).


;; We don't use 'expect' because that depends on 'core'.
(define (assert-eq actual expected)
  (define `(eq a b)
    (if (findstring a (findstring b a))
        "1"
        (if (or a b) "" 1)))
  
  (if (eq actual expected)
      "" ; (info "ok" actual)
    (error (concat "Assertion failed:\n"
                   "Expected: '" expected "'\n"
                   "  Actual: '" actual "'\n"))))

;; ^set
(declare (^set var val ret))

(^set ".v" "$$")
(assert-eq (value ".v") "$$")
(assert-eq (call ".v") "$$")

;; ^fset
(declare (^fset var val ret))

(^fset ".f" "$$")
(assert-eq (value ".f") "$$")
(assert-eq (call ".f") "$")

;; *args*
(define (rtrim-vec v prev)
  (if (filter-out "!." v)
      (concat prev (word 1 v) (rtrim-vec (wordlist 2 99999 v) " "))))


(assert-eq (rtrim-vec ((lambda () *args*) 1 2 "" "3 4" "\n"))
           [1 2 "" "3 4" "\n"] )

(assert-eq (rtrim-vec ((lambda () *args*)))
           [] )


;; ^apply

(declare (^apply))
(define (abacab a b c) (concat a b a c a b))
(assert-eq 121312 (^apply abacab [1 2 3]))


;; ^esc-RHS

(declare (^esc-RHS))
(assert-eq (^esc-RHS "\n$#") "$(\\n)$$$(\\H)")

;; ^esc-LHS

(declare (^esc-LHS))
(assert-eq (^esc-LHS "a= c ")
           "$(if ,,a= c )")
(assert-eq (^esc-LHS "a\nb")
           "$(if ,,a$(\\n)b)")
(assert-eq (^esc-LHS ")$(")
           "$(if ,,$(\\R)$$$(\\L))")
           

;; ^e
(declare (^e val lvl))
(declare (^se s))

(assert-eq (^se "a b c")
           "a b c")

(assert-eq (^se " a b")
           "$(if ,, a b)")

(assert-eq (^se "ab ")
           "$(if ,,ab )")

(assert-eq (^se "a,b")
           "$(if ,,a,b)")

(assert-eq (^e "abc")
           "abc")

(assert-eq (^e ",")
           "$(if ,,,)")

(assert-eq (^e "$")
           "$$")

(assert-eq (^e "$" 2)
           "$$$$")

(assert-eq (^e ")")
           "$(\\R)")

(assert-eq (^e ")" 2)
           "$$(\\R)")

(define (TE str)
  (assert-eq ((^e str))
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

