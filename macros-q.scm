;;--------------------------------------------------------------
;; Tests for compile.scm
;;--------------------------------------------------------------

(require "core")
(require "parse")
(require "gen")
(require "gen0")
(require "gen1")
(require "macros")


;; in case something went very wrong...
(if (eq "a" (first "a b c")) "" (error "something's wrong"))

(define (DUMP name val)
  (if (findstring "D" SCAM_DEBUG)
      (print name ": " (format val)))
  val)

;; compile first expression in text to IL
(define (C0X text env)
  (DUMP "gen0" (gen0 (parse-text text) env)))

;; compile first expression in text
(define (CX text env is-file)
  (nth 2 (DUMP "gen1" (gen1 (DUMP "gen0" (gen0 (parse-text text) env)) is-file))))

;; compile and execute form
;;
;; WARNING: Executing generated code may invoke runtime functions, which
;; may be out of sync with the compiler sources when we are compiling the
;; compiler (when building/testing v1 we execute with v0's runtime).
;;
(define (XT text)
  ((CX text)))

(define (strip-indices il)
  (if (word 1 il)
      (append (word 1 (subst "." " " (word 1 il)))
              (for w (rest il) (strip-indices w)))))


;;--------------------------------------------------------------
;; macro tests
;;--------------------------------------------------------------

;; print

(expect "$(info )" (CX "(print)"))
(expect "$(info 1)" (CX "(print 1)"))
(expect "$(info 123)" (CX "(print 1 2 3)"))

;; native
(expect "$x"
        (CX "(native \"$x\")"))

(expect ["E" "invalid CODE in (native CODE); expected a literal string"]
        (strip-indices (first (gen-extract (CX "(native FOO)")))))
           
;; expect

;(expect "" (CX "(expect (parse 1 2) 2)"))


;; concat

(expect "ab$(or 1)" (CX "(concat \"a\" \"b\" (or 1))"))

(expect "$$1$$(call ^n,1,$$9)"
        (CX "(lambda (a b c d e f g h i) (concat a i))"))


;; vector

(expect "1 2" (CX "(vector 1 2)"))
(expect ["a" "b c" "d"] (CX "(vector \"a\" \"b c\" \"d\")"))
(expect "$(call ^d,$(or 1))" (CX "(vector (or 1))"))

;; set

(expect "$(call ^set,var,1)" (CX "(declare var) (set var 1)"))
(expect "$(call ^fset,fn,1)" (CX "(declare (fn)) (set fn 1)"))
(expect "$(call ^fset,fn,1,2)" (CX "(declare (fn)) (set fn 1 2)"))


;; ?

(expect "$(call ^trace,f,1)" (CX "(? f 1)" (bind "f" "F f")))



;; let&

(expect (bind "y" ["M" "Q Y"]
          (bind "x" ["M" "Q 1"]
            (bind "a" "Q.1 S")))
       (let&-env [ ["L" "S x" "Q 1"] ["L" "S y" "Q Y"] ]
                 (bind "a" "Q.1 S")))


(expect (bind "x" ["M" "S a"]
              (lambda-env ["S a"] ""))
        (let&-env [ ["L" "S x" "S a"] ]
                  (lambda-env ["S a"] "")))


(expect "1" (CX "(let& ((a 1)) a)"))
(expect "2" (CX "(let& ((a 1) (b 2)) b)"))
(foreach SCAM_DEBUG "-"
    (expect "$(call f,1)" (CX "(begin (declare (f)) (let& ((x (f 1))) x))")))


(let-global ((SCAM_DEBUG ""))
    (expect "$$(call ^Y,x,,,,,,,,,$$$$1$$(call ^e,$$1))"
            (CX "(lambda (a) (let& ((x a)) (let ((b \"x\")) (concat b x))))")))


;; let


(define (macrotest in out)
  (let&
   ((name (concat "ml.macro-" (symbol-name (nth 2 in)))))
   (let ((result (strip-indices (call name in)))
         (expected (strip-indices out)))
     (if (eq result expected)
         1
         (begin
           (print "Result: " (subst "\n" "\n        " (format-form result)))
           (print "   Not: " (subst "\n" "\n        " (format-form expected))))))))


(expect 1 (macrotest
           '(let ((a 1) (b "q")) (+ a b))
           '((lambda (a b) (+ a b)) 1 "q")))

(expect 1 (macrotest
           '(let (a) a)
           ["E" "invalid (VAR VALUE) in (let ((VAR VALUE)...) BODY); expected a list"]))

(expect 1 (macrotest
           '(let a a)
           ["E" "invalid ((VAR VALUE)...) in (let ((VAR VALUE)...) BODY); expected a list"]))

(expect 1 (macrotest
           '(let (("a")) a)
           ["E" "invalid VAR in (let ((VAR VALUE)...) BODY); expected a symbol"]))

(expect 1 (macrotest
           '(let ((a)) a)
           ["E" "missing VALUE in (let ((VAR VALUE)...) BODY)"]))

(expect (strip-indices (c0 '((lambda (a b) (a b)) "$1" 2)))
        (strip-indices (c0 '(let ((a "$1")(b 2)) (a b)))))


;; let-global

(expect 1 (macrotest
           '(let-global ((V 1)) (print 1) V)
           '(set V (set V 1 V) (begin (print 1) V))))

(expect 1 (macrotest
           '(let-global ((X 1)
                         (Y 2))
                        3)
           '(set X (set X 1 X)
                 (set Y (set Y 2 Y)
                      3))))

;; foreach

(expect "$(foreach v,1 2 3,$v)"
        (CX "(foreach v \"1 2 3\" v)"))

(expect 22
        (XT "(foreach x \"2\" (concat x (value \"x\")))"))

(expect [["E.2" "\"foreach\" accepts 3 arguments, not 2"]]
        (C0X "(foreach a b)"))

;; for

(expect (strip-indices (c0 '(foreach a& [1 2]
                                     (call "^d" (let& ((a (call "^u" a&)))
                                                      (or a))))))
        (strip-indices (c0 '(for a [1 2] (or a)))))

(expect "$(foreach x&,1 2 3,$(call ^d,$(and $(call ^u,$(x&)))))"
     (CX "(for x \"1 2 3\" (and x))"))


;; cond

(expect 1 (macrotest
           '(cond (a b) (c d))
           '(if a b (if c d))))

(expect 1 (macrotest
           '(cond (a b) (c d e) (else f g))
           '(if a b (if c (begin d e) (begin f g)))))

(print "compile ok")
