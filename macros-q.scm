;;--------------------------------------------------------------
;; Tests for compile.scm
;;--------------------------------------------------------------

(require "core")
(require "parse")
(require "gen")
(require "gen0")
(require "gen1")
(require "escape")
(require "macros" &private)

;;--------------------------------
;; utilities
;;--------------------------------

;; Trim ".INDEX" suffixes from a vector of forms.
(define (trim-indices forms)
  (for form forms
       (append (word 1 (subst "." " " (word 1 form)))
               (if (type? "L% `% '% ,% @%" form)
                   (trim-indices (rest form))
                   (rest form)))))

;; Parse, trim position data, and return vector of forms.
(define (pp text)
  (trim-indices (parse-text text)))

;; Parse and trim position data; return ONE form.
(define (p1 text)
  (let ((o (pp text)))
    (expect "" (word 2 o))
    (first o)))

(define (DUMP name val)
  (if (findstring "D" SCAM_DEBUG)
      (print name ": " (format val)))
  val)

;; compile first expression in text to IL
(define (C0X text env)
  (DUMP "gen0" (gen0 (parse-text text) env)))

;; compile first expression in text
(define (CX text env is-file)
  (define `forms (parse-text text))
  (define `il (gen0 forms (or env base-env)))
  (define `errors-exe (gen1 il is-file))

  (nth 2 errors-exe))
;  (nth 2 (DUMP "gen1" (? gen1 (DUMP "gen0" (gen0 (parse-text text) env)) is-file))))

;; TEMP: data

;(printf "data = %q" (CX "(data Type (Ctor a))"))
(expect
 (concat "$(call " (global-name ^add-tags) ",!1:Type0!=Ct!0S)")
 (CX "(data Type (Ct a))"))

;; compile and execute form
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

;; concat

(expect "ab$(or 1)" (CX "(concat \"a\" \"b\" (or 1))"))

(expect (escape "$1$(call ^n,1,$9)")
        (CX "(lambda (a b c d e f g h i) (concat a i))"))

;; vector

(expect "1 2" (CX "(vector 1 2)"))
(expect ["a" "b c" "d"] (CX "(vector \"a\" \"b c\" \"d\")"))
(expect "$(call ^d,$(or 1))" (CX "(vector (or 1))"))

;; set

(expect "$(call ^set,var,1)" (CX "(declare var &global) (set var 1)"))
(expect "$(call ^fset,fn,1)" (CX "(declare (fn) &global) (set fn 1)"))
(expect "$(call ^fset,fn,1,2)" (CX "(declare (fn) &global) (set fn 1 2)"))

;; ?

(expect "$(call ^t,f,1)" (CX "(? f 1)" (hash-bind "f" "F f")))

;; let&

(expect (hash-bind "y" (ESMacro "Q Y" "")
         (hash-bind "x" (ESMacro "Q 1" "")
          (hash-bind "a" (EVar "a" "."))))
       (let&-env [ ["L" "S x" "Q 1"] ["L" "S y" "Q Y"] ]
          (hash-bind "a" (EVar "a" "."))))


(expect (hash-bind "x" (ESMacro "S a" nil)
              (lambda-env ["S a"] ""))
        (let&-env [ ["L" "S x" "S a"] ]
                  (lambda-env ["S a"] "")))

(expect "1" (CX "(let& ((a 1)) a)"))
(expect "2" (CX "(let& ((a 1) (b 2)) b)"))
(foreach SCAM_DEBUG "-"
    (expect "$(call f,1)" (CX "(begin (declare (f) &global) (let& ((x (f 1))) x))")))


(let-global ((SCAM_DEBUG ""))
    (expect "$`(call ^Y,x,,,,,,,,,$``1$`(call ^E,$`1))"
            (CX "(lambda (a) (let& ((x a)) (let ((b \"x\")) (concat b x))))")))

;; let

(define (macrotest in out)
  (let&
      ((name (local-to-global (concat "ml.macro-" (symbol-name (nth 2 in))))))
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


;; append-for

(expect "$(filter %,$(foreach x&,1 2 3,$(call ^u,$(x&))))"
        (CX "(append-for x \"1 2 3\" x)"))

;; concat-for

(expect "$(subst |1,|,$(subst |.,,$(subst |. ,$(subst |,|1,D),$(foreach x&,a b,$(subst |,|1,$(call ^u,$(x&)))|.))))"
        (CX "(concat-for x \"a b\" \"D\" x)"))

(expect "$(foreach x&,a b,$(call ^u,$(x&)))"
        (CX "(concat-for x \"a b\" \" \" x)"))

;; cond

(expect 1 (macrotest
           '(cond (a b) (c d))
           '(if a b (if c d))))

(expect 1 (macrotest
           '(cond (a b) (c d e) (else f g))
           '(if a b (if c (begin d e) (begin f g)))))

(print "compile ok")


;; global-name


(let ((form1 ["L" ["S" "local-to-global"] ["Q" "X"]])
      (form2 ["L" ["S" "global-name"] ["S" "A"]])
      (form3 ["L" ["S" "global-name"] ["S" "UNDEF"]])
      (form4 ["L" ["S" "global-name"] ["L"]])
      (env (hash-bind "A" ["V" "ns~A" "" ""])))

  (expect (Concat [ (String (gen-global-name "")) (String "X") ])
          (ml.special-local-to-global form1 env))

  (expect (String "ns~A")
          (ml.special-global-name form2 env))

  (expect ["E." "\"UNDEF\" is not a global variable"]
          (ml.special-global-name form3 env))

  (expect 1 (see "invalid"
               (ml.special-global-name form4 env))))

;; defmacro

(declare SCAM_NS &global)

(let-global ((SCAM_NS "_"))
  (let ((o (ml.special-defmacro `(defmacro (foo form env) "Q hello")
                                (hash-bind "a" "B c")
                                1)))
    (define `env (nth 2 o))
    (expect "env" (first o))
    (expect (hash-get "foo" env) (EXMacro "_foo" nil))
    (expect (hash-get "a" env) "B c")
    (expect (word 1 (Builtin "X" []))
            (word 1 (nth-rest 3 o)))))

;; (use STRING)

(define *use-test* nil)
(let-global ((^require (lambda (s) (set *use-test* s)))
             (require-module (lambda (name priv)
                                   (hash-bind "name" ["X" "name" "i"]))))
  (define `env (hash-bind "x" "M abc"))
  (expect "Q" (ml.special-use `(use "foo")))
  (expect ["env" (hash-bind "name" ["X" "name" "i"] env) "Q"]
          (ml.special-use `(use "foo") env 1))
  (expect "foo" *use-test*)
  (print "ok"))


;; ml.special-data

(expect (DataType "T1" "Ctor" "W S L" "a b c")
        (get-type `(Ctor &word a b &list c) "T1" ["L.1"]))

(expect (DataType "X" "Ctor" "W S L" "a b c")
        (get-type `(Ctor "X" &word a b &list c) "T1" ["L.1"]))

;; list not at end => use S, not L encoding
(expect (DataType "T1" "Ctor" "W S S" "a b c")
        (get-type `(Ctor &word a &list b c) "T1" ["L.1"]))

(expect [ (DataType "!:T0" "Ctor" "W S L" "a b c")
          (DataType "!:T1" "CtorB" "S" "a") ]
        (get-types `(data) "!:T" [ `(Ctor &word a b &list c)
                                   `(CtorB a) ] nil))

(let ((o (ml.special-data `(data T
                                 (CA a &word b &list c)
                                 (CB))
                          (hash-bind "env" "old")
                          1)))

  (expect (ERecord "S W L" "." "!:T0")
          (hash-get "CA" (nth 2 o))))


;; ml.special-case

(expect (append
         (hash-bind "a" (EIL (Builtin "word" [ (String 2) (String 123) ]) "."))
         (hash-bind "b" (EIL (Call "^n" [ (String 3) (String 123) ]) ".")))
        (arg-bindings [ ["S" "a"] ["S" "b"] ]
                      "W S"
                      "Q 123"))

(expect
 (Builtin "if" [ (Builtin "filter" [ (String "!:T0")
                                     (Builtin "firstword" [ (String 1) ]) ])
                 (Builtin "wordlist" [ (String 4) (String 99999999) (String 1) ])
                 (String 1)])

 (ml.special-case (p1 "(case 1 ((Ctor s w v) v) (a a))")
                  (hash-bind "Ctor"
                             (ERecord "S W L" "." "!:T0"))))
