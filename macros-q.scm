;;--------------------------------------------------------------
;; Tests for compile.scm
;;--------------------------------------------------------------

(require "gen-testutils")
(require "macros" &private)

;; symbol to use for the `sym` argument for macros (or `parent` node)
(define `macro-sym (PSymbol 0 "MACRO"))


;;--------------------------------------------------------------
;; tests
;;--------------------------------------------------------------

;; read-pairs

(define `(test-read-pairs text-pairs)
  (read-pairs (form-set-indices 0 (p1 text-pairs)) macro-sym "WHERE"))

(expect [ [ (PSymbol 0 "x") (PString 0 1) ]
          [ (PSymbol 0 "y") (PString 0 2) ] ]
        (test-read-pairs "((x 1) (y 2))"))
(expect (PError 0 "invalid VAR in WHERE; expected a symbol")
        (test-read-pairs "((1 1) (y 2))"))
(expect (PError 0 "missing VALUE in WHERE")
        (test-read-pairs "((x) (y 2))"))
(expect (PError 0 "invalid (VAR VALUE) in WHERE; expected a list")
        (test-read-pairs "(sym (x 1))"))

;;--------------------------------
;; (print args...)
;;--------------------------------

(expect "(.info )" (c0-ser "(print)"))
(expect "(.info 1)" (c0-ser "(print 1)"))
(expect "(.info 123)" (c0-ser "(print 1 2 3)"))

;;--------------------------------
;; (current-env)
;;--------------------------------

(expect default-env
        (c0-ser "(current-env)"))

;;--------------------------------
;; (concat FORM...)
;;--------------------------------

(expect "ab(.or 1)" (c0-ser "(concat \"a\" \"b\" (or 1))"))

(expect "`{1}(.call ^n,1,{9})"
        (c0-ser "(lambda (a b c d e f g h i) (concat a i))"))

;;--------------------------------
;; (vector FORM...)
;;--------------------------------

(expect "1 2" (c0-ser "(vector 1 2)"))
(expect ["a" "b c" "d"] (c0-ser "(vector \"a\" \"b c\" \"d\")"))
(expect "(^d (.or 1))" (c0-ser "(vector (or 1))"))

;;--------------------------------
;; (subst FROM TO {FROM TO}... STR)
;;--------------------------------

(expect (c0-ser "(subst 1 2 v)")
        "(.subst 1,2,{V})")
(expect (c0-ser "(subst 1 2 3 4 v)")
        "(.subst 3,4,(.subst 1,2,{V}))")
(expect (c0-ser "(subst 1 2 3 a)")
        "!(PError 2 '(subst {FROM TO}+ STR) accepts 2n+1 arguments, not 4')")

;;--------------------------------
;; (set SYM VALUE [RETVAL])
;;--------------------------------

(expect (c0-ser "(set v 1)")
        "(^set V,1)")
(expect (c0-ser "(set f 1)")
        "(^fset F,1)")
(expect (c0-ser "(set f 1 2)")
        "(^fset F,1,2)")
(expect (c0-ser "(set f 1 2 2)")
        "!(PError 2 '\\'set\\' accepts 2 or 3 arguments, not 4')")
(expect (c0-ser "(set 1 2 2)")
        "!(PError 4 'invalid NAME in (set NAME VALUE [RETVAL]); expected a symbol')")

;;--------------------------------
;; (? <fn> ...args...)
;;--------------------------------

(expect (c0-ser "(? f 1)")
        "(^t F,1)")
(expect (c0-ser "(? m a)" (hash-bind "m" (EFunc NoGlobalName "." 1 nil)))
        "!(PError 4 'FUNC in (? FUNC ...) is not traceable')")

;;--------------------------------
;; (let ((VAR VAL)...) BODY)
;;--------------------------------

(expect (c0-ser "(let ((a 1) (b \"q\")) (f a b))")
        (c0-ser "((lambda (a b) (f a b)) 1 \"q\")"))
(expect (c0-ser "(let a a)")
        (concat
         "!(PError 4 'invalid ((VAR VALUE)...) in (let ((VAR VALUE)...) BODY)"
         "; expected a list')"))
(expect (c0-ser "(let (a) a)")
        (concat
         "!(PError 5 'invalid (VAR VALUE) in (let ((VAR VALUE)...) BODY)"
         "; expected a list')"))
(expect (c0-ser "(let ((\"a\")) a)")
        (concat
         "!(PError 6 'invalid VAR in (let ((VAR VALUE)...) BODY)"
         "; expected a symbol')"))
(expect (c0-ser "(let ((a)) a)")
        "!(PError 5 'missing VALUE in (let ((VAR VALUE)...) BODY)')")

;;--------------------------------
;; (let-global ((VAR VALUE)...) BODY)
;;--------------------------------

(expect (c0-ser "(let-global ((v 1)) f)")
        "(^set V,(^set V,1,{V}),(.value F))")
(expect (c0-ser "(let-global ((v 1) (f 2)) 9)")
        "(^set V,(^set V,1,{V}),(^fset F,(^fset F,2,(.value F)),9))")

;;--------------------------------
;; (let& ((VAR VAL)...) BODY)
;;--------------------------------

(expect (let&-env [ [ (PSymbol 0 "x") (PString 0 1) ]
                    [ (PSymbol 0 "y") (PSymbol 0 "Y") ] ]
                  (hash-bind "Y" (EVar "yname" "x"))
                  ".")
        (append (hash-bind "y" (EIL "." "-" (IVar "yname")))
                (hash-bind "x" (EIL "." "-" (IString 1)))
                (hash-bind "Y" (EVar "yname" "x"))))

(expect "1" (c0-ser "(let& ((a 1)) a)"))
(expect "2" (c0-ser "(let& ((a 1) (b 2)) b)"))
(expect "3" (c0-ser "(let& ((a 1) (b 2) (a 3)) a)"))

;;--------------------------------
;; (foreach VAR LIST BODY)
;;--------------------------------

(expect (c0-ser "(foreach v \"1 2 3\" v)")
        "(.foreach v,1 2 3,{v})")
(expect (c0-ser "(foreach a b)")
        "!(PError 2 'missing BODY in (foreach VAR LIST BODY)')")
(expect (c0-ser "(foreach a)")
        "!(PError 2 'missing LIST in (foreach VAR LIST BODY)')")
(expect (c0-ser "(foreach)")
        (concat
         "!(PError 2 'missing VAR in (foreach VAR LIST BODY)"
         "; expected a symbol')"))

;;--------------------------------
;; (for VAR VEC BODY)
;;--------------------------------

(expect (c0-ser "(for x \"1 2 3\" (and x))")
        "(.foreach x,1 2 3,(^d (.and (^u {x}))))")

;;--------------------------------
;; (append-for VAR VEC BODY)
;;--------------------------------

(expect (c0-ser "(append-for x \"1 2 3\" x)")
        "(.filter %,(.foreach x,1 2 3,(^u {x})))")

;;--------------------------------
;; (concat-for VAR VEC DELIM BODY)
;;--------------------------------

(expect "aBc"
        (il-ser (il-subst "b" "B" (IString "abc"))))
(expect "(.subst b,B,{V})"
        (il-ser (il-subst "b" "B" (IVar "V"))))

;; delim == " "
(expect "(.foreach x,a b,(^u {x}))"
        (c0-ser "(concat-for x \"a b\" \" \" x)"))

;; delim == IString
(expect "(.subst |1,|,(.subst |0, ,(.subst  ,|1,(.foreach x,a b,(.subst  ,|0,(.subst |,|1,(^u {x})))))))"
        (c0-ser "(concat-for x \"a b\" \"|\" x)"))

;; general case
(expect "(.subst |1,|,(.subst |0, ,(.subst  ,(.subst |,|1,{D}),(.foreach x,a b,(.subst  ,|0,(.subst |,|1,(^u {x})))))))"
        (c0-ser "(concat-for x \"a b\" d x)"
            (hash-bind "d" (EVar "D" nil))))

;;--------------------------------
;; (cond (TEST BODY)...)
;;--------------------------------

(expect (c0-ser "(cond (a 1) (2 f))")
        (c0-ser "(if a 1 (if 2 f))"))
(expect (c0-ser "(cond (a v 1) (else f))")
        (c0-ser "(if a (begin v 1) f)"))

;;--------------------------------
;; (global-name SYM)
;;--------------------------------

(expect "V"
        (c0-ser "(global-name v)"))
(expect "!(PError 4 '\\'a\\' is not a global variable')"
        (c0-ser "(global-name a)"))
(expect "!(PError 4 'invalid NAME in (global-name NAME); expected a symbol')"
        (c0-ser "(global-name 1)"))
(expect "!(PError 2 '\\'global-name\\' accepts 1 argument, not 2')"
        (c0-ser "(global-name v v)"))

;;--------------------------------
;; (defmacro (NAME ARG...) BODY)
;;--------------------------------

(let ((out (c0 (p1 "(defmacro (foo a) a)") nil 1)))
  (case out
    ((IEnv env il)
     (fexpect env
              (xns (hash-bind "foo" (EXMacro "~foo" "x"))))
     (expect (il-ser il)
             (xns "(^fset ~foo,`{1})")))
    (else
     ;; not an ILEnv
     (expect 1 0))))

(p1-block-cc
 "(defmacro (foo a) a)"
 (lambda (env sil)
   (expect sil (xns "(^fset ~foo,`{1})"))
   (expect env (xns (hash-bind "foo" (EXMacro "~foo" "x"))))))


;;--------------------------------
;; (use MODULE)
;;--------------------------------

(define *use-test* nil)
(let-global
 ;; harness
 ((use-module
   (lambda (mod-name)
     (set *use-test* mod-name)
     (hash-bind "name" (EXMacro "name" "i")))))

 (expect (c0-ser "(use \"x\" \"y\")")
         "!(PError 2 '\\'use\\' accepts 1 argument, not 2')")
 (expect (c0-ser "(use a)")
         "!(PError 4 'invalid MODULE in (use MODULE); expected a literal string')")

 (p1-block-cc
  "(use \"MOD\")"
  (lambda (env sil)
    (expect env (hash-bind "name" (EXMacro "name" "i")))
    (expect sil "")
    (expect *use-test* "MOD"))))


;;--------------------------------
;; (data NAME CTOR...)
;;--------------------------------

;; read-type
(expect (read-type (p1 "(Ctor &word a b &list c)") "T1" macro-sym)
        (DataType "T1" "Ctor" "W S L" "a b c"))
(expect (DataType "X" "Ctor" "W S L" "a b c")
        (read-type (p1 "(Ctor \"X\" &word a b &list c)") "T1" macro-sym))
;; ... &list not at end => use S, not L encoding
(expect (read-type (p1 "(Ctor &word a &list b c)") "T1" macro-sym)
        (DataType "T1" "Ctor" "W S S" "a b c"))
(expect (read-type (p1 "(Ctor &word)") "T1" macro-sym)
        (PError 1 "no argument following last flag: &word"))
(expect (read-type (p1 "(Ctor &word &list a)") "T1" macro-sym)
        (PError 6 "two type flags supplied for one argument"))
(expect (read-type (p1 "(Ctor &blarg a)") "T1" macro-sym)
        (PError 4 "unknown flag [supported: &list, &word]"))
(expect (read-type (p1 "(Ctor &word 1)") "T1" macro-sym)
        (PError 6 "invalid ARG in (data NAME (CTOR ARG...)...); expected a symbol"))
(expect (read-type (p1 "(1 &word 1)") "T1" macro-sym)
        (PError 2 "invalid CTOR in (data NAME (CTOR ARG...)...); expected a symbol"))

;; read-types
;; ... two success cases
(expect (read-types macro-sym "!:T"
                    (pN "(Ctor &word a b &list c) (CtorB a)")
                    nil nil nil)
        [ (DataType "!:T0" "Ctor" "W S L" "a b c")
          (DataType "!:T1" "CtorB" "S" "a") ])
;; ... error handled in read-types
(expect (read-types macro-sym "!:T"
                    (pN "(\"Ctor\" a)")
                    nil nil nil)
        (PError 0 "invalid CTOR in (data NAME (CTOR ARG...)...); expected a symbol"))
;; ... error returned from read-type
(expect (read-types macro-sym "!:T"
                    (pN "(1 2)")
                    nil nil nil)
        (PError 0 "invalid CTOR in (data NAME (CTOR ARG...)...); expected a symbol"))

;; ml.special-data
(expect (c0-ser "(data 1 (X))")
        "!(PError 4 'invalid NAME in (data NAME (CTOR ARG...)...); expected a symbol')")
;; ... cascaded error
(expect (c0-ser "(data Foo (1))")
        "!(PError 7 'invalid CTOR in (data NAME (CTOR ARG...)...); expected a symbol')")
;; ... success
(p1-block-cc
 "(data T (CA a &word b &list c) (CB))"
 (lambda (env sil)
   (expect (hash-get "CA" env)
           (ERecord "S W L" "p" "!:T0"))
   ;; ^add-tags is &global
   (expect sil
           "(^add-tags !1:T0!=CA!0S!0W!0L !1:T1!=CB)")))

(p1-block-cc
 "(data T &public (CA a &word b &list c))"
 (lambda (env sil)
   (expect (hash-get "CA" env)
           (ERecord "S W L" "x" "!:T0"))))


;;--------------------------------
;; (case VALUE (PATTERN BODY)... )
;;--------------------------------

(expect
 (arg-bindings [ (PSymbol 0 "a") (PSymbol 0 "b") ]
               "W S"
               (IString 123)
               ".")
 (append
  (hash-bind "a" (EIL "." "-" (IBuiltin "word" [ (IString 2) (IString 123) ])))
  (hash-bind "b" (EIL "." "-" (ICall "^n" [ (IString 3) (IString 123) ])))))

;; single case
(expect (c0-ser "(case v ((Ctor s w v) v))"
               (hash-bind "Ctor" (ERecord "S W L" "." "!:T0")
                          default-env))
        "(.if (.filter !:T0,(.firstword {V})),(.wordlist 4,99999999,{V}))")

;; multiple cases
(expect (c0-ser "(case v ((Ctor s w l) l) (a a))"
                (hash-bind "Ctor" (ERecord "S W L" "." "!:T0")
                          default-env))
        "(.if (.filter !:T0,(.firstword {V})),(.wordlist 4,99999999,{V}),{V})")

;; non-ctor in pattern
(expect (c0-ser "(case v ((Foo a) 1))")
        "!(PError 8 'symbol \\'Foo\\' does not identify a record type')")

;; bad value expr
(expect (c0-ser "(case bogus)")
        "!(PError 4 'undefined variable \\'bogus\\'')")

;; wrong number of arguments
(expect (c0-ser "(case v ((Ctor s l) l))"
                (hash-bind "Ctor" (ERecord "S W L" "." "!:T0")
                           default-env))
        "!(PError 8 '\\'Ctor\\' accepts 3 arguments, not 2')")
