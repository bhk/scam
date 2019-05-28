;;--------------------------------------------------------------
;; Tests for compile.scm
;;--------------------------------------------------------------

(require "core.scm")
(require "parse.scm")
(require "gen.scm")
(require "gen0.scm")
(require "gen-testutils.scm")
(require "macros.scm" &private)

;; symbol to use for the `sym` argument for macros (or `parent` node)
(define `macro-sym (PSymbol 0 "MACRO"))


;;--------------------------------------------------------------
;; tests
;;--------------------------------------------------------------

;; read-pairs

(define `(test-read-pairs text-pairs)
  (read-pairs (p1 text-pairs) macro-sym "WHERE"))

(expect [ [ (PSymbol 3 "x") (PString 5 1) ]
          [ (PSymbol 9 "y") (PString 11 2) ] ]
        (test-read-pairs "((x 1) (y 2))"))
(expect (PError 3 "invalid VAR in WHERE; expected a symbol")
        (test-read-pairs "((1 1) (y 2))"))
(expect (PError 2 "missing VALUE in WHERE")
        (test-read-pairs "((x) (y 2))"))
(expect (PError 2 "invalid (VAR VALUE) in WHERE; expected a list")
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
        "!(PError 2 '`set` accepts 2 or 3 arguments, not 4')")
(expect (c0-ser "(set 1 2 2)")
        "!(PError 4 'invalid NAME in (set NAME VALUE [RETVAL]); expected a symbol')")

;;--------------------------------
;; (? <fn> ...args...)
;;--------------------------------

(expect (c0-ser "(? f 1)")
        "(^t F,1)")
(expect (c0-ser "(? m a)"
                { m: (EMacro "p" "." 1 (IArg 0 ".")) })
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

(expect (let&-env [ [ (PSymbol 0 "x") (PSymbol 0 "X") ]
                    [ (PSymbol 0 "y") (PSymbol 0 "x") ] ]
                  (append
                   { X: (EVar "p" "xname") }
                   (depth-marker ".")))
        (append
         { y: (EIL "p" "." (IVar "xname")) }
         { x: (EIL "p" "." (IVar "xname")) }
         { X: (EVar "p" "xname") }
         (depth-marker ".")))

(expect "1" (c0-ser "(let& ((a 1)) a)"))
(expect "2" (c0-ser "(let& ((a 1) (b 2)) b)"))
(expect "3" (c0-ser "(let& ((a 1) (b 2) (a 3)) a)"))

;; capture in sym macro value
(c0-ser "(foreach n 1 (let& ((m n)) (concat m (lambda () m))))"
        "(.foreach \"0,1,{\"0}`{.n\"0})")
(c0-ser "(let& ((m (foreach n,1,n))) (concat m (lambda () m))))"
        "(.foreach \"0,1,{\"0})`(.foreach \"0,1,{\"0})")

;;--------------------------------
;; (foreach VAR LIST BODY)
;;--------------------------------

(expect (c0-ser "(foreach v \"1 2 3\" v)" "-")
        "(.foreach ;,1 2 3,{;})")
(expect (c0-ser "(foreach x \"1 2 3\" (foreach y 4 (concat x y)))")
        "(.foreach ;,1 2 3,(.foreach ;;,4,{;}{;;}))")
(expect (c0-ser "(lambda (a) (define `m a) m)")
        "`{1}")
(expect (c0-ser "(begin (define `m (foreach v \"1 2 3\" v)) m)")
        "(.foreach ;,1 2 3,{;})")
(expect (c0-ser "(foreach N 3 (lambda () N))")
        "(.foreach ;,3,`{.;})")
(expect (c0-ser "(begin (define `m (foreach v \"1 2 3\" v)) (lambda () m))")
        "`(.foreach ;,1 2 3,{;})")
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
        "(.foreach ;,1 2 3,(^d (.and (^u {;}))))")

;;--------------------------------
;; (append-for VAR VEC BODY)
;;--------------------------------

(expect (c0-ser "(append-for x \"1 2 3\" x)")
        "(.filter %,(.foreach ;,1 2 3,(^u {;})))")

;;--------------------------------
;; (concat-for VAR VEC DELIM BODY)
;;--------------------------------

(expect "aBc"
        (il-ser (il-subst "b" "B" (IString "abc"))))
(expect "(.subst b,B,{V})"
        (il-ser (il-subst "b" "B" (IVar "V"))))

;; delim == " "
(expect "(.foreach ;,a b,(^u {;}))"
        (c0-ser "(concat-for x \"a b\" \" \" x)" { d: (EVar "p" "D") }))

;; delim == IString
(expect "(.subst |1,|,(.subst |0, ,(.subst  ,|1,(.foreach ;,a b,(.subst  ,|0,(.subst |,|1,(^u {;})))))))"
        (c0-ser "(concat-for x \"a b\" \"|\" x)" { d: (EVar "p" "D" ) }))

;; general case
(expect "(.subst |1,|,(.subst |0, ,(.subst  ,(.subst |,|1,{D}),(.foreach ;,a b,(.subst  ,|0,(.subst |,|1,(^u {;})))))))"
        (c0-ser "(concat-for x \"a b\" d x)" { d: (EVar "p" "D") }))

;;--------------------------------
;; (cond (TEST BODY)...)
;;--------------------------------

(expect (c0-ser "(cond (a 1) (2 f))")
        (c0-ser "(if a 1 (if 2 f))"))
(expect (c0-ser "(cond (a v 1) (else f))")
        (c0-ser "(if a (begin v 1) f)"))
(expect (c0-ser "(cond (nil))")
        "!(PError 4 'missing BODY in (cond (TEST BODY)...)')")
(expect (c0-ser "(cond ())")
        "!(PError 4 'missing TEST in (cond (TEST BODY)...)')")


;;--------------------------------
;; (native-name SYM)
;;--------------------------------

(expect "V"
        (c0-ser "(native-name v)"))
(expect "!(PError 4 '`a` is not a global variable')"
        (c0-ser "(native-name a)"))
(expect "!(PError 4 'invalid NAME in (native-name NAME); expected a symbol')"
        (c0-ser "(native-name 1)"))
(expect "!(PError 2 '`native-name` accepts 1 argument, not 2')"
        (c0-ser "(native-name v v)"))

;;--------------------------------
;; (defmacro (NAME ARG...) BODY)
;;--------------------------------

(let ((out (c0 (p1 "(defmacro (foo a) a)") nil)))
  (case out
    ((IEnv env il)
     (fexpect env
              (xns { foo: (EXMacro "x" "~foo") }))
     (expect (il-ser (case il
                       ((IEnv _ node) node)
                       (else il)))
             (xns "(^fset ~foo,`{1})")))
    (else
     ;; not an ILEnv
     (expect 1 0))))

(p1-block-cc
 "(defmacro (foo a) a)"
 (lambda (env sil)
   (expect sil (xns "(^fset ~foo,`{1})"))
   (expect env (xns { foo: (EXMacro "x" "~foo") }))))


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
   (expect (dict-get "CA" env)
           (ERecord "p" "S W L" "!:T0"))
   ;; ^at is &native
   (expect sil
           "(^at !1:T0!=CA!0S!0W!0L !1:T1!=CB)")))

(p1-block-cc
 "(data T &public (CA a &word b &list c))"
 (lambda (env sil)
   (expect (dict-get "CA" env)
           (ERecord "x" "S W L" "!:T0"))))


;;--------------------------------
;; (case VALUE (PATTERN BODY)... )
;;--------------------------------

(define tc-node (IArg 1 ".."))
(define tc-defn (EIL "p" ".." (IArg 1 "..")))

(expect (extract-member tc-defn 1 "S")
        (EIL "p" ".." (ICall "^n" [(IString "2") tc-node])))

(expect (member-bindings [(PSymbol 0 "a!") (PSymbol 0 "b") (PSymbol 0 "c")]
                         ["S" "W" "L"]
                         tc-defn)
        { a!: (EIL "p" ".." (ICall "^n" [(IString "2") tc-node])),
          b: (EIL "p" ".." (IBuiltin "word" [(IString "3") tc-node])),
          c: (EIL "p" ".." (IBuiltin "wordlist" [(IString "4")
                                                 (IString 99999999)
                                                 tc-node]))})

;; clauses-merge

(define `(check-merge pvs)
  (define `clauses
    (for pv pvs
         (if (word 2 pv)
             (IBuiltin "if" [ (IBuiltin "filter" [(IString (first pv))
                                                  (IArg 1 ".")])
                              (IString (nth 2 pv)) ])
             (IString pv))))

  (for il (clauses-merge clauses)
       (il-ser il)))


(expect (check-merge [ "a X"])
        ["(.if (.filter a,{1}),X)"])

(expect (check-merge [ "a X" "b X"])
        ["(.if (.filter a b,{1}),X)"])

(expect (check-merge [ "a X" "b X" "c X"])
        ["(.if (.filter a b c,{1}),X)"])

(expect (check-merge [ "a X" "b Y" 1])
        ["(.if (.filter a,{1}),X)"
         "(.if (.filter b,{1}),Y)"
         "1"])

;; c0-case

;; error
(expect (c0-ser "(case)")
        "!(PError 2 'missing VALUE in (case VALUE (PATTERN BODY)...)')")

(define tc-env
  (append { C: (ERecord "p" "S W L" "!:T0") }
          { D: (ERecord "p" "S W" "!:T1") }
          { F: (EFunc "p" "F" 0) }
          default-env))

;; single case
(expect (c0-ser "(case v ((C s w v) v))" tc-env)
        "(.if (.filter !:T0,(.word 1,{V})),(.wordlist 4,99999999,{V}))")

;; multiple cases
(expect (c0-ser "(case v ((C s w l) l) (a a))" tc-env)
        "(.if (.filter !:T0,(.word 1,{V})),(.wordlist 4,99999999,{V}),{V})")

;; non-ctor in pattern
(expect (c0-ser "(case v ((Foo a) 1))")
        "!(PError 8 'symbol `Foo` is not a record constructor')")

;; bad value expr
(expect (c0-ser "(case bogus)")
        "!(PError 4 'undefined variable: `bogus`')")

;; wrong number of arguments
(expect (c0-ser "(case v ((C s l) l))" tc-env)
        "!(PError 8 '`C` accepts 3 arguments, not 2')")

;; captures in value
(expect (c0-ser "(lambda (v) (case v ((C a b c) a)))" tc-env)
        "`(.if (.filter !:T0,(.word 1,{1})),(^n 2,{1}))")
(expect (c0-ser "(lambda (v) (case v ((C a b c) (lambda () a))))" tc-env)
        "`(.if (.filter !:T0,(.word 1,{1})),`(^n 2,{.1}))")
(expect (c0-ser "(foreach v 1 (case v ((C a b c) (lambda () a))))" tc-env)
        "(.foreach ;,1,(.if (.filter !:T0,(.word 1,{;})),`(^n 2,{.;})))")

;; complex value => use `foreach`
(expect (c0-ser "(case (F) ((C a b c) (lambda () a)))" tc-env)
        (concat "(.foreach ;,(^d (F )),(.if (.filter !1:T0!0%,{;}!0),"
                "`(^n 2,(^u {.;}))))"))

;; nested complex value => generate unique auto var
(expect (c0-ser "(case (F) ((C a b c) (case (F) (else 1))))" tc-env)
        (concat "(.foreach ;,(^d (F )),(.if (.filter !1:T0!0%,{;}!0),"
                "(.foreach ;;,(^d (F )),1)))"))

;; collapse clauses with equivalent bodies
(expect (c0-ser "(case v ((C a b c) b) ((D a b) b) (a a))" tc-env)
        "(.if (.filter !:T0 !:T1,(.word 1,{V})),(.word 3,{V}),{V})")
