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
(expect "ab(.or 1)" (c0-ser "(.. \"a\" \"b\" (or 1))"))
(expect "a b (.or 1)" (c0-ser "(._. \"a\" \"b\" (or 1))"))

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
;; (let ((TARGET VAL)...) BODY)
;;--------------------------------

(expect (c0-ser "(let ((a 1) (b \"q\")) (f a b))")
        (c0-ser "((lambda (a b) (f a b)) 1 \"q\")"))
(expect (c0-ser "(let (([a b] 9)) (.. a b))")
        "(^Y `(^n 1,{1})(^n 2,{1}),9)")
(expect (c0-ser "(let a a)")
        "!(PError 4 'expected ((TARGET VALUE)...) in (let ((TARGET VALUE)...) BODY)')")
(expect (c0-ser "(let (a) a)")
        "!(PError 5 'expected (TARGET VALUE) in (let ((TARGET VALUE)...) BODY)')")
(expect (c0-ser "(let ((a)) a)")
        "!(PError 5 'missing VALUE in (let ((TARGET VALUE)...) BODY)')")
(expect 1 (see "!(PError 6 'invalid assignment target"
               (c0-ser "(let ((\"a\" 1)) a)")))
(expect 1 (see "!(PError 6 ''?NAME' and '...NAME' cannot be used"
               (c0-ser "(let ((?a 1)) a)")))
(expect 1 (see "!(PError 6 ''?NAME' and '...NAME' cannot be used"
               (c0-ser "(let ((...a 1)) a)")))

;;--------------------------------
;; (let-global ((VAR VALUE)...) BODY)
;;--------------------------------

(expect (c0-ser "(let-global ((v 1)) f)")
        "(^set V,(^set V,1,{V}),(.value F))")
(expect (c0-ser "(let-global ((v 1) (f 2)) 9)")
        "(^set V,(^set V,1,{V}),(^fset F,(^fset F,2,(.value F)),9))")
(expect (c0-ser "(let-global (([v] 1) (f 2)) 9)")
        "(^set V,(^set V,(^n 1,1),{V}),(^fset F,(^fset F,2,(.value F)),9))")

(expect (c0-ser "(let-global (([x y] 5) (f 2)) a)"
                {x: (EVar "p" "X"),
                 y: (EVar "p" "Y"),
                 f: (EFunc "p" "F" 1),
                 a: (EDefn.arg 1 ".")})
        (.. "(^Y `(^set X,(^set X,(^n 1,{1}),{X}),"
            "(^set Y,(^set Y,(^n 2,{1}),{Y}),"
            "(^fset F,(^fset F,2,(.value F)),{.1}))),5)"))

;;--------------------------------
;; (let& ((TARGET VAL)...) BODY)
;;--------------------------------

(expect "1" (c0-ser "(let& ((a 1)) a)"))
(expect "2" (c0-ser "(let& ((a 1) (b 2)) b)"))
(expect "3" (c0-ser "(let& ((a 1) (b 2) (a 3)) a)"))
(expect "(^n 2,x y z)" (c0-ser "(let& (([a b] \"x y z\")) b)"))

;;--------------------------------
;; (foreach (VAR LIST DELIM?) BODY)
;;--------------------------------

(expect (c0-ser "(foreach (z 1) z)" nil)
        "(.foreach ;,1,{;})")

(expect (c0-ser "(foreach (z 1 \" \") z)" nil)
        "(.foreach ;,1,{;})")

(expect (.. "(.subst |~,,(.subst |~ ,(.subst |~,||~~,{G}),"
            "(.foreach ;,1,(.subst |~,||~~,{;})|~)))")
        (c0-ser "(foreach (z 1 g) z)" {g: (EVar "p" "G")}))

(expect 1 (see "!(PError 2 'missing TARGET in (foreach (TARGET LIST ?DELIM) BODY)"
               (c0-ser "(foreach ())")))

(expect 1 (see "!(PError 2 'missing LIST in (foreach (TARGET LIST ?DELIM) BODY)')"
               (c0-ser "(foreach (a))")))

(expect 1 (see "!(PError 2 'missing BODY in (foreach (TARGET"
               (c0-ser "(foreach (a 1))")))

;; destructuring
(expect (c0-ser "(foreach {=a:b} \"1 2 3\" (f a b))")
        "(.foreach ;,1 2 3,(F (^dk {;}),(^dv {;})))")
(expect (c0-ser "(foreach {key:a} \"1 2 3\" (f a \"\"))")
        "(.foreach ;,1 2 3,(F (^dv (.filter key!=%,{;})),))")
(expect (c0-ser "(foreach [a b] \"1 2 3\" (f a b))")
        "(.foreach ;,1 2 3,(F (^n 1,{;}),(^n 2,{;})))")

;;--------------------------------
;; old: (foreach VAR LIST BODY)
;;--------------------------------

(expect (c0-ser "(foreach v \"1 2 3\" v)" "-")
        "(.foreach ;,1 2 3,{;})")
(expect (c0-ser "(foreach x \"1 2 3\" (foreach y 4 (f x y)))")
        "(.foreach ;,1 2 3,(.foreach ;;,4,(F {;},{;;})))")
(expect (c0-ser "(lambda (a) (define `m a) m)")
        "`{1}")
(expect (c0-ser "(begin (define `m (foreach v \"1 2 3\" v)) m)")
        "(.foreach ;,1 2 3,{;})")
(expect (c0-ser "(foreach N 3 (lambda () N))")
        "(.foreach ;,3,`{.;})")
(expect (c0-ser "(begin (define `m (foreach v \"1 2 3\" v)) (lambda () m))")
        "`(.foreach ;,1 2 3,{;})")
(expect (c0-ser "(foreach a b)")
        "!(PError 2 'missing BODY in (foreach TARGET LIST BODY)')")
(expect (c0-ser "(foreach a)")
        "!(PError 2 'missing LIST in (foreach TARGET LIST BODY)')")
(expect (c0-ser "(foreach)")
        (.. "!(PError 2 'missing TARGET in (foreach TARGET LIST BODY)"
            "; expected a symbol')"))

;;--------------------------------
;; (for (VAR VEC) BODY)
;;--------------------------------

(expect (c0-ser "(for (x \"1 2 3\") (and x))")
        "(.foreach ;,1 2 3,(^d (.and (^u {;}))))")

;;--------------------------------
;; old: (for VAR VEC BODY)
;;--------------------------------

(expect (c0-ser "(for x \"1 2 3\" (and x))")
        "(.foreach ;,1 2 3,(^d (.and (^u {;}))))")

;;--------------------------------
;; (append-for (VAR VEC) BODY)
;;--------------------------------

(expect (c0-ser "(append-for (x \"1 2 3\") x)")
        "(.filter %,(.foreach ;,1 2 3,(^u {;})))")

;;--------------------------------
;; old: (append-for VAR VEC BODY)
;;--------------------------------

(expect (c0-ser "(append-for x \"1 2 3\" x)")
        "(.filter %,(.foreach ;,1 2 3,(^u {;})))")

;;--------------------------------
;; (concat-for (VAR VEC ?DELIM) BODY)
;;--------------------------------

;; delim == " "
(expect (c0-ser "(concat-for (x \"a b\" \" \") x)" { d: (EVar "p" "D") })
        "(.foreach ;,a b,(^u {;}))")


;; delim == IString   (that contains "~x")
(expect (c0-ser "(concat-for (x \"a b\" \"|~\") x)" { d: (EVar "p" "D" ) })
        (.. "(.subst |~,,(.subst |~ ,||~~,"
            "(.foreach ;,a b,(.subst |~,||~~,(^u {;}))|~)))"))

;; general case
(expect (.. "(.subst |~,,(.subst |~ ,(.subst |~,||~~,{D}),"
            "(.foreach ;,a b,(.subst |~,||~~,(^u {;}))|~)))")
        (c0-ser "(concat-for (x \"a b\" d) x)" { d: (EVar "p" "D") }))

;;--------------------------------
;; old: (concat-for VAR VEC DELIM BODY)
;;--------------------------------

;; delim == " "
(expect "(.foreach ;,a b,(^u {;}))"
        (c0-ser "(concat-for x \"a b\" \" \" x)" { d: (EVar "p" "D") }))

;; general case
(expect (.. "(.subst |~,,(.subst |~ ,(.subst |~,||~~,{D}),"
            "(.foreach ;,a b,(.subst |~,||~~,(^u {;}))|~)))")
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

(case (c0 (p1 "(defmacro (foo a) a)") nil)
  ((IEnv env il)
   (fexpect env
            (xns { foo: (EXMacro "x" "~foo") }))
   (expect (il-ser (case il
                     ((IEnv _ node) node)
                     (else il)))
           (xns "(^fset ~foo,`{1})")))
  (else
   ;; not an ILEnv
   (expect 1 0)))

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

;; M.data
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


(define _a (PSymbol 1 "a"))
(define _x (PSymbol 5 "x"))
(define _y (PSymbol 6 "y"))
(define _?y (PSymbol 16 "?y"))
(define Ctor1-tag "!:C1")
(define Ctor1-encs "W L")
(define Ctor1-record  (ERecord "p" Ctor1-encs Ctor1-tag))
(define _Ctor1        (PSymbol 101 "Ctor1"))
(define _Ctor1_x_y    (PList 102 [_Ctor1 _x _y]))
(define _<x>          (PVec 103 [_x]))
(define _Ctor1_<x>_y  (PList 102 [_Ctor1 _<x> _y]))

(define case-env { Ctor1: Ctor1-record })


;; NXMAP -> list of {name: il} / [PError]
;;
(define `(nxmap-apply nxmap)
  (append-for (rec nxmap)
    (case rec
      ((Bind name xtor)
       { =name: (xtor identity) })
      (o [o]))))


;; Apply `nxmap-apply` to each NXMAP field in [ (Clause tag nxmap body)... ]
;;
(define `(Clauses-apply clauses)
  (for (c clauses)
    (case c
      ((Clause tag nxmap body)
       (Clause tag (nxmap-apply nxmap) body))
      (_ _))))


(expect (case-parse [(PList 7 [])] case-env)
        [(PError 7 "missing PATTERN in (case VALUE (PATTERN BODY)...)")])

(expect (case-parse [(PList 7 [_Ctor1_x_y])] case-env)
        [(PError 7 "missing BODY in (case VALUE (PATTERN BODY)...)")])

(expect (case-parse [(PList 7 [(PList 9 [_a _x _y]) _x])] case-env)
        [(PError 1 "expected a record constructor name")])

(expect (case-parse [_a] case-env)
        [(PError 1 "invalid (PATTERN BODY) in (case VALUE (PATTERN BODY)...); expected a list")])

(expect (case-parse [(PList 9 [_?y _y])] case-env)
        [(PError 16 "'?NAME' can appear only in parameter lists after all non-optional parameters")])


;; simple ctor pattern
(expect (Clauses-apply (case-parse [(PList 0 [_Ctor1_x_y _x])]
                                    case-env))
        [(Clause Ctor1-tag
                 {x: (il-word 2 identity), y: (il-nth-rest 3 identity)}
                 [_x])])

;; target in pattern
(expect (Clauses-apply (case-parse [(PList 0 [_Ctor1_<x>_y _x])]
                                    case-env))
        [(Clause Ctor1-tag
                 {x: (il-nth 1 (il-word 2 identity)),
                  y: (il-nth-rest 3 identity)}
                 [_x])])

;; pattern is a symbol
(expect (Clauses-apply (case-parse [(PList 0 [_x _x])] case-env))
        [(Clause "" {x: identity} [_x])])

;; pattern is a vector target
(expect (Clauses-apply (case-parse [(PList 0 [_<x> _x])] case-env))
        [(Clause "" {x: (il-nth 1 identity)} [_x])])

;; [C] -> [C2]
(expect (case-bodies [(Clause "" [(Bind "x" (lambda (il) (il-nth 1 il)))] [_x])]
                     (IVar "V")
                     [])
        [ ["" (il-nth 1 (IVar "V"))] ])



(expect (case-merge [ ["a" (IVar "V")]
                      ["b" (IVar "V")]
                      ["c" (IVar "X")]
                      ["" (IVar "X")] ])
        [ ["a b" (IVar "V")]
          ["" (IVar "X")] ])


(expect (case-fold [ ["a b" (IVar "V")]
                     ["c" (IVar "X")] ]
                   (lambda (tag) [(IString (addsuffix "%" tag)) (IVar "R")]))
        (IBuiltin "if" [ (IBuiltin "filter" [ (IString "a% b%") (IVar "R")])
                         (IVar "V")
                         (IBuiltin "if" [ (IBuiltin "filter" [ (IString "c%")
                                                               (IVar "R")])
                                          (IVar "X")
                                          (IString "")])]))

(expect (case-fold [ ["a b" (IVar "V")]
                     ["" (IVar "X")] ]
                   (lambda (tag) [ (IString (addsuffix "%" tag)) (IVar "R")]))
        (IBuiltin "if" [ (IBuiltin "filter" [ (IString "a% b%") (IVar "R") ])
                         (IVar "V")
                         (IVar "X") ]))


;; eval-only-once?

(expect nil (eval-only-once? (IArg ";" ".")))
(expect nil (eval-only-once? (IVar "x")))
(expect nil (eval-only-once? (IString "x")))
(expect nil (eval-only-once? (ICall "^n" [(IString 1) (IVar "x")])))
(expect nil (eval-only-once? (IBuiltin "wordlist" [(IString 1) (IVar "x")])))
(assert (eval-only-once? (ICall "^n" [(IString 1) (IBlock [])])))
(assert (eval-only-once? (ICall "foo" [])))
(assert (eval-only-once? (IBlock [])))

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
        "(.if (.filter !:T0,(.word 1,{V})),(.wordlist 4,99999999,{V}),)")

;; multiple cases
(expect (c0-ser "(case v ((C s w l) l) (a a))" tc-env)
        "(.if (.filter !:T0,(.word 1,{V})),(.wordlist 4,99999999,{V}),{V})")

;; bad value expr
(expect 1 (see "!(PError 4 'undefined variable: `bogus`')"
               (c0-ser "(case bogus)")))

;; wrong number of arguments
(expect (c0-ser "(case v ((C s l) l))" tc-env)
        "!(PError 8 '`C` accepts 3 arguments, not 2')")

;; captures in value
(expect (c0-ser "(lambda (v) (case v ((C a b c) a)))" tc-env)
        "`(.if (.filter !:T0,(.word 1,{1})),(^n 2,{1}),)")
(expect (c0-ser "(lambda (v) (case v ((C a b c) (lambda () a))))" tc-env)
        "`(.if (.filter !:T0,(.word 1,{1})),`(^n 2,{.1}),)")
(expect (c0-ser "(foreach v 1 (case v ((C a b c) (lambda () a))))" tc-env)
        "(.foreach ;,1,(.if (.filter !:T0,(.word 1,{;})),`(^n 2,{.;}),))")

;; complex value => use `foreach`
(expect (c0-ser "(case (F) ((C a b c) (lambda () a)))" tc-env)
        (.. "(.foreach ;,(^d (F )),(.if (.filter !1:T0!0%,{;}!0),"
            "`(^n 2,(^u {.;})),))"))

;; nested complex value => generate unique auto var
(expect (c0-ser "(case (F) ((C a b c) (case (F) (else 1))))" tc-env)
        (.. "(.foreach ;,(^d (F )),(.if (.filter !1:T0!0%,{;}!0),"
            "(.foreach ;;,(^d (F )),1),))"))

;; collapse clauses with equivalent bodies
(expect (c0-ser "(case v ((C a b c) b) ((D a b) b) (a a))" tc-env)
        "(.if (.filter !:T0 !:T1,(.word 1,{V})),(.word 3,{V}),{V})")

;; complex value & collapsed clauses
(expect (c0-ser "(case (F) ((C a b c) b) ((D a b) b))" tc-env)
        (.. "(.foreach ;,(^d (F )),(.if (.filter !1:T0!0% !1:T1!0%,{;}!0),"
            "(.word 3,(^u {;})),))"))
