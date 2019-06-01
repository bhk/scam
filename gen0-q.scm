(require "core.scm")
(require "gen.scm")
(require "parse.scm")
(require "gen0.scm" &private)
(require "gen-testutils.scm")
(require "io.scm")

;;--------------------------------
;; utilities
;;--------------------------------


;; Compile text, returning final env (or error).
;;
(define (text-to-env text ?env ?allow-nodes)
  (c0-block-cc env (pN text)
               (lambda (env nodes)
                 (if (and nodes
                          (not allow-nodes)
                          (not (eq? [(IString "")] nodes)))
                     (sprintf "UNEXPECTED NODES: %q" nodes)
                     env))))


;;--------------------------------
;; tests
;;--------------------------------


;; get-flags, skip-flags

(define `flag-args
  [ (PString 0 "F") (PSymbol 2 "&private") (PSymbol 3 "&native")
    (PSymbol 2 "bar") ])

(expect 0 (scan-flags flag-args 1 0))
(expect 3 (scan-flags flag-args 2 1))
(expect 3 (scan-flags flag-args 3 2))
(expect 3 (scan-flags flag-args 4 3))
(expect 4 (scan-flags flag-args 5 4))

(expect ["&private" "&native"]  (get-flags flag-args))
(expect ["&native"]             (get-flags (rest flag-args)))
(expect [(PSymbol 2 "bar") ]    (skip-flags flag-args))

;; get-arity

(expect "1" (get-arity "a"))
(expect "0 1" (get-arity "?a"))
(expect "0+" (get-arity "...a"))
(expect "1+" (get-arity "a ?c ...d"))
(expect "1 2 3" (get-arity "a ?c ?d"))

;; check-args

(expect nil (check-args [(PSymbol 0 "a") (PSymbol 0 "b")]))
(expect nil (check-args [(PSymbol 0 "?a") (PSymbol 0 "?b")]))
(expect nil (not (check-args [(PSymbol 0 "?a") (PSymbol 0 "b")])))
(expect nil (not (check-args [(PSymbol 0 "...a") (PSymbol 0 "b")])))
(expect nil (not (check-args [(PSymbol 0 "...a") (PSymbol 0 "...b")])))

;; c0-local

(expect (IArg 3 ".") (c0-local 3 "."  "." nil))
(expect (IArg 3 "..") (c0-local 3 "."  ".." nil))
(expect (IArg 3 "..") (c0-local 3 ".." "..." nil))
(expect (IArg 3 "...") (c0-local 3 "."  "..." nil))


;; translate

(expect (translate (IString "x") "." "." [] 9)
        (IString "x"))
;; IWhere
(expect (translate (IWhere 2) "." "." [] 9)
        (IWhere 9))

;; symbol macro with capture
(expect (translate (IArg 2 ".") "." "..." [] 9)
        (IArg 2 "..."))
;; symbol macro with interior arg
(expect (translate (ILambda (IArg 2 ".")) "." "..." [] 9)
        (ILambda (IArg 2 ".")))
;; symbol macro with auto arg
(expect (translate (IArg ";" ".") ";" ";" [] 9)
        (IArg ";" "."))


;; compound macro with capture
(expect (translate (IArg 2 "..") ".." "..." [(IString "a")] 9)
        (IArg 2 "..."))
(expect (translate (IArg 2 "..") ".." "." [(IString "a")] 9)
        (IArg 2 "."))
;; compound macro with auto capture
(expect (translate (IArg ";" "..") ".." ".;" [(IString 1)] 9)
        (IArg ";" "."))

(expect (translate (IArg 2 "..") ".." ".;;" [(IString "a")] 9)
        (IArg 2 "."))
;; compound macro with interior arg
(expect (translate (ILambda (IArg 2 ".")) ".." "..." [(IString "x")] 9)
        (ILambda (IArg 2 ".")))
;; compound macro with argument
(expect (translate (IArg 2 ".") ".." "..." [(IString "a") (IString "b")] 9)
        (IString "b"))

;; compound macro with "rest" argument
(expect (translate (IArg "1+" ".") ".." "..." [(IString "a") (IString "b")] 9)
        (il-vector [(IString "a") (IString "b")]))

;; macro argument w/ lambda capture
(foreach
    to-depth ". .. ..."
    (expect (translate (IArg 1 ".") ".." to-depth [(IArg 2 ".")] 9)
            (IArg 2 "."))
    (expect (translate (ILambda (IArg 1 "..")) ".." to-depth [(IArg 2 ".")] 9)
            (ILambda (IArg 2 "..")))
    ;; macro argument w/ interior arg
    (expect (translate (ILambda (IArg 1 "..")) ".." to-depth
                       [(ILambda (IArg 1 "."))] 9)
            (ILambda (ILambda (IArg 1 ".")))))

;; symbol macro with auto vars & old-ad
(expect (translate (IFor ";;" (IArg ";" ".") (IArg ";;" "."))
                   "..;" "...;;" nil 9)
        (IFor ";;;" (IArg ";" "..") (IArg ";;;" ".")))
(expect (translate (IFor ";;;" (IArg ";" ".") (IConcat [(IArg ";;" ".")
                                                        (IArg ";;;" ".")]))
                   "..;;" "..." nil 9)
        (IFor ";" (IArg ";" "..") (IConcat [(IArg ";;" "..")
                                            (IArg ";" ".")])))

;; macro argument w/ interior auto; AD != OLD-AD
(expect (translate (IFor ";" (IArg ";" "..")
                         (IConcat [(IArg 1 ".")
                                   (IArg 2 ".")
                                   (ILambda (IArg 1 ".."))
                                   (ILambda (IArg 2 ".."))
                                   (IArg ";" ".")]))
                   ".." "...;;"
                   [(IArg ";;" ".")
                    (IFor ";;;" (IArg ";;" ".") (IArg ";;;" "."))
                    ] 9)
        (IFor ";;;" (IArg ";" "...")
              (IConcat [(IArg ";;" ".")
                        (IFor ";;;;" (IArg ";;" ".") (IArg ";;;;" "."))
                        (ILambda (IArg ";;" ".."))
                        (ILambda (IFor ";" (IArg ";;" "..") (IArg ";" ".")))
                        (IArg ";;;" ".")])))


;;--------------------------------
;; c0
;;--------------------------------

;; PString
(expect (c0 (PString 9 " a!0b ") nil)
        (IString " a!0b "))
;; PQuote
(expect (c0 (PQuote 8 (PString 9 " a!0b ")) nil)
        (IString (PString 9 " a!0b ")))
;; errors
(expect (c0 (PUnquote 9 (PString 10 "x")) nil)
        (PError 9 "unquote (,) outside of a quasiquoted (`) form"))
(expect (c0 (PSplice 9 (PString 10 "x")) nil)
        (PError 9 "splice (,@) outside of a quasiquoted (`) form"))
(expect (c0 (PError 9 " a!0b ") nil)
        (PError 9 " a!0b "))

;;--------------------------------
;; c0 PDict: dictionaries
;;--------------------------------

(expect (c0-ser "{}") "")
(expect (c0-ser "{a:1}") "a!=1")
(expect (c0-ser "{a:a}") "a!=(^d {1})")
(expect (c0-ser "{(f 1 2):a}") "(^k (F 1,2))!=(^d {1})")
(expect (c0-ser "{a:1 b:2}") "a!=1 b!=2")
(expect (c0-ser "{=a:1}") "(^k {1})!=1")


;;--------------------------------
;; c0 PQQuote
;;--------------------------------

(define (cqq text)
  (c0-ser text { sym: (EIL "p" "" (IString "SYM")),
                 var: (EVar "p" "VAR"),
                 ;; args = [`a `b]
                 args: (EIL "p" "" (IString [(PSymbol 1 "a")
                                             (PSymbol 2 "b")])) }))

(expect (c0 (p1 "`x") nil) (IString (p1 " x")))
(expect (cqq "`,sym") "SYM")
(expect (cqq "`,var") "{VAR}")
(expect (cqq "`(a 1 ,var)")
        (.. (PList 2 [ (PSymbol 3 "a") (PString 5 1) ]) " (^d {VAR})"))

;; nested quote/unquote
(begin
  (define `(dd node) (.. "(^d " node ")"))
  (define (cc ...nodes) (IConcat nodes))

  ;; Some demote operations are deferred until run-time, some are already
  ;; applied to literal values.
  (expect (subst "TOP"   (PList 2 ["(A)"])
                 "(A)"   (dd (PQQuote 4 "(B)"))
                 "(B)"   (PList 5 ["(C1)" "(C2)" "(C3)"])
                 "(C1)"  [(PSymbol 6 "a")]
                 "(C2)"  [(PUnquote 8 (PSymbol 9 "var"))]
                 "(C3)"  (dd (PUnquote 11 "{VAR}"))
                 "TOP")
          (cqq "`( `(a ,var ,,var))")))

;; errors

(expect (cqq "`)")  "!(PError 2 ') .')")
(expect (cqq "`,)") "!(PError 3 ') .')")

;; splicing

(expect (cqq "`(1 ,@args 2)")
        (PList 2 [(PString 3 1) (PSymbol 1 "a") (PSymbol 2 "b") (PString 8 2)]))
(expect (cqq "`(1 ,@var 2)")
        (PList 2 [ (PString 3 1) "{VAR}" (PString 8 2) ]))


;;--------------------------------
;; c0 PSymbol  ->  c0-S
;;--------------------------------

;; global data variable
(expect (c0 (PSymbol 9 "d") {d: (EVar "p" "~d")})
        (IVar "~d") )

;; global function variable
(expect (c0 (PSymbol 9 "f") {f: (EFunc "p" "~f" 1)})
        (IBuiltin "value" [(IString "~f")]))

;; undefined
(expect (c0 (PSymbol 9 "x") nil)
        (PError 9 "undefined variable: `x`"))

;; local variable
(expect (c0 (PSymbol 9 "a") (append {a: (ELocal 1 ".")}
                                    (depth-marker ".")))
        (IArg 1 "."))

(expect (c0 (PSymbol 9 "a") (append {a: (ELocal 1 ".")}
                                    (depth-marker "..")))
        (IArg 1 ".."))

(expect (c0 (PSymbol 9 "a") (append {a: (ELocal ";" nil)}
                                    (depth-marker ";")))
        (IArg ";" "."))


;; compound macro [and see macro round-trip tests, below]


;; (lambda (x)
;;   (define `(cm a _ c)
;;      (.. a x (lambda (z) z)))
;;   cm)
;;
(define `cm
  (IConcat [(IArg 1 ".")                  ;; macro arg
            (IArg 2 "..")                 ;; capture
            (ILambda (IArg 3 "."))]))     ;; internal arg

(expect (c0 (PSymbol 9 "m") (append {m: (EMacro "p" ".." 1 cm)}
                                    (depth-marker ".")))
        (ILambda
         (IConcat [(IArg 1 ".")
                   (IArg 2 "..")
                   (ILambda (IArg 3 "."))])))


;; builtin
(expect (c0 (PSymbol 9 "a") {a: (EBuiltin "p" "words" 1)})
        (ILambda (IBuiltin "words" [ (IArg 1 ".") ])))

;; vararg builtins
(expect (il-ser (c0-builtin nil "or" "%"))
        "`(^na or,{^av})")


;;--------------------------------
;; c0-block
;;--------------------------------

(expect (c0-ser "\"x\" v") "(IBlock x,{V})")


;;--------------------------------
;; c0 PList
;;--------------------------------

;; PList = (functionvar ...)

(expect (c0-ser "(f!0! 1 2)")
        "(F!0! 1,2)")
(expect (c0-ser "(f 1)")
        "!(PError 2 '`f` accepts 2 arguments, not 1')")
(expect (c0-ser "(f)" (text-to-env "(declare (f a ?b))"))
        "!(PError 2 '`f` accepts 1 or 2 arguments, not 0')")
(expect (c0-ser "(f)" (text-to-env "(declare (f a ?b ...))"))
        "!(PError 2 '`f` accepts 1 or more arguments, not 0')")
(expect (c0-ser "(f)" (text-to-env "(declare (f a b ...))"))
        "!(PError 2 '`f` accepts 2 or more arguments, not 0')")
(expect (c0-ser "(f)" (text-to-env "(declare (f a ?b ?c))"))
        "!(PError 2 '`f` accepts 1 or 2 or 3 arguments, not 0')")


;; PList = (macro ...)

;; (define `(f a) a) *(f 1)*
(expect (c0-ser "(f 1)" (append {f: (EMacro "p" "." 1 (IArg 1 "."))}))
        "1")

;; (lambda (x y) (define `(f a) (.. a y)) *(f 7)* )    [capture]
(expect (c0-ser "(f 7)" (append {f: (EMacro "p" ".." 1
                                            (IConcat [(IArg 1 ".")
                                                      (IArg 2 "..")]))}
                                (depth-marker ".")))
        "7{2}")

;; (define `(f) (lambda (x) x)) *(f)*
(expect (c0-ser "(f)" { f: (EMacro "p" "." 0 (ILambda (IArg 1 "."))) })
        "`{1}")


;; macro defined in ".;;" and expanded at ".;;" with an arg (IArg ";;" ".")

;; (lambda (_)
;;   (foreach a XXX
;;      (define `(cm x) a)
;;      (cm 1)))
(expect (c0-ser "(cm 1)" (append { cm: (EMacro "p" ".." 1 (IArg ";" "..")) }
                                 (depth-marker ".;")))
        "{;}")

;; PList = (<builtin> ...)

(expect (c0-ser "(or 7)" nil)
        "(.or 7)")
(expect (c0-ser "(if 1)")
        "!(PError 2 '`if` accepts 2 or 3 arguments, not 1')")
(expect (c0-ser "(if 1 2 3 4)")
        "!(PError 2 '`if` accepts 2 or 3 arguments, not 4')")
(expect (c0-ser "(bar)")
        "!(PError 2 'undefined symbol: `bar`')")
(expect (c0-ser "()")
        "!(PError 1 'missing function/macro name')")


;; PList = (datavar ...)

(expect (c0-ser "(d!0! 7)")
        "(^Y {D!0!},7)")


;; PList = (arg ...)

(expect (c0-ser "(var 7)" (append (depth-marker ".")
                                  { var: (ELocal 1 ".") }))
        "(^Y {1},7)")


;; PList = (xmacro ...)

(begin
  (define (test-xmacro form)
    (PString 1 "hi"))
  (define `test-xm-env
    { var: (EXMacro "i" (native-name test-xmacro))})

  (expect (c0-ser "(var 7)" test-xm-env)
          "hi"))


;; PList: (lambda NAMES BODY)  --> special-lamda

;; arg-locals

(expect (arg-locals [(PSymbol 1 "a") (PSymbol 3 "?b") (PSymbol 5 "...c")]
                    1 "...")
        (append {a: (ELocal 1 "...")}
                {b: (ELocal 2 "...")}
                {c: (ELocal "3+" "...")}))

;; (lambda ...)

(expect (c0-ser "(lambda (a) v)")
        "`{V}")
(expect (c0-ser "(lambda (a b) a b)")
        "`(IBlock {1},{2})")
(foreach SCAM_DEBUG "-" ;; avoid upvalue warning
         (expect (c0-ser "(lambda (a) (lambda (b) a b))")
                 "``(IBlock {.1},{1})"))


;; PList = (record ...)

(expect (il-ser (c0-record nil
                           (PSymbol 0 "CA")
                           [ (PString 1 "1") (PString 1 "2") ]
                           "S L"
                           "!:D0"))
        "!:D0 1 2")


;;--------------------------------
;; declare & define
;;--------------------------------

(expect (text-to-env "(declare var)")
        { var: (EVar "p" (gen-native-name "var" nil)) })
(expect (text-to-env "(declare var &public)")
        { var: (EVar "x" (gen-native-name "var" nil)) })

;; declare FUNC
(expect (text-to-env "(declare (fn a b))")
        { fn: (EFunc "p" (gen-native-name "fn" nil) 2) })
(expect (text-to-env "(declare (fn a b) &public)")
        { fn: (EFunc "x" (gen-native-name "fn" nil) 2) })

;; declare errors
(expect (c0-ser "(declare)")
        "!(PError 2 'missing FORM in (declare FORM ...); expected a list or symbol')")
(expect (c0-ser "(declare foo 7)")
        "!(PError 6 'too many arguments to (declare ...)')")
(expect (c0-ser "(declare (1 a))")
        "!(PError 5 'invalid NAME in (declare (NAME...)); expected a symbol')")


;; define VAR
(p1-block-cc
 "(define x 1) (info x)"
 (lambda (env sil)
   (expect env { x: (EVar "p" (xns "~x")) })
   (expect sil (xns "(IBlock (^set ~x,1),(.info {~x}))"))))


;; define FUNC
(expect (c0-ser "(define (f a ?b) (join a b))")
        (xns "(^fset ~f,`(.join {1},{2}))"))

(expect (text-to-env "(define (f a) a)" nil 1)
        (xns { f: (EFunc "p" "~f" 1) }))

;; define compound macro
(expect (text-to-env "(define `(M a) (words a))")
        {M: (EMacro "p" "." 1 (IBuiltin "words" [(IArg 1 ".")]))})

(expect (text-to-env "(define `(M a) &public (words a))")
        {M: (EMacro "x" "." 1 (IBuiltin "words" [(IArg 1 ".")]))})


;; define symbol macro
(expect (text-to-env "(define `I 7)"
                     {x: (EVar "p" "x")})
        { I: (EIL "p" "" (IString 7)),
          x: (EVar "p" "x") })

(expect (text-to-env "(define `I &public 7)"
                     {x: (EVar "p" "x")})
        { I: (EIL "x" "" (IString 7)),
          x: (EVar "p" "x") })

;; (define ...) errors

(expect (c0-ser "(define)")
        "!(PError 2 'missing FORM in (define FORM ...); expected a list or symbol')")
(expect (c0-ser "(define `1)")
        "!(PError 5 'invalid FORM in (define `FORM ...); expected a list or symbol')")(expect (c0-ser "(define `(m ?a) a)")
        "")
(expect (c0-ser "(define (f ...x ?z) x)")
        "!(PError 7 ''...' argument not in last position')")
(expect (c0-ser "(define (f ?a x) x)")
        "!(PError 9 'non-optional parameter after optional one')")
;; report errors when compiled, not when used
(expect (c0-ser "(define `M UNDEF)")
        "!(PError 7 'undefined variable: `UNDEF`')")
(expect (c0-ser "(define `(M) UNDEF)")
        "!(PError 9 'undefined variable: `UNDEF`')")

;; define and use symbol macro

(expect (c0-ser "(define `X 3) X")
        "3")

;; define and use compound macro

(expect (c0-ser "(define `(M a) (info a)) (M 2)")
        "(.info 2)")
(expect (c0-ser "(define `(M a) (info a)) M")
        "`(.info {1})")
(expect (c0-ser "(define `(M ?a) (info a)) (M)")
        "(.info )")


;;--------------------------------
;; macro round-trip tests
;;--------------------------------

;; compound macro with capture

(expect (c0-ser "(lambda (a b) (define `(m a) (f a b)) (m 1))")
        (c0-ser "(lambda (a b) (f 1 b))"))

;; symbol macro with capture
;;
;; (lambda (s)
;;   (define `(m)
;;     (define `s a)
;;     s)
;;   (m))

(expect (c0-ser "(lambda (a) (define `(m) (define `s a) s) (m))")
        (c0-ser "(lambda (a) a)"))

;; symbol macro with internal arg

(expect (c0-ser "(begin (define `s2 (lambda (x) x)) (s2 3))")
        (c0-ser "((lambda (x) x) 3)"))

;; macro with "rest" arg

(expect (c0-ser "(begin (define `(m ...z) z) (m 1 2 3))")
        (c0-ser "\"1 2 3\""))


;;--------------------------------
;; c0 PSymbol: record constructors
;;--------------------------------

(expect (il-ser
         (c0-ctor {Ctor: (ERecord "p" "S L" "!:T0")}
                  (PSymbol 0 "Ctor")
                  "S L"))
        "`!:T0 (^d {1}) {2}")

(expect (c0-ser "C" {C: (ERecord "p" "S W L" "!:T0")})
        "`!:T0 (^d {1}) {2} {3}")


;;--------------------------------
;; gen0
;;--------------------------------

(expect [ {A: (EVar "p" "'A")}
          (ICall "^set" [(IString "'A") (IString "1")])
          (IVar "'A") ]
        (let-global ((*is-boot* nil))
          (gen0 (pN "(define A 1) A") nil)))
