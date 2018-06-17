(require "core")
(require "gen")
(require "parse")
(require "gen0" &private)
(require "gen-testutils")
(require "io")

;;--------------------------------
;; utilities
;;--------------------------------

(define (p1-0 text)
  (form-set-indices 0 (p1 text)))

;; Compile text, returning final env (or error).
;;
(define (text-to-env text ?env ?allow-nodes)
  (c0-block-cc env (pN text)
               (lambda (env nodes)
                 (if (and nodes
                          (not allow-nodes)
                          (not (eq? [(IString "")] nodes)))
                     (sprintf "UNEXPECTED NODES: '%q'" nodes)
                     env))))


;;--------------------------------
;; tests
;;--------------------------------


(define `flag-args
  [ (PString 0 "F") (PSymbol 2 "&private") (PSymbol 3 "&global")
    (PSymbol 2 "bar") ])

(expect 0 (scan-flags flag-args 0))
(expect 3 (scan-flags flag-args 1))
(expect 3 (scan-flags flag-args 2))
(expect 3 (scan-flags flag-args 3))
(expect 4 (scan-flags flag-args 4))

(expect ["&private" "&global"]  (get-flags flag-args 1))
(expect ["&global"]             (get-flags flag-args 2))
(expect flag-args               (skip-flags flag-args 0))
(expect [(PSymbol 2 "bar") ]    (skip-flags flag-args 1))
(expect [(PSymbol 2 "bar") ]    (skip-flags flag-args 2))


(expect (ILocal 3 0) (c0-local ".3"  "." nil))
(expect (ILocal 3 1) (c0-local ".3"  ".." nil))
(expect (ILocal 3 1) (c0-local "..3" "..." nil))
(expect (ILocal 3 2) (c0-local ".3"  "..." nil))


;;--------------------------------
;; c0-xxxx tests
;;--------------------------------


;; PString
(expect (c0-ser "\" x \"")
        " x ")


;; PSymbol: global data variable
(expect (c0-ser "d!0!")
        "{D!0!}")

;; PSymbol: global function variable
(expect (c0-ser "f!0!")
        "(.value F!0!)")

;; PSymbol: undefined variables
(expect (c0-ser "foo")
        "!(PError 1 'undefined variable \\'foo\\'')")

;; PDict: dictionaries

(expect (c0-ser "{}") "")
(expect (c0-ser "{a:1}") "a!=1")
(expect (c0-ser "{a:a}") "a!=(^d {1})")
(expect (c0-ser "{(f 1 2):a}") "(^k (F 1,2))!=(^d {1})")
(expect (c0-ser "{a:1 b:2}") "a!=1 b!=2")
(expect (c0-ser "{=a:1}") "(^k {1})!=1")

;; c0-block

(expect (c0-ser "\"x\" v")
        "(IBlock x,{V})")


;; PList: (functionvar ...)

(expect (c0-ser "(f!0! 1 2)")
        "(F!0! 1,2)")
(expect (c0-ser "(f 1)")
        "!(PError 2 '\\'f\\' accepts 2 arguments, not 1')")
(expect (c0-ser "(f)" (text-to-env "(declare (f a ?b))"))
        "!(PError 2 '\\'f\\' accepts 1 or 2 arguments, not 0')")
(expect (c0-ser "(f)" (text-to-env "(declare (f a ?b ...))"))
        "!(PError 2 '\\'f\\' accepts 1 or more arguments, not 0')")
(expect (c0-ser "(f)" (text-to-env "(declare (f a b ...))"))
        "!(PError 2 '\\'f\\' accepts 2 or more arguments, not 0')")
(expect (c0-ser "(f)" (text-to-env "(declare (f a ?b ?c))"))
        "!(PError 2 '\\'f\\' accepts 1 or 2 or 3 arguments, not 0')")


;; PList: (macro ...)

(define `inln-f
  (cons "." (IBuiltin "subst" [(ILocal 1 0)  ;; macro arg
                               (ILocal 1 1)  ;; capture
                               (ILocal 2 0)])))
(expect (c0-ser "(f 1 a)"
                { f: (EFunc NoGlobalName "." 2 inln-f),
                  a: (EArg ".7"),
                  =LambdaMarkerKey: (EMarker "..") })
        "(.subst 1,{1^1},{7^1})")

(define `inln-f-2
  (cons "." (ILambda (IBuiltin "word"
                               [ (ILocal 2 0)     ;; interior arg
                                 (ILocal 1 1)     ;; macro arg
                                 ]))))
(expect (c0-ser "(f 1)"
                 { f: (EFunc NoGlobalName "." 1 inln-f-2),
                   =LambdaMarkerKey: (EMarker "..") })
        "`(.word {2},1)")


;; Expand IWhere in macros.  Supports (current-file-line).

(let-global
 ((*compile-subject* "a b c")
  (*compile-file* "F2"))

 (expect (xlat-where (IWhere "F1:9") 1)
         (IWhere "F2:1"))

 ;; Expand compound macro with IWhere record.
 (define `inln-f-where (cons "." (IWhere "FM:99")))
 (expect (c0-ser "(f)" {f: (EFunc NoGlobalName "." 0 inln-f-where)})
         "!(IWhere 'F2:1')")

 ;; Expand symbol macro with IWhere record.
 (expect (c0-ser "S" {S: (EIL "" "p" (IWhere "FM:9"))})
         "!(IWhere 'F2:1')"))


;; PList: (datavar ...)

(expect (c0-ser "(d!0! 7)")
        "(^Y {D!0!},7)")

;; PList: (record ...)

(expect (il-ser (c0-record nil
                           (PSymbol 0 "CA")
                           [ (PString 1 "1") (PString 1 "2") ]
                           "S L"
                           "!:D0"))
        "!:D0 1 2")

(expect (il-ser
         (c0-ctor {Ctor: (ERecord "S L" "." "!:T0")}
                  (PSymbol 0 "Ctor")
                  "S L"))
        "`!:T0 (^d {1}) {2}")

(expect (c0-ser "C" {C: (ERecord "S W L" "." "!:T0")})
        "`!:T0 (^d {1}) {2} {3}")


;; PList: (<builtin> ...)

(expect (c0-ser "(or 7)" nil)
        "(.or 7)")
(expect (c0-ser "(if 1)")
        "!(PError 2 '\\'if\\' accepts 2 or 3 arguments, not 1')")
(expect (c0-ser "(if 1 2 3 4)")
        "!(PError 2 '\\'if\\' accepts 2 or 3 arguments, not 4')")
(expect (c0-ser "(bar)")
        "!(PError 2 'undefined symbol: \\'bar\\'')")
(expect (c0-ser "()")
        "!(PError 1 'missing function/macro name')")

;; PList: (arg ...)

(expect (c0-ser "(var 7)"
                (lambda-env [(PSymbol 0 "var")] nil))
        "(^Y {1},7)")

(begin
  (define (test-xmacro form)
    (PString 1 "hi"))
  (define `test-xm-env
    { var: (EXMacro (global-name test-xmacro) "i")})

  (expect (c0-ser "(var 7)" test-xm-env)
          "hi"))

;; PList: (lambda NAMES BODY)

;; lambda-env
(expect (lambda-env [(PSymbol 1 "b")]
                    (lambda-env [(PSymbol 1 "a")]
                                nil))
        (append { =LambdaMarkerKey: (EMarker ".."),
                  b: (EArg "..1"),
                  =LambdaMarkerKey: (EMarker "."),
                  a: (EArg ".1") }))

(let ((env (lambda-env (pN "a b c e f g h i j k") nil)))
  (expect (word 1 env)
          { =LambdaMarkerKey: (EMarker ".")})
  (expect (dict-get "a" env)
          (EArg ".1"))
  (expect (dict-get "i" env)
          (EArg ".8"))
  (expect (dict-get "j" env)
          (EIL "." "-" (IBuiltin "call" [(IString "^n") (IString 1) (ILocal 9 0)])))
  (expect (dict-get "k" env)
          (EIL "." "-" (IBuiltin "call" [(IString "^n") (IString 2) (ILocal 9 0)]))))

(expect (dict-get "..." (lambda-env (pN "a b ...") nil))
        (EIL "" "-" (IBuiltin "foreach" [(IString "N") (IString 3) (IVar "^v")])))
(expect (dict-get "r" (lambda-env (pN "a b ...r") nil))
        (EIL "" "-" (IBuiltin "foreach" [(IString "N") (IString 3) (IVar "^v")])))
(expect (dict-get "..." (lambda-env (pN "a b c d e f g h i ...") nil))
        (EIL "." "-" (IBuiltin "wordlist" [(IString 2) (IString 999999) (ILocal 9 0)])))
(expect (dict-get "..." (lambda-env (pN "a b c d e f g h ...") nil))
        (EArg ".9"))
(expect (dict-get "r" (lambda-env (pN "a b c d e f g h ...r") nil))
        (EArg ".9"))

;; local variable referencing arg 9
(expect (c0-ser "X" (lambda-env (pN "a a a a a a a a X Y") nil))
        "(.call ^n,1,{9})")

(expect (c0-ser "(lambda (a) v)")
        "`{V}")

(expect (c0-ser "(lambda (a b) a b)")
        "`(IBlock {1},{2})")

(foreach SCAM_DEBUG "-" ;; avoid upvalue warning
         (expect (c0-ser "(lambda (a) (lambda (b) a b))")
                 "``(IBlock {1^1},{1})"))

;; PSymbol: macro  (uses c0-lambda)
(begin
  (define `inln
    (cons "." (IBuiltin "subst" [(ILocal 1 0)  ;; macro arg
                                 (ILocal 1 1)  ;; capture
                                 (ILocal 2 0)])))
  (expect (il-ser (c0-macro { =LambdaMarkerKey: (EMarker "..") }
                            inln))
          "`(.subst {1},{1^2},{2})"))


;; PSymbol: builtin  (uses c0-lambda)

(expect (il-ser (c0-builtin nil "word" "2 or 1"))
        "`(.word {1},{2})")
(expect (il-ser (c0-builtin nil "or" "%"))
        "`(^apply or,{^av})")

;; ': quote

(expect (c0-ser "'(joe bob)")
        (p1 " (joe bob)"))

;;
;; `: quasi-quote
;;
;; A quoted expression *evaluates* to the AST for the quoted expression:
;;    ((c1 (c0 ["`" AST]))) -> AST

(define (cqq text)
  (c0-ser text { sym: (EIL "" "-" (IString "SYM")),
                 var: (EVar "VAR" "."),
                 ;; args = [`a `b]
                 args: (EIL "" "-" (IString [(PSymbol 1 "a")
                                             (PSymbol 2 "b")])) }))

(expect (c0 (p1-0 "`x") nil)
        (IString (p1-0 "x")))

(expect (cqq "`,sym")
        "SYM")


(expect (cqq "`,var")
        "{VAR}")

(expect (cqq "`(a 1 ,var)")
        (concat (PList 2 [ (PSymbol 3 "a") (PString 5 1) ]) " (^d {VAR})"))



;; nested quote/unquote
(begin
  (define `(dd node) (concat "(^d " node ")"))
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

(expect (c0-ser ",a")
        "!(PError 1 'unquote (,) outside of a quasiquoted (`) form')")
(expect (cqq "`)")
        "!(PError 2 ') .')")
(expect (cqq "`,)")
        "!(PError 3 ') .')")


;; splicing

(expect (cqq "`(1 ,@args 2)")
        (PList 2 [(PString 3 1) (PSymbol 1 "a") (PSymbol 2 "b") (PString 8 2)]))

(expect (cqq "`(1 ,@var 2)")
        (PList 2 [ (PString 3 1) "{VAR}" (PString 8 2) ]))

;;
;; declare & define
;;

(expect (text-to-env "(declare var)")
        { var: (EVar (gen-global-name "var" nil) "p") })
(expect (text-to-env "(declare var &public)")
        { var: (EVar (gen-global-name "var" nil) "x") })

;; declare FUNC
(expect (text-to-env "(declare (fn a b))")
        { fn: (EFunc (gen-global-name "fn" nil) "p" 2 nil) })
(expect (text-to-env "(declare (fn a b) &public)")
        { fn: (EFunc (gen-global-name "fn" nil) "x" 2 nil) })

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
   (expect env { x: (EVar (xns "~x") "p") })
   (expect sil (xns "(IBlock (^set ~x,1),(.info {~x}))"))))


;; define FUNC
(expect (c0-ser "(define (f a ?b) (join a b))")
        (xns "(^fset ~f,`(.join {1},{2}))"))

(expect (text-to-env "(define (f a) a)" nil 1)
        (xns { f: (EFunc "~f" "p" 1 nil) }))

;; define compound macro
(expect (text-to-env "(define `(M a) (words a))")
        { M: (EFunc NoGlobalName "p" 1
                    (cons "" (IBuiltin "words" [(ILocal 1 0)]))) })

(expect (text-to-env "(define `(M a) &public (words a))")
        { M: (EFunc NoGlobalName "x" 1
                    (cons "" (IBuiltin "words" [(ILocal 1 0)]))) })


;; define symbol macro
(expect (text-to-env "(define `I 7)"
                     {x: (EVar "x" "")})
        { I: (EIL "" "p" (IString 7)),
          x: (EVar "x" "") })

(expect (text-to-env "(define `I &public 7)"
                     {x: (EVar "x" "")})
        { I: (EIL "" "x" (IString 7)),
          x: (EVar "x" "") })


;; (define ...) errors

(expect nil (check-optional-args [(PSymbol 0 "a") (PSymbol 0 "b")]))
(expect nil (check-optional-args [(PSymbol 0 "?a") (PSymbol 0 "?b")]))
(expect nil (not (check-optional-args [(PSymbol 0 "?a") (PSymbol 0 "b")])))
(expect nil (not (check-optional-args [(PSymbol 0 "...a") (PSymbol 0 "b")])))


(expect (c0-ser "(define)")
        "!(PError 2 'missing FORM in (define FORM ...); expected a list or symbol')")
(expect (c0-ser "(define `1)")
        "!(PError 5 'invalid FORM in (define `FORM ...); expected a list or symbol')")
(expect (c0-ser "(define `(m ...x) x)")
        "!(PError 8 'macros cannot have rest (...) parameters')")
;; Allow "?x", but not "...x".
(expect (c0-ser "(define `(m ?a) a)")
        "")
(expect (c0-ser "(define (f ...x z) x)")
        "!(PError 9 'non-optional parameter after optional one')")
(expect (c0-ser "(lambda (f ...x z) x)")
        "!(PError 9 'non-optional parameter after optional one')")
(expect (c0-ser "(define (f ?a x) x)")
        "!(PError 9 'non-optional parameter after optional one')")
;; report errors when compiled, not when used
(expect (c0-ser "(define `M UNDEF)")
        "!(PError 7 'undefined variable \\'UNDEF\\'')")
(expect (c0-ser "(define `(M) UNDEF)")
        "!(PError 9 'undefined variable \\'UNDEF\\'')")
;; check for name conflicts with built-ins and automatic vars
(expect (c0-ser "(define + 1)")
        "!(PError 4 'cannot redefine automatic variable '$+'')")
(expect (c0-ser "(define (word a) a)")
        "!(PError 4 'cannot redefine built-in function \\'word\\'')")

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


;;
;; Macro exporting/importing.
;;

(define (canned-MIN name)
  (cond
   ((eq? ".out/D/M.min" name) "(declare X) (declare x &public)")
   ((eq? ".out/M.min" name) "(declare X) (declare x &public)")
   ((eq? ".out/CM.min" name) "(define `(F) 1337) (define `(G) &public (F))")
   ((eq? ".out/SM.min" name) "(define `A 7331) (define `B &public A)")
   (else (expect (concat "Bad module: " name) nil))))

(define (canned-read-file name)
  (env-export-line (text-to-env (canned-MIN name) nil 1)))

(define (harness-locate-module src name)
  (concat (patsubst "./%" "%" (dir src)) name ".scm"))

(let-global
    ;; This overrides (!) the function for looking up modules.
    ((locate-module harness-locate-module)
     (read-file canned-read-file)
     (read-lines (lambda (file a b) (wordlist a b (split "\n" (canned-read-file file)))))
     (*compile-file* "foo.scm")
     (*file-mods* "D/M M F CM SM")
     (*obj-dir* ".out/")
     (*is-boot* nil))

  ;; Test: (require MOD)

  (expect (module-id (locate-module *compile-file* "D/M"))
          "D/M")

  (expect (c0-ser "(require \"D/M\")")
          "(IBlock (^require D/M),!(ICrumb 'require' 'D/M.scm'))")

  (let-global ((*is-boot* 1)
               (*file-mods* "'D/M"))
    (expect (c0-ser "(require \"D/M\")")
            "(IBlock (^require 'D/M),!(ICrumb 'require' 'D/M.scm'))"))

  (expect (text-to-env "(require \"M\")" nil 1)
          {x: (EVar (gen-global-name "x" nil) "i")})

  (expect (c0-ser "(require \"D/M\" \"xyz\")")
          "!(PError 0 'too many arguments to require')")

  ;; (require MOD &private)

  (expect (text-to-env "(require \"M\" &private)" nil 1)
          { x: (EVar (gen-global-name "x" nil) "x"),
               X: (EVar (gen-global-name "X" nil) "p") })

  ;; Verify that IMPORTED macros are expanded in their original environment
  ;; (read from their MIN files' exports) so they can see private members.

  ;; IMPORTED compound macro
  (expect 1 (see 1337
                 (c0-ser "(require \"CM\") (G)")))

  ;; IMPORTED symbol macro
  (expect 1 (see 7331
                 (c0-ser "(require \"SM\") B"))))
