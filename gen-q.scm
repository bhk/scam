(require "core.scm")
(require "parse.scm")
(require "gen.scm" &private)

;;
;; IL-related definitions
;;

(expect (il-concat nil) nil)
(expect (il-concat [ (IString "abc") ])
        (IString "abc"))
(expect (il-concat [ (IString "a") (IString "b") ])
        (IString "ab"))
(expect (il-concat [ (IString "a")
                     (IConcat [ (IString "b") (IVar "V") (IString "c") ])
                     (IString "d") ])
        (IConcat [ (IString "ab") (IVar "V") (IString "cd") ]))


(expect (il-demote (IVar "V"))
        (ICall "^d" [ (IVar "V") ]))
(expect (il-demote (IString "a b"))
        (IString ["a b"]))
(expect (il-demote (ICall "^u" [ (IVar "V") ]))
        (IVar "V"))


;;
;; Environment definitions
;;



;; current-depth

(expect "." (depth.l ".;"))
(expect ";" (depth.a ".;"))

(expect nil (current-depth nil))
(expect ".;" (current-depth (depth-marker ".;")))

;; gen-native-name

(let-global ((*is-boot* 1))
  (expect "`a" (gen-native-name "a" nil))
  (expect "a" (gen-native-name "a" "&native")))

(let-global ((*is-boot* nil))
  (expect "'a" (gen-native-name "a" nil))
  (expect "a" (gen-native-name "a" "&native")))

;; gensym

(expect (gensym-name "foo" nil nil)
        "foo&")
(expect (gensym (PSymbol 0 "foo") nil)
        (PSymbol 0 "foo&"))
(expect (gensym (PSymbol 0 "foo")
                {(symbol-name (gensym (PSymbol 0 "foo") nil)): (EVar "p" "x")})
        (PSymbol 0 "foo&1"))

;; gen-error

(expect (gen-error (PString 12 "x") "Msg: %s %s" "hello" "error")
        (PError 12 "Msg: hello error"))


;; check-arity

(define `args2 [(PSymbol 1 "a") (PSymbol 2 "b")])
(define `argsym (PSymbol 0 "word"))

(expect nil (check-arity "1 2 3" args2 argsym))
(expect nil (check-arity "2+" args2 argsym))
(expect nil (check-arity "0+" [] argsym))
(expect (check-arity "3+" args2 argsym)
        (PError 0 "`word` accepts 3 or more arguments, not 2"))
(expect (check-arity "3" args2 argsym)
        (PError 0 "`word` accepts 3 arguments, not 2"))


;; get-bindings  [de-structuring]

(expect (pat-bindings `a (IString "1") ";")
        { a: (EIL "p" ";" (IString 1)) })

(expect (pat-bindings `{=a:b} (IString "1") ";")
        { b: (EIL "p" ";" (ICall "^dv" [(IString 1)])),
          a: (EIL "p" ";" (ICall "^dk" [(IString 1)])) })

;; base-env and resolve

(expect (resolve (PSymbol 0 "d!0!") { "d!0!": (EVar "p" "D!0!")})
        (EVar "p" "D!0!"))
