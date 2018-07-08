(require "core.scm")
(require "parse.scm")
(require "gen.scm" &private)

;;
;; IL-related definitions
;;

(expect (il-concat nil)
        (IString ""))
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

;; gensym

(expect (gensym-name "foo" nil nil)
        "foo&")
(expect (gensym (PSymbol 0 "foo") nil)
        (PSymbol 0 "foo&"))
(expect (gensym (PSymbol 0 "foo")
                {(symbol-name (gensym (PSymbol 0 "foo") nil)): (EVar "x" ".")})
        (PSymbol 0 "foo&1"))


;; gen-error

(expect (gen-error (PString 12 "x") "Msg: %s %s" "hello" "error")
        (PError 12 "Msg: hello error"))

;; base-env and resolve

(expect (resolve (PSymbol 0 "d!0!") { "d!0!": (EVar "D!0!" nil)})
        (EVar "D!0!" nil))
