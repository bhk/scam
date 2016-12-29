;; Tests for gen0

(require "core")
(require "parse")
(require "gen" &private)

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

;; after, env-rewind

(expect "b c a"  (after "a" "a b c a"))
(expect "c d e"  (after "c" "a b c c d e"))
(expect ""       (after "f" "a b c d e f"))
(expect ""       (after "z" "a b c d e f"))

(expect (hash-bind "a" "asdf")
        (env-rewind-M (hash-bind "x" 1
                            (hash-bind "m" "-"
                                  (hash-bind "a" "asdf")))
                      "m"))

(expect (append
         (hash-bind LambdaMarkerKey "..")
         (hash-bind "a" "asdf"))
        (env-rewind (hash-bind "x" 1
                          (hash-bind LambdaMarkerKey ".."
                                (hash-bind "f" (EFunc "f" "." "1" nil)
                                      (hash-bind "a" "asdf"))))
                    "f"))


;; gensym

(expect (gensym-name "foo" nil nil)
        "foo&")
(expect (gensym (PSymbol 0 "foo") nil)
        (PSymbol 0 "foo&"))
(expect (gensym (PSymbol 0 "foo")
                (hash-bind (symbol-name (gensym (PSymbol 0 "foo") nil))
                           (EVar "x" ".")))
        (PSymbol 0 "foo&1"))


;; gen-error

(expect (gen-error (PString 12 "x") "Msg: %s %s" "hello" "error")
        (PError 12 "Msg: hello error"))


;; env-compress

(for s [",.;[]\!1!0!11!10!021!10 !. !@#$%^&*()_+=|}{\\][/.,?><';\":`~,i x!=F!0x v!=V!0x"]
     (expect s (env-expand (env-compress s))))


;; env-export & env-import

;; import only public members

(fexpect (import-binding "f" (EFunc "F" "x" 2 nil) "MOD")
         (hash-bind "f" (EFunc "F" "iMOD" 2 nil)))

(fexpect (import-binding "f" (EFunc "F" "p" 2 nil) "MOD")
         nil)


(define (export-round-trip env flag filename)
  (env-import
   (env-parse [ "# comment"
                (subst "\n" "" (env-export env))
                "# F F F F F F"])
   flag
   filename))


(fexpect (export-round-trip
          (append (hash-bind "f" (EFunc "f" "x" 2 nil))
                  (hash-bind "x" (EVar "X" "x"))
                  (hash-bind "a" (EFunc "fa" "x" 2 ["a b" (PSymbol 0 "a")]))
                  (hash-bind "g" (EFunc "g" "p" 1 nil))  ;; private
                  (hash-bind "g" (EFunc "g" "i" 1 nil))  ;; imported
                  (hash-bind "m" (ESMacro "Q 1" "x"))
                  (hash-bind "a:n\n,x" (EVar "xyz" "x")))
          nil
          "MOD")

         (append (hash-bind "f" (EFunc "f" "iMOD" 2 nil))
                 (hash-bind "x" (EVar "X" "i"))
                 (hash-bind "a" (EFunc "fa" "iMOD" 2 ["a b" (PSymbol 0 "a")]))
                 (hash-bind "m" (ESMacro "Q 1" "iMOD"))
                 (hash-bind "a:n\n,x" (EVar "xyz" "i"))))


;; import public AND private members

(expect (export-round-trip
         (append (hash-bind "f" (EFunc "f" "x" 2 nil))
                 (hash-bind "x" (EVar "X" "x"))
                 (hash-bind "a" (EFunc "a" "i" 2 ["a b" (PSymbol 0 "a")]))
                 (hash-bind "g" (EFunc "g" "p" 1 nil))  ;; private
                 (hash-bind "g" (ESMacro "g" "i"))    ;; imported macro
                 (hash-bind "a:n\n,x" (EVar "xyz" "x")))
         1
         "File Name.min")

        (append (hash-bind "f" (EFunc "f" "x" 2 nil))
                (hash-bind "x" (EVar "X" "x"))
                (hash-bind "a" (EFunc "a" "i" 2 ["a b" (PSymbol 0 "a")]))
                (hash-bind "g" (EFunc "g" "p" 1 nil))
                (hash-bind "g" (ESMacro "g" "i"))  ;; imported
                (hash-bind "a:n\n,x" (EVar "xyz" "x"))))

;; base-env and resolve

(expect (resolve (PSymbol 0 "d!0!") (hash-bind "d!0!" (EVar "D!0!" nil)))
        (EVar "D!0!" nil))
