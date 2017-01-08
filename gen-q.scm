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

;;--------------------------------
;; env-export & env-import
;;--------------------------------

;; A has no imported bindings
(define mod-A-env
  (append (hash-bind "g" (EFunc "F" "x" 1 nil))
          (hash-bind "q" (EFunc "Q" "p" 1 nil))))

(define (harness-get-module-env mod all)
  (expect mod "A")
  (env-import (env-export mod-A-env) nil))


;; no imports to trim
(expect (env-export mod-A-env)
        mod-A-env)

;; reconstruct A (include)
(expect (env-import mod-A-env 1)
        mod-A-env)

;; get public bindinds (import)
(fexpect (env-import mod-A-env nil)
         (hash-bind "g" (EFunc "F" "i" 1 nil)))


;; env-compress

(for s [",.;[]\!1!0!11!10!021!10 !. !@#$%^&*()_+=|}{\\][/.,?><';\":`~,i x!=F!0x v!=V!0x"]
     (expect s (env-expand (env-compress s))))


;; tokenize-key, detokenize-key:

(define `(tok-test env)
  (eq? env (detokenize-key (tokenize-key env))))

(expect 1 (tok-test (hash-bind "a" "a")))
(expect 1 (tok-test (hash-bind "a" "a%a!p!P!%")))
(expect 1 (tok-test (hash-bind "%" "a%a!p!P!%")))


;; env-export & env-import

;; import only public members

(fexpect (import-binding "f" (EFunc "F" "x" 2 nil))
         (hash-bind "f" (EFunc "F" "i" 2 nil)))

(fexpect (import-binding "f" (EFunc "F" "p" 2 nil))
         nil)


(define (export-round-trip env flag)
  (env-import
   (env-parse [ "# comment"
                (subst "\n" "" (env-export-line env))
                "# F F F F F F"])
   flag))


(fexpect (export-round-trip
          (append (hash-bind "f" (EFunc "f" "x" 2 nil))
                  (hash-bind "x" (EVar "X" "x"))
                  (hash-bind "a" (EFunc "fa" "x" 2 ["a b" (PSymbol 0 "a")]))
                  (hash-bind "g" (EFunc "g" "p" 1 nil))  ;; private
                  (hash-bind "g" (EFunc "g" "i" 1 nil))  ;; imported
                  (hash-bind "m" (EIL "" "x" NoOp))
                  (hash-bind "a:n\n,x" (EVar "xyz" "x")))
          nil)

         (append (hash-bind "f" (EFunc "f" "i" 2 nil))
                 (hash-bind "x" (EVar "X" "i"))
                 (hash-bind "a" (EFunc "fa" "i" 2 ["a b" (PSymbol 0 "a")]))
                 (hash-bind "m" (EIL "" "i" NoOp))
                 (hash-bind "a:n\n,x" (EVar "xyz" "i"))))


;; import public AND private members

(let-global
 ((get-module-env harness-get-module-env))          ;; hook get-module-env

 (define mod-C-env
   (append (hash-bind "f" (EFunc "f" "x" 2 nil))
           (hash-bind "x" (EVar "X" "x"))
           ;; other definitions in this module
           (hash-bind "g" (EFunc "g" "p" 1 nil))  ;; private
           (hash-bind "a:n\n,x" (EVar "xyz" "x"))))

 (fexpect (export-round-trip mod-C-env 1)
          mod-C-env))

;; base-env and resolve

(expect (resolve (PSymbol 0 "d!0!") (hash-bind "d!0!" (EVar "D!0!" nil)))
        (EVar "D!0!" nil))
