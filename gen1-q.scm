;; tests for gen1

(require "core.scm")
(require "parse.scm")  ;; for PError
(require "gen.scm")
(require "escape.scm")
(require "gen1.scm" &private)


;; crumbs

(expect { code: "a b c", errors: [1 2], x: ["!=%"] }
        (crumb-extract (concat "a b"
                               (crumb "errors" 1)
                               " "
                               (crumb "errors" 2)
                               "c"
                               (crumb "x" "!=%")
                               )))

(define `msg "), \n \t ~3 $- $(call $1,2)")
(expect [msg] (dict-get "x" (crumb-extract (crumb "x" msg))))
(expect [msg] (dict-get "x" (crumb-extract
                             (c1-Lambda (protect-trim (crumb "x" msg))))))


;; IString: literal values

(expect " x "
        (c1 (IString " x ")))

(expect "xyz"
        (c1 (IString "xyz")))

(expect (escape "$")
        (c1 (IString "$")))

(expect "$(info $(if ,,,))"
        (c1 (IBuiltin "info" [(IString ",")])))

;; IConcat: concatenate values

(expect "abc"
        (c1 (IConcat [ (IString "a") (IString "b") (IString "c") ])))

;; V: variable reference

(expect "$a"
        (c1 (IVar "a")))

(expect "$(foo)"
        (c1 (IVar "foo")))

;; IBuiltin: call Make builtin function

(expect "$(value FUNC)"
        (c1 (IBuiltin "value" [ (IString "FUNC") ])))

(expect "$(info a)"
        (c1 (IBuiltin "info" [ (IString "a") ])))

;; and & or:  protect against trimming

(expect "$(or $(if ,, a ))"
        (c1 (IBuiltin "or" [ (IString " a ") ])))

(expect "$(and $(if ,,\na\n))"
        (c1 (IBuiltin "and" [ (IString "\na\n") ])))

;; ICall: call user-defined function by name

(expect "$(call fn)"
        (c1 (ICall "fn" [] )))

(expect "$(call fn,1)"
        (c1 (ICall "fn" [ (IString 1) ])))

;; many args
(expect "$(call fn,1,2,3,4,5,6,7,8,9 a b!0)"
        (c1 (ICall "fn" (for s "1 2 3 4 5 6 7 8 9 a b!0" (IString s)))))

(expect "$(call fn,1,2,3,4,5,6,7,8,9 $] $(call ^d,$v))"
        (c1 (ICall "fn" (conj (for s "1 2 3 4 5 6 7 8 9 )" (IString s))
                             (IVar "v")))))

;; i-8

(expect 0 (i-8 8))
(expect 1 (i-8 9))
(expect 2 (i-8 10))
(expect 82 (i-8 90))

;; IArg: value of local variable

(expect (c1-IArg 3 ".") "$3")
(expect (c1-IArg 3 "..") "$-(call ^E,$-3)")
(expect (c1-IArg 3 "...") "$--(call ^E,$--3,`)")

(expect (c1-IArg 9 ".")   "$(call ^n,1,$9)")
(expect (c1-IArg 9 "..")  "$-(call ^E,$-(call ^n,1,$-9))")
(expect (c1-IArg 9 "...") "$--(call ^E,$--(call ^n,1,$--9),`)")

(expect (c1-IArg "3+" ".")  "$(foreach N,3,$(^v))")
(expect (c1-IArg "3+" "...")  "$--(call ^E,$--(foreach N,3,$--(^v)),`)")
(expect (c1-IArg "9+" ".")  "$9")
(expect (c1-IArg "10+" ".")  "$(wordlist 2,99999999,$9)")
(expect (c1-IArg "10+" "...")  "$--(call ^E,$--(wordlist 2,99999999,$--9),`)")

(expect (c1-IArg "=x" ".")  "$(x)")
(expect (c1-IArg "=x" "..")  "$-(call ^E,$-(x))")


;; Funcall: call an anonymous function

(expect "$(call ^Y,,,,,,,,,,$1)"
        (c1 (IFuncall [ (IArg 1 ".") ] )))

(expect "$(call ^Y,a,,,,,,,,,$1)"
        (c1 (IFuncall [ (IArg 1 ".") (IString "a") ])))

(expect "$(call ^Y,a,b,c,d,e,f,g,h,i j,$1)"
        (c1 (IFuncall (cons (IArg 1 ".")
                           (for s "a b c d e f g h i j"
                                (IString s))))))

;; Block: a sequence of expressions

(expect "$(and X1,$[)"
        (c1 (IBlock [ (IString "X") (IString "(") ])))
(expect "X"
        (c1 (IBlock [ (IString "X") ])))


;; Lambda: construct an anonymous function
;;    (lambda (args...) body) -->  (ILambda BODY)

(expect "$``"                (c1 (ILambda (IString "$"))))
(expect "$`1"                (c1 (ILambda (IArg 1 "."))))
(expect "$(call ^E,$3)"      (c1 (ILambda (IArg 3 ".."))))
(expect "$-(call ^E,$-3,`)"  (c1 (ILambda (IArg 3 "..."))))

;; c1-E

(expect [(PError 0 "message")]
        (dict-get "errors"
                  (crumb-extract (c1 (IBuiltin "wildcard"
                                               [(PError 0 "message")])))))

;; c1-file-set and c1-file-fset

(expect "x := $  $` \n"
        (c1-file-set "x" " $` "))

(expect "x := \\#$'\\\n"
        (c1-file-set "x" "#\n\\"))

(expect "x$\" := \\#\n"
        (c1-file-set "x#" "#"))

(expect "f = $\n"
        (c1-file-fset "f" "$`"))  ;; "$`" expands to "$" == $(value f)

(expect (concat "$(call " "^fset" ",f,$(foo))\n")
        (c1-file-fset "f" "$(foo)"))

(expect "define f\n $1\n$2 \nendef\n"
        (c1-file-fset "f" " $`1\n$`2 "))

(expect "define f\n$   define\n$ endef\nendef\n"
        (c1-file-fset "f" "  define\nendef"))

;; c1-file

;; (ICall "^set" ...)  -->   c1-file-set
(expect "a := A\n"
        (c1-file (ICall "^set" [ (IString "a") (IString "A") ])))

;; (ICall "^fset" ...)  -->  c1-file-fset
(expect "a = A\n"
        (c1-file (ICall "^fset" [ (IString "a") (IString "A") ])))

;; (IBuiltin "call" (IString S)) -->  (ICall S ...)
(expect "a := A\n"
        (c1-file (IBuiltin "call"
                          [ (IString "^set") (IString "a") (IString "A") ])))

;; (IBuiltin "eval" (IString TEXT))  -->  TEXT
(expect "a=1\nb=2\n"
        (c1-file (IBuiltin "eval" [ (IString "a=1\nb=2") ])))

;; (IBlock ...) -> still in file mode
(expect "X\n"
        (c1-file (IBlock [ (IBuiltin "eval" [ (IString "X") ]) ])))

;; non-void function
(expect "$(if $(shell ls),)\n"
        (c1-file (IBuiltin "shell" [ (IString "ls") ])))

;; void function
(expect "$(error hi)\n"
        (c1-file (IBuiltin "error" [ (IString "hi") ])))

;; gen1

(define (gen-out node is-file)
  (dict-get "code" (gen1 [ node ] is-file)))

(expect "x := 1\n"
        (gen-out (ICall "^set" [ (IString "x") (IString 1) ]) 1))

(expect "$(call ^set,x,1)"
        (gen-out (ICall "^set" [ (IString "x") (IString 1) ]) nil))

;; Ensure markers are not confused with RHS literals in file syntax.
(expect {code: "$(call ^fset,f,$`.{ERR)\n"}
        (gen1 [ (ICall "^fset" [ (IString "f") (IString "$.{ERR") ]) ]
              1))

(expect [(PError 1 "MSG")]
        (dict-get "errors" (gen1 [(PError 1 "MSG")] 1)))
