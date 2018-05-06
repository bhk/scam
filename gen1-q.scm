;; tests for gen1

(require "core")
(require "parse")  ;; for PError
(require "gen")
(require "escape")
(require "gen1" &private)


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


;; make-list

(expect "x" (make-list 1 1 "x"))
(expect "" (make-list 2 1 "x"))
(expect "xxxx" (make-list 1 4 "x"))


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

;; Local: value of local variable

(expect "$3"                   (c1-Local 3 0))
(expect "$-(call ^E,$-3)"      (c1-Local 3 1))
(expect "$--(call ^E,$--3,`)"  (c1-Local 3 2))

;; Funcall: call an anonymous function

(expect "$(call ^Y,,,,,,,,,,$1)"
        (c1 (IFuncall [ (ILocal 1 0) ] )))

(expect "$(call ^Y,a,,,,,,,,,$1)"
        (c1 (IFuncall [ (ILocal 1 0) (IString "a") ])))

(expect "$(call ^Y,a,b,c,d,e,f,g,h,i j,$1)"
        (c1 (IFuncall (cons (ILocal 1 0)
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
(expect "$`1"                (c1 (ILambda (ILocal 1 0))))
(expect "$(call ^E,$3)"      (c1 (ILambda (ILocal 3 1))))
(expect "$-(call ^E,$-3,`)"  (c1 (ILambda (ILocal 3 2))))

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
