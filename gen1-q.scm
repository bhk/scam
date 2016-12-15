;; tests for gen1

(require "parse")  ;; for PError
(require "gen1" &private)

;; make-list

(expect "x" (make-list 1 1 "x"))
(expect "" (make-list 2 1 "x"))
(expect "xxxx" (make-list 1 4 "x"))

;; gen-embed / gen-quote

(let& ((msg "), \n \t ~3 $- $(call $1,2)"))
      (expect msg (gen-decode (gen-encode msg)))
      (expect msg (gen-decode (c1-Lambda (protect-trim (gen-encode msg))))))

(let& ((msg "A B ! !0")
       (msg2 " \t\n "))
      (expect [msg] (gen-extract (concat (gen-embed msg) "$x")))
      (expect [msg] (gen-extract (concat " " (gen-embed msg))))
      (expect [msg "" msg2]
              (gen-extract (concat " " (gen-embed msg)
                                   (gen-embed "")
                                   (gen-embed msg2)))))

;; String: literal values

(expect " x "
        (c1 (String " x ")))

(expect "xyz"
        (c1 (String "xyz")))

(expect (escape "$")
        (c1 (String "$")))

(expect "$(info $(if ,,,))"
        (c1 (Builtin "info" [(String ",")])))

;; Concat: concatenate values

(expect "abc"
        (c1 (Concat [ (String "a") (String "b") (String "c") ])))

;; V: variable reference

(expect "$a"
        (c1 (Var "a")))

(expect "$(foo)"
        (c1 (Var "foo")))

;; Builtin: call Make builtin function

(expect "$(value FUNC)"
        (c1 (Builtin "value" [ (String "FUNC") ])))

(expect "$(info a)"
        (c1 (Builtin "info" [ (String "a") ])))

;; and & or:  protect against trimming

(expect "$(or $(if ,, a ))"
        (c1 (Builtin "or" [ (String " a ") ])))

(expect "$(and $(if ,,\na\n))"
        (c1 (Builtin "and" [ (String "\na\n") ])))

;; Call: call user-defined function by name

(expect "$(call fn)"
        (c1 (Call "fn" [] )))

(expect "$(call fn,1)"
        (c1 (Call "fn" [ (String 1) ])))

;; many args
(expect "$(call fn,1,2,3,4,5,6,7,8,9 a b!0)"
        (c1 (Call "fn" (for s "1 2 3 4 5 6 7 8 9 a b!0" (String s)))))

(expect "$(call fn,1,2,3,4,5,6,7,8,9 $] $(call ^d,$v))"
        (c1 (Call "fn" (conj (for s "1 2 3 4 5 6 7 8 9 )" (String s))
                             (Var "v")))))

;; Local: value of local variable

(expect "$3"                   (c1-Local 3 0))
(expect "$-(call ^E,$-3)"      (c1-Local 3 1))
(expect "$--(call ^E,$--3,`)"  (c1-Local 3 2))

;; Funcall: call an anonymous function

(expect "$(call ^Y,,,,,,,,,,$1)"
        (c1 (Funcall [ (Local 1 0) ] )))

(expect "$(call ^Y,a,,,,,,,,,$1)"
        (c1 (Funcall [ (Local 1 0) (String "a") ])))

(expect "$(call ^Y,a,b,c,d,e,f,g,h,i j,$1)"
        (c1 (Funcall (cons (Local 1 0)
                           (for s "a b c d e f g h i j"
                                (String s))))))

;; Block: a sequence of expressions

(expect "$(and X1,$[)"
        (c1 (Block [ (String "X") (String "(") ])))
(expect "X"
        (c1 (Block [ (String "X") ])))


;; Lambda: construct an anonymous function
;;    (lambda (args...) body) -->  (Lambda BODY)

(expect "$``"                (c1 (Lambda (String "$"))))
(expect "$`1"                (c1 (Lambda (Local 1 0))))
(expect "$(call ^E,$3)"      (c1 (Lambda (Local 3 1))))
(expect "$-(call ^E,$-3,`)"  (c1 (Lambda (Local 3 2))))

;; c1-E

(expect [(PError 0 "message")]
        (gen-extract (c1 (Builtin "wildcard"
                                  [(PError 0 "message")]))))

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

;; (Call "^set" ...)  -->   c1-file-set
(expect "a := A\n"
        (c1-file (Call "^set" [ (String "a") (String "A") ])))

;; (Call "^fset" ...)  -->  c1-file-fset
(expect "a = A\n"
        (c1-file (Call "^fset" [ (String "a") (String "A") ])))

;; (Builtin "call" (String S)) -->  (Call S ...)
(expect "a := A\n"
        (c1-file (Builtin "call"
                          [ (String "^set") (String "a") (String "A") ])))

;; (Builtin "eval" (String TEXT))  -->  TEXT
(expect "a=1\nb=2\n"
        (c1-file (Builtin "eval" [ (String "a=1\nb=2") ])))

;; (Block ...) -> still in file mode
(expect "X\n"
        (c1-file (Block [ (Builtin "eval" [ (String "X") ]) ])))

;; non-void function
(expect "$(if $(shell ls),)\n"
        (c1-file (Builtin "shell" [ (String "ls") ])))

;; void function
(expect "$(error hi)\n"
        (c1-file (Builtin "error" [ (String "hi") ])))

;; gen1

(define (gen-out node is-file)
  (nth 2 (gen1 [ node ] is-file)))

(expect "x := 1\n"
        (gen-out (Call "^set" [ (String "x") (String 1) ]) 1))

(expect "$(call ^set,x,1)"
        (gen-out (Call "^set" [ (String "x") (String 1) ]) nil))

;; Ensure markers are not confused with RHS literals in file syntax.
(expect ["" "$(call ^fset,f,$`.{ERR)\n"]
        (gen1 [ (Call "^fset" [ (String "f") (String "$.{ERR") ]) ]
              1))

(expect [(PError 1 "MSG")]
        (nth 1 (gen1 [(PError 1 "MSG")] 1)))
