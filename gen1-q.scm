;; tests for gen1

(require "core.scm")
(require "parse.scm")  ;; for PError
(require "gen.scm")
(require "escape.scm")
(require "gen1.scm" &private)


;; crumbs

(expect {name: ["!" "!1" "$." "~"]}
        (crumb-decode
         (subst "$.{" nil "$.}" nil (crumb "name" ["!" "!1" "$." "~"]))))

(expect (decrumb (.. "$1$(call " (crumb "fn" ["!1" "$1"]) ",a)") 1)
        { code: (.. "!1' = $1\n"
                    "$1$(call !1',a)") })

(expect (decrumb (.. "$1$(call " (crumb "fn" ["!1" "$1"]) ",a)") nil)
        { code: (.. "$(call ^SF,!1',$`1)"
                    "$1$(call !1',a)") })

(expect (decrumb (.. "$1$(call " (crumb "fn" ["!1" "$1"]) ",a)"
                     "$(call "
                     (crumb "fn" ["!1"
                                  (.. "$(" (crumb "fn" ["f2" "$1"]) ")")])
                     ")"
                     (crumb "errors" "E1") "$3"
                     (crumb "errors" "E2") "$4")
                 1)
        { code: (.. "!1'' = $(!1')\n"
                    "!1' = $1\n"
                    "$1$(call !1',a)$(call !1'')$3$4"),
          errors: ["E1" "E2"] })


;; IString: literal values

(expect " x "
        (c1 (IString " x ")))

(expect "xyz"
        (c1 (IString "xyz")))

(expect (escape "$")
        (c1 (IString "$")))

(expect "$(info $&)"
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

;; "var" pseudo-builtin, for (native-var EXPR)

(expect "$1"
        (c1 (IBuiltin "var" [ (IString "1") ])))

;; Optimize (if CONST ...)

(expect "$1"
        (c1 (IBuiltin "if" [ (IString " ") (IArg 1 ".") (IArg 2 ".") ])))
(expect "$2"
        (c1 (IBuiltin "if" [ (IString "") (IArg 1 ".") (IArg 2 ".") ])))
(expect ""
        (c1 (IBuiltin "if" [ (IString "1") (IString "") (IArg 2 ".") ])))
(expect ""
        (c1 (IBuiltin "if" [ (IString "") (IArg 1 ".") ])))

;; Optimize (if (not COND) ...)

(define `ifnot
   (IBuiltin "if" [ (IVar "v") (IString nil) (IString 1) ]))

(expect "$(if $v,$2,$1)"
        (c1 (IBuiltin "if" [ ifnot (IArg 1 ".") (IArg 2 ".") ])))

(expect "$(if $v,,$1)"
        (c1 (IBuiltin "if" [ ifnot (IArg 1 ".") ])))

;; Retain error crumbs
(expect (crumb "errors" (PError 1 "E"))
        (c1 (IBuiltin "if" [ (IString "")
                             (IConcat [ (PError 1 "E") (ICrumb "x" "y") ]) ])))

;; nested reductions
(expect "$2"
        (c1 (IBuiltin "if"
                      [ (IBuiltin "if" [ (IString "") (IArg 1 ".") ])
                        (IArg 1 ".")
                        (IArg 2 ".") ])))

;; IFor

(expect "$(foreach ;,1,2)"
        (c1 (IFor ";" (IString 1) (IString 2))))

;; and & or:  protect against trimming

(expect "$(or $(if ,, a ))"
        (c1 (IBuiltin "or" [ (IString " a ") ])))

(expect "$(and $(if ,,\na\n))"
        (c1 (IBuiltin "and" [ (IString "\na\n") ])))

;; ICallX: call function by name, with SCAM args and additional args

(expect "$(call fn)"
        (c1 (ICallX "fn" nil nil)))

(expect "$(call fn,1)"
        (c1 (ICallX "fn" [ (IString 1) ] [])))

(expect "$(call fn,,,,,,,,,,10,11)"
        (c1 (ICallX "fn" [] [ (IString 10) (IString 11) ])))

(expect "$(call fn,1,,,,,,,,,10)"
        (c1 (ICallX "fn" [(IString 1)] [ (IString 10) ])))

(expect "$(call fn,1,2,3,4,5,6,7,8,9 a b!0,10,11)"
        (c1 (ICallX "fn" (for (s "1 2 3 4 5 6 7 8 9 a b!0") (IString s))
                    [ (IString 10) (IString 11) ])))

(expect "$(call fn,1,2,3,4,5,6,7,8,9 $] $(call ^d,$v))"
        (c1 (ICall "fn" (conj (for (s "1 2 3 4 5 6 7 8 9 )") (IString s))
                              (IVar "v")))))

;; Hoisted let (ICallX)

(expect (.. "$(call " (crumb "fn" ["f1" "$1"]) ",a)")
        (c1 (ICallX "^X" [ (IString "a") ]
                  [ (ILambda "f1" (IArg 1 ".")) ])))

;; Function call -> variable reference

(expect "$(^d)"
        (c1 (ICallX "^d" [ (IArg 1 ".") ] nil)))


;; Fold (wordlist K,99999999,LIST)...

(define wl3
  (IBuiltin "wordlist" [ (IString 3) (IString 99999999) (IVar "foo") ]))

(expect "$(wordlist 4,99999999,$(foo))"
        (c1 (IBuiltin "wordlist" [ (IString 2) (IString 99999999) wl3 ])))

(expect "$(word 5,$(foo))"
        (c1 (IBuiltin "word" [ (IString 3) wl3 ])))

(expect "$(call ^n,6,$(foo))"
        (c1 (ICall "^n" [ (IString 4) wl3 ])))


;; ICall: call user-defined SCAM function

(expect "$(call fn,1)"
        (c1 (ICall "fn" [ (IString 1) ])))

;; IFuncall: call an anonymous function

(expect "$(call ^X,a,,,,,,,,,$1)"
        (c1 (IFuncall (IArg 1 ".") [ (IString "a") ])))

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

(expect (c1-IArg ";" ".")  "$;")
(expect (c1-IArg ";" "..")  "$-(call ^E,$-;)")
(expect (c1-IArg ";;" "..")  "$-(call ^E,$-(;;))")


;; Block: a sequence of expressions

(expect "$(call ^S,v,x)"
        (c1-void (ICall "^S" [ (IString "v") (IString "x") ])))
(expect "$(if $(call ^S,v,x,$v),)"
        (c1-void (ICall "^S" [ (IString "v") (IString "x") (IVar "v") ])))

(expect "X"
        (c1 (IBlock [ (IString "X") ])))
(expect "$(if $(shell ls),) x "
        (c1 (IBlock [ (IString nil)
                      (IBuiltin "shell" [(IString "ls")])
                      (IString " x ") ])))


;; Lambda: construct an anonymous function
;;    (lambda (args...) body) -->  (ILambda BODY)

(expect "$``"                (c1 (ILambda nil (IString "$"))))
(expect "$`1"                (c1 (ILambda nil (IArg 1 "."))))
(expect "$(call ^E,$3)"      (c1 (ILambda nil (IArg 3 ".."))))
(expect "$-(call ^E,$-3,`)"  (c1 (ILambda nil (IArg 3 "..."))))


;; c1-file-set

(expect "x := $  $` \n"
        (c1-file-set "x" " $` " 1))

(expect "x := \\#$'\\\\#\n"
        (c1-file-set "x" "#\n\\" 1))

(expect "x$\" := \\#\n"
        (c1-file-set "x#" "#" 1))

(expect "f = $\n"
        (c1-file-set "f" "$`" nil))  ;; "$`" expands to "$" == $(value f)

(expect "$(call ^SF,f,$(foo))\n"
        (c1-file-set "f" "$(foo)" nil))

(expect "define f\n $1\n$2 \nendef\n"
        (c1-file-set "f" " $`1\n$`2 " nil))

(expect "define f\n$   define\n$ endef\nendef\n"
        (c1-file-set "f" "  define\nendef" nil))

;; c1-file

;; (ICall "^S" ...)  -->   c1-file-set
(expect "a := A\n"
        (c1-file (ICall "^S" [ (IString "a") (IString "A") ])))

;; (ICall "^SF" ...)  -->  c1-file-fset
(expect "a = A\n"
        (c1-file (ICall "^SF" [ (IString "a") (IString "A") ])))

;; (IBuiltin "call" (IString S)) -->  (ICall S ...)
(expect "a := A\n"
        (c1-file (IBuiltin "call"
                          [ (IString "^S") (IString "a") (IString "A") ])))

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
        (gen-out (ICall "^S" [ (IString "x") (IString 1) ]) 1))

(expect "$(call ^S,x,1)"
        (gen-out (ICall "^S" [ (IString "x") (IString 1) ]) nil))

;; Ensure markers are not confused with RHS literals in file syntax.
(expect "$(call ^SF,f,$`.{ERR)\n"
        (c1-file (ICall "^SF" [ (IString "f") (IString "$.{ERR") ])))

;; But allow a function with crumbs to use file syntax
(expect "f = $.{k!=v$.}\n"
        (c1-file (ICall "^SF" [ (IString "f") (ILambda "f" (ICrumb "k" "v")) ])))

(expect [(PError 1 "MSG")]
        (dict-get "errors" (gen1 [(PError 1 "MSG")] 1)))
