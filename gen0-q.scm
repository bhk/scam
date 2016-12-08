;; Tests for gen0
(require "core")
(require "io")
(require "parse")
(require "gen")
(require "gen0" &private)

;;--------------------------------
;; utilities

;; Remove all position markers from an environment, IL, or AST node.
;;
(define (postrim node)
  (foreach
   w node
   (if (findstring "!=" w)
       (hash-bind (hash-key w) (postrim (hash-value w)))
       (if (or (findstring "!1" w)
               (findstring "!0" w))
           (demote (postrim (promote w)))
           (if (filter "1% 2% 3% 4% 5% 6% 7% 8% 9%" (word 2 (subst "." " " w)))
               (word 1 (subst "." " " w))
               w)))))

;; perform pre-order traversal of a tree of IL nodes
;;
(define (il-visit-x node fn)
  (define `children
    (case node
      ((Builtin name args) args)
      ((Call name args) args)
      ((Funcall name args) args)
      ((Concat nodes) nodes)
      ((Block nodes) nodes)
      ((Lambda node) [node])))

   (if node
       (append (fn node)
               (append-for c children
                           (il-visit-x c fn)))))

(define (il-visit node fn)
  (strip-vec (il-visit-x node fn)))


;; Extract errors from an IL node
(define (il-errors node)
  (il-visit node (lambda (node)
                   (if (error? node)
                       (word 2 node)))))


;; parse and trim position data
;;
(define (pp text)
  (postrim (parse-text text)))

;; compile text to IL; return IL (unless 'k' is specified as non-nil)
(define (C0 text env)
  (postrim (c0-block (parse-text text) (or env base-env))))

;; compile text to IL; return final env (or error)
(define (C0env text env)
  (c0-block-cc (pp text) env (lambda (nodes env) env)))

;; compile text to IL; return [code env]
(define (C0both text env)
  (c0-block-cc (pp text) env (lambda (nodes env) [nodes env])))

;; compile text to IL; return error strings
(define (C0err text env)
  (il-errors (c0-block (parse-text text) (or env base-env))))


;;--------------------------------

;; bind-sym

(expect (hash-bind "f" ["F" (gen-global-name "f") "p"] "...")
        (bind-sym "..." "S f" "F" ["S &private"]))

(expect (hash-bind "f" ["F" "f"] "...")
        (bind-sym "..." "S f" "F" ["S &global"]))

(expect (hash-bind "f" ["F" "f" "" "DEFN"] "...")
        (bind-sym "..." "S f" "F" ["S &global"] "DEFN"))


;; Q: string
(expect (String " x ")
        (c0 ["Q" " x "]))


;; S: global data variable
(expect (Var "DATA")
        (c0 ["S" "v!"] (hash-bind "v!" ["V" "DATA"])))


;; S: global function variable
(expect (Builtin "value" [ (String "FUNC") ])
        (c0 ["S" "var"] (hash-bind "var" ["F" "FUNC"])))


;; S: undefined variables
(expect ["E.13" "undefined variable: \"foo\""]
        (c0 ["S.13" "foo"]))


;; L: (functionvar ...)
(expect (Call "FUNC" [ (String 7) ])
        (c0 ["L" "S var" "Q 7"] (hash-bind "var" ["F" "FUNC"])))
(expect (Call "NAME!1" [ (String 7) ])
        (c0 ["L" "S var" "Q 7"] (hash-bind "var" ["F" "NAME!1"])))


;; L: (inlinefunc ...)

(expect (Builtin "patsubst" [ (String 1) (String 2) (Var "oldG") ])
        (c0 ["L" "S var" "Q 1" "Q 2"]
            (append
             (hash-bind "g" "V newG")
             (hash-bind "var" ["F" "FNAME" ""
                               [ "a b"                               ; args
                                 [ "L" "S patsubst" "S a" "S b" "S g"]]]) ; body
             (hash-bind "g" "V oldG")
             base-env)))

;; L: (datavar ...)

(expect (Funcall (Var "DATA") [ (String 7) ])
        (c0 ["L" "S var" "Q 7"] (hash-bind "var" ["V" "DATA"])))

;; L: (record ...)

(expect (Concat
         [(String "!:D0") (String " ") (String "1") (String " ") (String "2")])
        (c0-record ["L" "S CA" "Q 1" "Q 2"] nil ["R" "S L" nil "!:D0"]))

;; L: (<builtin> ...)  with base-env

(expect (Builtin "or" [ (String 7) ])
        (c0 ["L" "S or" "Q 7"] base-env))

(expect ["\"if\" accepts 2 or 3 arguments, not 1"]
        (C0err "(if 1)"))

(expect ["\"if\" accepts 2 or 3 arguments, not 4"]
        (C0err "(if 1 2 3 4)"))


;; L: errors
(expect ["undefined symbol: \"bar\""]
        (C0err "(bar)"))
(expect ["missing function/macro name"]
        (C0err "()"))


;; S: local variables

(expect (Local 3 0) (c0-local "$3" "$"))
(expect (Local 3 1) (c0-local "$3" "$$"))
(expect (Local 3 1) (c0-local "$$3" "$$$$"))
(expect (Local 3 2) (c0-local "$3" "$$$$"))

(expect (hash-bind "$" "$ $$"
          (hash-bind "b" ["A" "$$1"]
            (hash-bind "$" "$ $"
              (hash-bind "a" ["A" "$1"]))))
        (lambda-env ["S b"] (lambda-env ["S a"] "")))

(expect (Local 1 0)
        (c0 ["S" "var"] (lambda-env ["S var"])))


(expect (hash-bind "$" "$ $"
              (hash-bind "a" "A $1"
                    (hash-bind "b" "A $2")))
        (lambda-env ["S a" "S b"]))

;; extended args: 9th -> (nth 1 $9), 10th -> (nth 2 $9), ...
(expect (hash-bind "X" ["M" ["L" "S call" ["Q" "^n"] "Q 1" "S arg9&"]]
              (hash-bind "Y" ["M" ["L" "S call" ["Q" "^n"] "Q 2" "S arg9&"]]
                    (hash-bind "arg9&" ["A" "$9"])))
        (wordlist 10 99
                  (lambda-env ["S a" "S a" "S a" "S a" "S a"

                               "S a" "S a" "S a" "S X" "S Y"])))


;; S: symbol macro
(expect (Builtin "call" [ (String "^n") (String 1) (Local 9 0) ])
        (c0 "S X" (append
                   (lambda-env ["S a" "S a" "S a" "S a" "S a"
                                "S a" "S a" "S a" "S X" "S Y"])
                   base-env)))

;; L: (lambda NAMES BODY)

;; (lambda (a) v)
(expect (Lambda (Var "DATA"))
        (c0 ["L" "S lambda" ["L" "S a"] "S v"] (hash-bind "v" ["V" "DATA"])))

;; (lambda (a b) a b)
(expect (Lambda (Block [ (Local 1 0) (Local 2 0) ]))
        (c0 ["L" "S lambda" ["L" "S a" "S b"] "S a" "S b"]))

;; (lambda (a) (lambda (b) a b)))
(foreach SCAM_DEBUG "-" ;; avoid upvalue warning
  (expect (Lambda (Lambda (Block [ (Local 1 1) (Local 1 0)])))
     (c0 ["L" "S lambda" ["L" "S a"]
           ["L" "S lambda" ["L" "S b"]
             "S a" "S b"]])))


;; L: (arg ...)

(expect (Funcall (Local 1 0) [ (String 7) ])
        (c0 ["L" "S var" "Q 7"] (lambda-env ["S var"])))

(define (test-xmacro form) "Q hi")
(expect (String "hi")
        (c0 ["L" "S var" "Q 7"]
            (hash-bind "var" ["X" (global-name test-xmacro) "i"])))


;; ': quote

(expect (String ["L.4" "S.5 joe" "S.7 bob"])
        (c0 ["'.1" ["L.4" "S.5 joe" "S.7 bob"]]))

;; `: quasi-quote

;; A quoted expression *evaluates* to the AST for the quoted expression.
;; ((c1 (c0 ["`" AST]))) -> AST

(define (CQ text)
  (C0 text (append
            (hash-bind "q" ["I" ["Q" "Q q"]])
            (hash-bind "v" "V v")
            (hash-bind "l" ["I" ["Q" ["S a" "S b"]]]))))

(expect (String "S x")
        (CQ "`x"))

(expect (String "Q q")
        (CQ "`,q"))

(expect (Var "v")
        (CQ "`,v"))

(expect (Concat [ (String "L S!0a Q!01")
                  (Call "^d" [ (Var "v") ]) ])
        (CQ "`(a 1 ,v)"))


;; nested quote/unquote
(begin
  (define `(dd node) (Call "^d" [ node ]))
  (define (cc ...) (Concat *args*))

  (expect (cc (String "L")
              (dd (cc  (String "`")
                       (dd  (cc (String "L S!0a ,!0S!10v")
                                (dd (cc (String ",")
                                        (dd (Var "v")))))))))
          (CQ "`( `(a ,v ,,v))")))


;; errors

(expect ["unquote (,) outside of a quasiquoted (`) expression"]
        (il-errors (CQ ",a")))

(expect "E )"
        (CQ "`)"))

(expect "E )"
        (CQ "`,)"))

;; splicing

(expect (String ["L" "Q 1" "S a" "S b" "Q 2"])
        (CQ "`(1 ,@l 2)"))

(expect (Concat [ (String "L Q!01")
                  (Var "v")
                  (String "Q!02") ])
        (CQ "`(1 ,@v 2)"))

;;--------------------------------------------------------------
;; errors
;;--------------------------------------------------------------

(expect 1 (see ["undefined variable"]
               (c0 ["S" "quatloo"])))

(expect 1 (see ["attempt to obtain value of builtin"]
               (c0 ["S" "subst"] (hash-bind "subst" ["B" "subst" ]))))


;;--------------------------------------------------------------
;; block-level expressions
;;--------------------------------------------------------------


(expect [ ["S &private" "S.2 &inline"] "S foo&private"]
        (collect-flags ["S &private" "S.2 &inline" "S foo&private"]))

(expect [ [] "Q &private" ]
        (collect-flags ["Q &private"]))


;;
;; declare & define
;;
(define env0 (hash-bind "x" "V x"))

;; declare VAR
(expect (hash-bind "var" ["V" (gen-global-name "var")])
        (C0env "(declare var)"))
(expect (hash-bind "var" ["V" (gen-global-name "var") "p"])
        (C0env "(declare var &private)"))

;; declare FUNC
(expect (hash-bind "fn" ["F" (gen-global-name "fn")])
        (C0env "(declare (fn a b))"))
(expect (hash-bind "fn" ["F" (gen-global-name "fn") "p"])
        (C0env "(declare (fn a b) &private)"))

;; declare errors
(expect ["missing FORM in (declare FORM); expected a list or symbol"]
        (C0err "(declare)"))
(expect ["too many arguments to (declare ...)"]
        (C0err "(declare foo 7)"))
(expect ["invalid NAME in (declare (NAME...)); expected a symbol"]
        (C0err "(declare (1 a))"))


;; define VAR
(define gx (gen-global-name "x"))
(define gf (gen-global-name "f"))

(expect (Block [ (Builtin "call" [ (String "^set") (String gx) (String 1) ])
                 (Builtin "info" [ (Var gx) ]) ])
        (C0 "(define x 1) (info x)"))

;; define FUNC
(expect (Builtin "call" [ (String "^fset")
                          (String gf)
                          (Lambda (Builtin "join" [ (Local 1 0) (Local 2 0) ])) ])
        (C0 "(define (f a b) (join a b))"))

;; define compound macro
(expect [""
         (hash-bind "M" ["F" "#" "" ["a" ["L" "S concat" "S a" "S a"]]])]
        (C0both "(define `(M a) (concat a a))"))
(expect [""
         (hash-bind "M" ["F" "#" "p" ["a" ["L" "S concat" "S a" "S a"]]])]
        (C0both "(define `(M a) &private (concat a a))"))

;; define symbol macro
(expect ["" (hash-bind "I" ["M" ["L" "S info" "Q 1"] ""] env0)]
        (C0both "(define `I (info 1))" env0))
(expect ["" (hash-bind "I" ["M" "Q 7" "p"] env0)]
        (C0both "(define `I &private 7)" env0))


;; define errors
(expect ["missing FORM in (define FORM ...); expected a list or symbol"]
        (C0err "(define)"))
(expect ["invalid FORM in (define `FORM ...); expected a list or symbol"]
        (C0err "(define `1)"))
(expect ["too many arguments to (define VAR EXPR)"]
        (C0err "(define X 0 1)"))
(expect ["too many arguments to (define `VAR EXPR)"]
        (C0err "(define `X 0 1)"))
(expect ["'&inline' applies only to function definitions"]
        (C0err "(define `M &inline 1)"))
(expect ["'&inline' applies only to function definitions"]
        (C0err "(define X &inline 1)"))

;; define and use symbol macro

(expect (String 3)
        (C0 "(define `X 3) X"))

;; define and use compound macro

(expect (Block [ (Builtin "info" [ (String 2) ])
                 (String 3) ])
        (C0 "(define `(M a) (info a) 3) (M 2)"))
(expect ["attempt to obtain value of compound macro"]
        (C0err "(define `(M a) (info a)) M"))


;; define inline FUNC
(define gf (gen-global-name "f"))
(define gg (gen-global-name "g"))

(expect (append
         (hash-bind "g" ["F" gg "" ["x" ["L" "S info" "Q hi"]
                                    ["L" "S info" "S x"] ]])
         (hash-bind "f" ["F" gf "" ["a b" ["L" "S join" "S a" "S b"]]]))

         (C0env "(define (f a b) &inline (join a b))
               (define (g x) &inline (info \"hi\") (info x))"))



;; define and use inline FUNC
(expect (Block
         [ (Builtin "call" [ (String "^fset")
                             (String gf)
                             (Lambda (Builtin "join" [ (Local 1 0)
                                                       (Local 2 0) ])) ])
           (Builtin "join" [ (String 1) (String 2) ]) ])
        (C0 "(define (f a b) &inline (join a b))  (f 1 2)"))


;;
;; Test macro & inline function exporting/importing
;;

(define gX (gen-global-name "X"))
(define (canned-MIN name)
  (cond
   ((eq "D/M" name) "(declare X &private) (declare x)")
   ((eq "M" name) "(declare X &private) (declare x)")
   ((eq "F" name) "(define X &private 1) (define (f) &inline X)")
   ((eq "CM" name) "(define `(F) &private 3) (define `(G) (F))")
   ((eq "SM" name) "(define `A &private 7) (define `B A)")
   (else (expect "" (concat "Bad module: " name)))))


(define (canned-read-file name)
  (env-export (C0env (canned-MIN name))))

(declare (mod-find name))

(let-global
 ;; Override function for looking up modules (!).
 ((mod-find (lambda (m f) m))
  (read-file canned-read-file)
  (read-lines (lambda (file a b) (wordlist a b (split "\n" (canned-read-file file))))))

 ;; (require MOD)

 (expect (Call "^require" [ (String "M") ])
         (C0 "(require \"D/M\")"))

 (expect (hash-bind "x" ["V" gx "i"])
         (C0env "(require \"M\")"))

 ;; (require MOD &private)

 (expect (hash-bind "x" ["V" gx] (hash-bind "X" ["V" gX "p"]))
         (C0env "(require \"M\" &private)"))

 ;; Verify that IMPORTED inline functions & macros are expanded in their
 ;; original environment (read from their MIN files' exports) so they can
 ;; see private members.

 ;; IMPORTED inline function
 (expect (Block [ (Call "^require" [ (String "F") ])
                  (Var gX) ])
         (C0 "(require \"F\") (f)"))

 ;; IMPORTED compound macro
 (expect (Block [ (Call "^require" [ (String "CM") ])
                  (String 3) ])
         (C0 "(require \"CM\") (G)"))

 ;; IMPORTED symbol macro
 (expect (Block [ (Call "^require" [ (String "SM") ])
                  (String 7) ])
         (C0 "(require \"SM\") B")))


 ;; RECURSIVE INLINE FUNCTION: we should see one level of expansion where it
 ;; is used.

(expect (Block [ (Builtin
                  "call"
                  [ (String "^fset")
                    (String gf)
                    (Lambda
                     (Builtin
                      "if" [ (Local 1 0)
                             (Local 2 0)
                             (Call gf [ (Local 2 0) (String "") ]) ])) ])
                 (Builtin "if" [ (String 1) (String 2)
                                 (Call gf [ (String 2) (String "") ]) ]) ])
        (C0 "(define (f a b) &inline (if a b (f b \"\")))
             (f 1 2)"))


(print "gen0 ok")
