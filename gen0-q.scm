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
       (bind (hash-key w) (postrim (hash-value w)))
       (if (or (findstring "!1" w)
               (findstring "!0" w))
           (demote (postrim (promote w)))
           (if (filter "1% 2% 3% 4% 5% 6% 7% 8% 9%" (word 2 (subst "." " " w)))
               (word 1 (subst "." " " w))
               w)))))

;; perform pre-order traversal of a tree of IL nodes
;;
(define (il-visit-x node fn)
  (let&
   ((children (if (filter "F% f% Y%" (word 1 node))
                  (nth-rest 3 node)
                  (if (filter "C% B% X%" (word 1 node))
                      (nth-rest 2 node)))))
   (if node
       (append (fn node)
               (foreach _c children
                        (il-visit-x (promote _c) fn))))))

(define (il-visit node fn)
  (filter "%" (il-visit-x node fn)))  ;; `strip` without corrupting newlines


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
  (postrim (c0-block (parse-text text) env)))

;; compile text to IL; return final env (or error)
(define (C0env text env)
  (c0-block-cc (pp text) env (lambda (nodes env) env)))

;; compile text to IL; return [code env]
(define (C0both text env)
  (c0-block-cc (pp text) env (lambda (nodes env) [nodes env])))

;; compile text to IL; return error strings
(define (C0err text env)
  (il-errors (c0-block (parse-text text)) env))


;;--------------------------------


;; Q: string
(expect ["Q" " x "] 
        (c0 ["Q" " x "]))


;; S: global data variable
(expect ["V" "DATA"]
        (c0 ["S" "v!"] (bind "v!" ["V" "DATA"])))


;; S: global function variable
(expect ["F" "value" "Q FUNC"]
        (c0 ["S" "var"] (bind "var" ["F" "FUNC"])))


;; S: undefined variables
(expect ["E.13" "undefined variable: \"foo\""]
        (c0 ["S.13" "foo"]))


;; L: (functionvar ...)
(expect ["f" "FUNC" "Q 7"]
        (c0 ["L" "S var" "Q 7"] (bind "var" ["F" "FUNC"])))


;; L: (inlinefunc ...)

(expect ["F" "patsubst" "Q 1" "Q 2" "V oldG"]
        (c0 ["L" "S var" "Q 1" "Q 2"]
            (bind "g" "V newG"
                  (bind "var" ["F" "FNAME" ""
                               [ "a b"                               ; args
                                 [ "L" "S patsubst" "S a" "S b" "S g"]]] ; body
                        (bind "g" "V oldG")))))
                 
;; L: (datavar ...)

(expect ["Y" ["V" "DATA"] "Q 7"]
        (c0 ["L" "S var" "Q 7"] (bind "var" ["V" "DATA"])))


;; L: (<builtin> ...)
(expect ["F" "or" "Q 7"]
        (c0 ["L" "S or" "Q 7"]))

(expect ["wrong number of arguments: \"if\" accepts 2 or 3"]
        (C0err "(if 1)"))

(expect ["wrong number of arguments: \"if\" accepts 2 or 3"]
        (C0err "(if 1 2 3 4)"))


;; L: errors
(expect ["undefined symbol: \"bar\""]
        (C0err "(bar)"))
(expect ["missing function/macro name"]
        (C0err "()"))


;; S: argument

(expect (bind "$" "$ $$"
          (bind "b" ["A" "$$1"]
            (bind "$" "$ $"
              (bind "a" ["A" "$1"]))))
        (lambda-env ["S b"] (lambda-env ["S a"] "")))

(expect ["R" "$1"]
        (c0 ["S" "var"] (lambda-env ["S var"])))


(expect (bind "$" "$ $"
              (bind "a" "A $1"
                    (bind "b" "A $2")))
        (lambda-env ["S a" "S b"]))

;; extended args: 9th -> (nth 1 $9), 10th -> (nth 2 $9), ...
(expect (bind "X" ["M" ["L" "S call" "Q ^n" "Q 1" "S arg9&"]]
              (bind "Y" ["M" ["L" "S call" "Q ^n" "Q 2" "S arg9&"]]
                    (bind "arg9&" ["A" "$9"])))
        (wordlist 10 99
                  (lambda-env ["S a" "S a" "S a" "S a" "S a" 

                               "S a" "S a" "S a" "S X" "S Y"])))


;; S: symbol macro
(expect ["F" "call" "Q ^n" "Q 1" "R $9"]
        (c0 "S X" (lambda-env ["S a" "S a" "S a" "S a" "S a" 
                               "S a" "S a" "S a" "S X" "S Y"])))

;; L: (lambda NAMES BODY)

;; (lambda (a) v)
(expect ["X" ["V" "DATA"]]
   (c0 ["L" "S lambda" ["L" "S a"] "S v"] (bind "v" ["V" "DATA"])))

;; (lambda (a b) a b)
(expect ["X" ["B" ["R" "$1"] ["R" "$2"]]]
   (c0 ["L" "S lambda" ["L" "S a" "S b"] "S a" "S b"]))

;; (lambda (a) (lambda (b) a b)))
(foreach SCAM_DEBUG "-" ;; avoid upvalue warning
  (expect ["X" ["X" ["B" ["R" "($.^=1)"] ["R" "$1"]]]]
     (c0 ["L" "S lambda" ["L" "S a"]
           ["L" "S lambda" ["L" "S b"]
             "S a" "S b"]])))


;; L: (arg ...)

(expect ["Y" "R $1" "Q 7"]
        (c0 ["L" "S var" "Q 7"] (lambda-env ["S var"])))


;; ': quote

(expect ["Q" ["L.4" "S.5 joe" "S.7 bob"]]
        (c0 ["'.1" ["L.4" "S.5 joe" "S.7 bob"]]))

;; `: quasi-quote

;; A quoted expression *evaluates* to the AST for the quoted expression.
;; ((c1 (c0 ["`" AST]))) -> AST

(define (CQ text)
  (C0 text (append
            (bind "q" ["I" ["Q" "Q q"]])
            (bind "v" "V v")
            (bind "l" ["I" ["Q" ["S a" "S b"]]]))))

(expect ["Q" "S x"]
        (CQ "`x"))

(expect ["Q" "Q q"]
        (CQ "`,q"))

(expect ["V" "v"]
        (CQ "`,v"))

(expect [ "C"
          ["Q" "L S!0a Q!01"]
          ["f" "^d" ["V" "v"]]]
        (CQ "`(a 1 ,v)"))


;; nested quote/unquote
(expect [ "C"
          ["Q" "L"]
          ["f" "^d" [ "C"
                      ["Q" "`"]
                      ["f" "^d" [ "C"
                                  ["Q" "L S!0a ,!0S!10v"]
                                  ["f" "^d" [ "C"
                                               ["Q" ","]
                                               ["f" "^d" "V v"]]]]]]]]
        (CQ "`( `(a ,v ,,v))"))


;; errors

(expect ["unquote (,) outside of a quasiquoted (`) expression"]
        (il-errors (CQ ",a")))

(expect "E )"
        (CQ "`)"))

(expect "E )"
        (CQ "`,)"))

;; splicing

(expect ["Q" ["L" "Q 1" "S a" "S b" "Q 2"]]
        (CQ "`(1 ,@l 2)"))

(expect [ "C"
          ["Q" "L Q!01"]
          ["V" "v"] 
          ["Q" "Q!02"]]
        (CQ "`(1 ,@v 2)"))

;;--------------------------------------------------------------
;; errors
;;--------------------------------------------------------------

(expect 1 (see ["undefined variable"]
               (c0 ["S" "quatloo"])))

(expect 1 (see ["attempt to obtain value of builtin"]
               (c0 ["S" "subst"] (bind "subst" ["B" "subst"]))))


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
(define env0 (bind "x" "V x"))

;; declare VAR
(expect (bind "var" "V var")
        (C0env "(declare var)"))
(expect (bind "var" "V var p")
        (C0env "(declare var &private)"))

;; declare FUNC
(expect (bind "fn" "F fn")
        (C0env "(declare (fn a b))"))
(expect (bind "fn" "F fn p")
        (C0env "(declare (fn a b) &private)"))

;; declare errors
(expect ["missing FORM in (declare FORM); expected a list or symbol"]
        (C0err "(declare)"))
(expect ["too many arguments to (declare ...)"]
        (C0err "(declare foo 7)"))
(expect ["invalid NAME in (declare (NAME...)); expected a symbol"]
        (C0err "(declare (1 a))"))


;; define VAR
(expect [ "B"
          ["F" "call" "Q ^set" "Q x" "Q 1"]
          ["F" "info" "V x"] ]
        (C0 "(define x 1) (info x)"))

;; define FUNC
(expect ["F" "call" "Q ^fset" "Q f" ["X" ["F" "join" "R $1" "R $2"]]]
        (C0 "(define (f a b) (join a b))"))

;; define compound macro
(expect [""
         (bind "M" ["F" "#" "" ["a" ["L" "S concat" "S a" "S a"]]])]
        (C0both "(define `(M a) (concat a a))"))
(expect [""
         (bind "M" ["F" "#" "p" ["a" ["L" "S concat" "S a" "S a"]]])]
        (C0both "(define `(M a) &private (concat a a))"))

;; define symbol macro
(expect ["" (bind "I" ["M" ["L" "S info" "Q 1"] ""] env0)]
        (C0both "(define `I (info 1))" env0))
(expect ["" (bind "I" ["M" "Q 7" "p"] env0)]
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

(expect "Q 3"
        (C0 "(define `X 3) X"))

;; define and use compound macro

(expect ["B" ["F" "info" "Q 2"] "Q 3"]
        (C0 "(define `(M a) (info a) 3) (M 2)"))
(expect ["attempt to obtain value of compound macro"]
        (C0err "(define `(M a) (info a)) M"))


;; define inline FUNC
(expect (append
         (bind "g" ["F" "g" "" ["x" ["L" "S info" "Q hi"]
                                    ["L" "S info" "S x"] ]])
         (bind "f" ["F" "f" "" ["a b" ["L" "S join" "S a" "S b"]]]))

         (C0env "(define (f a b) &inline (join a b))
               (define (g x) &inline (info \"hi\") (info x))"))


;; define and use inline FUNC
(expect [ "B" ["F" "call" "Q ^fset" "Q f" ["X" ["F" "join" "R $1" "R $2"]]]
              ["F" "join" "Q 1" "Q 2"] ]
        (C0 "(define (f a b) &inline (join a b))  (f 1 2)"))


;;
;; Test macro & inline function exporting/importing 
;;

(define (canned-MIN name)
  (cond
   ((eq "M" name) "(declare X &private) (declare Y)")
   ((eq "F" name) "(define X &private 1) (define (f) &inline X)")
   ((eq "CM" name) "(define `(F) &private 3) (define `(G) (F))")
   ((eq "SM" name) "(define `A &private 7) (define `B A)")
   (else (expect "" (concat "Bad module: " name)))))


(define (canned-read-file name)
  (env-export (C0env (canned-MIN name))))

(declare (^mod-find modname scamdir))

(let-global
 ;; NOTE: Overriding a runtime function can be tricky because it could
 ;; affect the execution of this test program (not just the code under
 ;; test).  For example, calling `require` within this block would produce
 ;; unexpected results.
 ((^mod-find (lambda (m f p) m))
  (read-file canned-read-file))

 ;; (require MOD)

 (expect ["f" "^require" "Q M"]
         (C0 "(require \"M\")"))

 (expect (bind "Y" "V Y i")
         (C0env "(require \"M\")"))

 ;; (require MOD &private)

 (expect (bind "Y" "V Y" (bind "X" "V X p"))
         (C0env "(require \"M\" &private)"))

 ;; Verify that IMPORTED inline functions & macros are expanded in their
 ;; original environment (read from their MIN files' exports) so they can
 ;; see private members.

 ;; IMPORTED inline function
 (expect ["B" ["f" "^require" "Q F"]
              "V X"]
         (C0 "(require \"F\") (f)"))

 ;; IMPORTED compound macro
 (expect ["B" ["f" "^require" "Q CM"]
              "Q 3"]
         (C0 "(require \"CM\") (G)"))

 ;; IMPORTED symbol macro
 (expect ["B" ["f" "^require" "Q SM"]
              "Q 7"]
         (C0 "(require \"SM\") B")))


 ;; RECURSIVE INLINE FUNCTION: we should see one level of expansion where it
 ;; is used.

(expect ["B" ["F" "call" "Q ^fset" "Q f" ["X" ["F" "if" "R $1" "R $2" ["f" "f" "R $2" ["Q" ""]]]]]
             ["F" "if" "Q 1" "Q 2" ["f" "f" "Q 2" ["Q" ""]]]]
        (C0 "(define (f a b) &inline (if a b (f b \"\")))
             (f 1 2)"))
 

(print "gen0 ok")
