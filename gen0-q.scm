;; Tests for gen0
(require "core")
(require "io")
(require "parse")
(require "gen")
(require "gen0" &private)

;;--------------------------------
;; utilities

;; Parse Tree constructors
(define `(PSym s) ["S" s])
(define `(PString s) ["Q" s])
(define (PList* ...) (cons "L" *args*))


;; Trim ".INDEX" suffixes from a vector of forms.
(define (trim-indices forms)
  (for form forms
       (append (word 1 (subst "." " " (word 1 form)))
               (if (type? "L% `% '% ,% @%" form)
                   (trim-indices (rest form))
                   (rest form)))))

;; Parse SCAM and trim position data.  Return vector of forms.
;;
(define (pp text)
  (trim-indices (parse-text text)))

;; Parse and trim position data; return ONE form.
;;
(define (p1 text)
  (let ((o (pp text)))
    (expect "" (word 2 o))
    (first o)))

;; perform pre-order traversal of a tree of IL nodes
;;
(define (il-visit-x node fn)
  (define `children
    (case node
      ((Builtin name args) args)
      ((Call name args) args)
      ((Funcall nodes) nodes)
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


;; compile text to IL; return IL (unless 'k' is specified as non-nil)
(define (C0 text env)
  (c0-block (trim-indices (parse-text text)) (or env base-env)))

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

;; Q: string
(expect (String " x ")
        (c0 (PString " x ")))

;; S: global data variable
(expect (Var "DATA")
        (c0 (PSym "v!")
            (hash-bind "v!" (EVar "DATA" "."))))

;; S: global function variable
(expect (Builtin "value" [ (String "FUNC") ])
        (c0 (PSym "var")
            (hash-bind "var" (EFunc "FUNC" "." nil))))

;; S: undefined variables
(expect ["E.13" "undefined variable \"foo\""]
        (c0 ["S.13" "foo"]))

;; S: record

(expect (Lambda
         (Concat [ (String "!:T0") (String " ") (Call "^d" [ (Local 1 0) ])
                   (String " ") (Local 2 0) (String " ") (Local 3 0) ]))
        (c0 (PSym "C")
            (hash-bind "C" (ERecord "S W L" "." "!:T0"))))

;; L: (functionvar ...)

(expect (Call "FUNC" [ (String 7) ])
        (c0 (PList* (PSym "var") (PString 7))
            (hash-bind "var" (EFunc "FUNC" "." nil))))

(expect (Call "NAME!1" [ (String 7) ])
        (c0 (PList* (PSym "var") (PString 7))
            (hash-bind "var" (EFunc "NAME!1" "." nil))))

;; L: (inlinefunc ...)

(expect (Builtin "filter" [ (String 1) (Var "oldG") ])
        (c0 (PList* (PSym "f") (PString 1))
            (append
             (hash-bind "g" (EVar "newG" "."))
             (hash-bind "f" (EFunc "FN" "." [ "a" (p1 "(filter a g)") ]))
             (hash-bind "g" (EVar "oldG" "."))
             base-env)))

;; L: (datavar ...)

(expect (Funcall [ (Var "DATA") (String 7) ])
        (c0 (PList* (PSym "var") (PString 7))
            (hash-bind "var" (EVar "DATA" "."))))

;; L: (record ...)

(expect (Concat
         [(String "!:D0") (String " ") (String "1") (String " ") (String "2")])
        (c0-record ["L" "S CA" "Q 1" "Q 2"] nil "S L" "!:D0"))

;; L: (<builtin> ...)  with base-env

(expect (Builtin "or" [ (String 7) ])
        (c0 (PList* (PSym "or") (PString 7))
            base-env))

(expect ["E." "\"if\" accepts 2 or 3 arguments, not 1"]
        (c0 (PList* (PSym "if") (PString 1))))

(expect ["E." "\"if\" accepts 2 or 3 arguments, not 4"]
        (c0 (PList* (PSym "if") (PString 1) (PString 1) (PString 1) (PString 1))))

(expect ["E." "undefined symbol: \"bar\""]
        (c0 (PList* (PSym "bar"))))

(expect ["E." "missing function/macro name"]
        (c0 (PList*)))

;; L: (arg ...)

(expect (Funcall [ (Local 1 0) (String 7) ])
        (c0 (p1 "(var 7)")
            (lambda-env ["S var"])))

(define (test-xmacro form) "Q hi")
(expect (String "hi")
        (c0 ["L" "S var" "Q 7"]
            (hash-bind "var" ["X" (global-name test-xmacro) "i"])))


;; L: (lambda NAMES BODY)

;; (lambda (a) v)
(expect (Lambda (Var "DATA"))
        (c0 (p1 "(lambda (a) v)")
            (hash-bind "v" (EVar "DATA" "."))))

;; (lambda (a b) a b)
(expect (Lambda (Block [ (Local 1 0) (Local 2 0) ]))
        (c0 (p1 "(lambda (a b) a b)")))

;; (lambda (a) (lambda (b) a b))
(foreach SCAM_DEBUG "-" ;; avoid upvalue warning
         (expect (Lambda (Lambda (Block [ (Local 1 1) (Local 1 0)])))
                 (c0 (p1 "(lambda (a) (lambda (b) a b))"))))


;; S: local variables

;(expect (Local 3 0) (c0-local "$3" "$"))
;(expect (Local 3 1) (c0-local "$3" "$$"))
;(expect (Local 3 1) (c0-local "$$3" "$$$$"))
;(expect (Local 3 2) (c0-local "$3" "$$$$"))

(expect (Local 3 0) (c0-local ".3" "."))
(expect (Local 3 1) (c0-local ".3" ".."))
(expect (Local 3 1) (c0-local "..3" "..."))
(expect (Local 3 2) (c0-local ".3" "..."))


(expect (append (hash-bind "$" (EMarker ".."))
                (hash-bind "b" (EArg "..1"))
                (hash-bind "$" (EMarker "."))
                (hash-bind "a" (EArg ".1")))
        (lambda-env [(PSym "b")]
                    (lambda-env [(PSym "a")]
                                nil)))

(expect (Local 1 0)
        (c0 (PSym "var")
            (lambda-env ["S var"])))


(expect (append (hash-bind "$" (EMarker "."))
                (hash-bind "a" (EArg ".1"))
                (hash-bind "b" (EArg ".2")))
        (lambda-env [(PSym "a") (PSym "b")]))

;; extended args: 9th -> (nth 1 $9), 10th -> (nth 2 $9), ...
(expect (append (hash-bind "X" (ESMacro (p1 "(call \"^n\" 1 arg9&)") "."))
                (hash-bind "Y" (ESMacro (p1 "(call \"^n\" 2 arg9&)") "."))
                (hash-bind "arg9&" (EArg ".9")))
        (wordlist 10 13
                  (lambda-env (pp "a a a a a a a a X Y"))))


;; S: symbol macro
(expect (Builtin "call" [ (String "^n") (String 1) (Local 9 0) ])
        (c0 (p1 "X")
            (append (lambda-env (pp "a a a a a a a a X Y"))
                    base-env)))

;; ': quote

(expect (String (p1 "(joe bob)"))
        (c0 (p1 "'(joe bob)")))


;; `: quasi-quote

;; A quoted expression *evaluates* to the AST for the quoted expression.
;; ((c1 (c0 ["`" AST]))) -> AST

(define (CQ text)
  (C0 text (append (hash-bind "q" (EIL (String "qsym") "."))
                   (hash-bind "v" (EVar "v" "."))
                   (hash-bind "l" (EIL ["Q" [(PSym "a") "S b"]] ".")))))

(expect (String (p1 "x"))
        (c0 (p1 "`x")))

(expect (String "qsym")
        (CQ "`,q"))

(expect (Var "v")
        (CQ "`,v"))

(expect (Concat [ (String "L S!0a Q!01 ")
                  (Call "^d" [ (Var "v") ]) ])
        (CQ "`(a 1 ,v)"))


;; nested quote/unquote
(begin
  (define `(dd node) (Call "^d" [ node ]))
  (define (cc ...) (Concat *args*))

  (expect (cc (String "L ")
              (dd (cc (String "` ")
                      (dd (cc (String "L S!0a ,!0S!10v ")
                              (dd (cc (String ", ")
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

(expect (String (p1 "(1 a b 2)"))
        (CQ "`(1 ,@l 2)"))

(expect (Concat [ (String (concat (p1 "(1)") " "))
                  (Var "v")
                  (String (concat " " [(p1 "2")])) ])
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

(expect [ [] (PString "&private") ]
        (collect-flags [(PString "&private")]))


;;
;; declare & define
;;
(define env0 (hash-bind "x" "V x"))

;; declare VAR
(expect (hash-bind "var" (EVar (gen-global-name "var") nil))
        (C0env "(declare var)"))
(expect (hash-bind "var" (EVar (gen-global-name "var") "p"))
        (C0env "(declare var &private)"))

;; declare FUNC
(expect (hash-bind "fn" (EFunc (gen-global-name "fn") "." nil))
        (C0env "(declare (fn a b))"))
(expect (hash-bind "fn" (EFunc (gen-global-name "fn") "p" nil))
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
         (hash-bind "M" (EFunc "#" "." ["a" ["L" "S concat" "S a" "S a"]]))]
        (C0both "(define `(M a) (concat a a))"))
(expect [""
         (hash-bind "M" ["F" "#" "p" ["a" ["L" "S concat" "S a" "S a"]]])]
        (C0both "(define `(M a) &private (concat a a))"))

;; define symbol macro
(expect ["" (hash-bind "I" (ESMacro ["L" "S info" "Q 1"] "") env0)]
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
(expect ["attempt to obtain value of compound macro \"M\""]
        (C0err "(define `(M a) (info a)) M"))


;; define inline FUNC
(define gf (gen-global-name "f"))
(define gg (gen-global-name "g"))

(expect (append
         (hash-bind "g" (EFunc gg "." ["x" ["L" "S info" "Q hi"]
                                       ["L" "S info" "S x"] ]))
         (hash-bind "f" (EFunc gf "." ["a b" ["L" "S join" "S a" "S b"]])))

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

 (expect (hash-bind "x" (EVar gx "i"))
         (C0env "(require \"M\")"))

 ;; (require MOD &private)

 (expect (hash-bind "x" (EVar gx nil) (hash-bind "X" (EVar gX "p")))
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

(expect (Block
         [ (Builtin "call"
                    [ (String "^fset")
                      (String gf)
                      (Lambda
                       (Builtin "if" [ (Local 1 0)
                                       (Local 2 0)
                                       (Call gf [ (Local 2 0)
                                                  (String "") ]) ])) ])
           (Builtin "if" [ (String 1) (String 2)
                           (Call gf [ (String 2) (String "") ]) ]) ])
        (C0 "(define (f a b) &inline (if a b (f b \"\")))
             (f 1 2)"))


(print "gen0 ok")
