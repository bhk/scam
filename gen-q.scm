(require "core.scm")
(require "parse.scm")
(require "gen.scm" &private)


;; Return 1 on match; otherwise print message and return nil.
;;
(define (match-perror rec sym-where pattern)
  (define `pos-in
    (if (word 2 sym-where)
        (form-index sym-where)
        sym-where))

  (or (case rec
        ((PError pos desc)
         (and (eq? pos pos-in)
              (findstring pattern desc)
              1)))
      (printf "Expected: %q\n     Got:  %q" (PError pos-in pattern) rec)))


;;
;; IL-related definitions
;;

(expect (il-concat nil) nil)
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


(define _a (PSymbol 1 "a"))
(define _b (PSymbol 2 "b"))
(define _x (PSymbol 5 "x"))
(define _y (PSymbol 6 "y"))
(define _?y (PSymbol 16 "?y"))
(define _=k (PSymbol 19 "=k"))
(define _... (PSymbol 20 "..."))
(define _...z (PSymbol 27 "...z"))
(define _1 (PString 91 1))
(define _2 (PString 92 2))


;; current-depth

(expect "." (depth.l ".;"))
(expect ";" (depth.a ".;"))

(expect "." (current-depth nil))
(expect ".;" (current-depth (depth-marker ".;")))

;; gen-native-name

(let-global ((*is-boot* 1))
  (expect "`a" (gen-native-name "a" nil))
  (expect "a" (gen-native-name "a" "&native")))

(let-global ((*is-boot* nil))
  (expect "'a" (gen-native-name "a" nil))
  (expect "a" (gen-native-name "a" "&native")))

;; gensym

(expect (gensym-name "foo" nil nil)
        "foo&")
(expect (gensym (PSymbol 0 "foo") nil)
        (PSymbol 0 "foo&"))
(expect (gensym (PSymbol 0 "foo")
                {(symbol-name (gensym (PSymbol 0 "foo") nil)): (EVar "p" "x")})
        (PSymbol 0 "foo&1"))

;; gen-error

(expect (gen-error (PString 12 "x") "Msg: %s %s" "hello" "error")
        (PError 12 "Msg: hello error"))


;; check-arity

(define `args2 [_a _b])
(define `argsym (PSymbol 0 "word"))

(expect nil (check-arity "1 2 3" args2 argsym))
(expect nil (check-arity "2+" args2 argsym))
(expect nil (check-arity "0+" [] argsym))
(expect (check-arity "3+" args2 argsym)
        (PError 0 "`word` accepts 3 or more arguments, not 2"))
(expect (check-arity "3" args2 argsym)
        (PError 0 "`word` accepts 3 arguments, not 2"))


;; parse-target

(define ARG1 (IArg 1 "."))

;; Return first PError if any are returned; otherwise return dictionary
;; associating names with their corresponding (xtor ARG1).
;;
(define (check-parse-target form)
  (let ((o (parse-target form)))
    (or (first-perror o)
        (append-for (rec o)
          (case rec
            ((Bind name xtor)
             {=name: (xtor ARG1)}))))))


(define `(expect-pt-error target sym-where pattern)
  (expect 1 (match-perror (check-parse-target target) sym-where pattern)))


;; Invalid targets

(expect-pt-error _?y _?y "'?NAME' can appear only in ")
(expect-pt-error _...z _...z "'...NAME' can appear only as ")
(expect-pt-error _... _... "'...NAME' can appear only as ")
(expect-pt-error (PVec 30 [_?y ]) _?y "'?NAME' can appear only in ")
(expect-pt-error (PDict 40 { =_=k: _?y }) _?y "'?NAME' can appear only in ")
(expect-pt-error (PDict 40 { =_x: _?y }) _?y "'?NAME' can appear only in ")
(expect-pt-error (PDict 40 {(PVec 30 [_x]): _?y}) 30 " keys must be ")
(expect-pt-error (PDict 40 { (PList 50 [_x]): _?y}) 50 " keys must be ")
(expect-pt-error _1 _1 "expected symbol, vector")


;; Valid targets

;; symbol
(expect (check-parse-target _a)
        { a: ARG1 })

;; vector deconstruction
(expect (check-parse-target (PVec 0 [ _x _y ]))
        { x: (il-nth 1 ARG1),
          y: (il-nth 2 ARG1) })

(expect (check-parse-target (PVec 0 [ _x _...z ]))
        { x: (il-nth 1 ARG1),
          z: (il-nth-rest 2 ARG1) })

;; pair deconstruction
(expect (check-parse-target (PDict 0 { =_=k: _x}))
        { k: (il-dict-key ARG1),
          x: (il-dict-value ARG1) })

;; ... with nested vector and "rest" of pairs
(expect (check-parse-target (PDict 0 { =_=k: (PVec 0 [_x]), =_... : _a}))
        { k: (il-dict-key ARG1),
          x: (il-nth 1 (il-dict-value ARG1)),
          a: (il-nth-rest 2 ARG1) })

;; field deconstruction
(expect (check-parse-target (PDict 0 { =_a: _x}))
        { x: (il-dict-get "a" ARG1) })

(expect (check-parse-target (PDict 0 { =_1: _x}))
        { x: (il-dict-get "1" ARG1) })

;; empty dictionary (allow it, I suppose)
(expect (check-parse-target (PDict 0 {}))
        nil)


;; bind-target (and indirectly, bind-nxmap)

(let (({=key: value} (bind-target _1 "x" "." (IString "s"))))
  (expect EnvErrorKey key)
  (expect 1 (match-perror value _1 "expected symbol, ")))


;; base-env and resolve

(expect (resolve (PSymbol 0 "d!0!") { "d!0!": (EVar "p" "D!0!")})
        (EVar "p" "D!0!"))
