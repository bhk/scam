;; tests for gen1

(require "core")
(require "parse")
(require "escape")
(require "gen")
(require "gen0")
(require "gen1")


;; Expand "^..." to namespaced version *within* STR
;;
;(define `(ns str)
;  (subst "^" (gen-global-name "^") str))


;;--------------------------------

;; Parse and compile first expression in `text`.  Returns "gen-coded"
;; result.
(define (C1 text env)
  (c1 (first (gen0 (parse-text text) (or env base-env)))))

;; Parse and compile `text`, returning just the `exe` portion.
(define (CX text env is-file)
  (nth 2 (gen1 (gen0 (parse-text text) (or env base-env)) is-file)))

;; Parse and compile `text` to file syntax.
(define (CXF text env)
  (CX text env 1))

;;--------------------------------

;; gen-embed / gen-quote

(expect ["Hello, world!($)"] (gen-extract (gen-embed "Hello, world!($)")))

(expect ["!0!10 x" " x\t\n"]
        (gen-extract (concat-vec (for s ["!0!10 x" " x\t\n"] (gen-embed s)))))

(expect ["Hello, world!($)"]
        (gen-extract (concat "$(or " (gen-embed "Hello, world!($)") "x)")))
(expect ["Hello, world!($)"]
        (gen-extract
         (protect-arg (concat ")" (gen-embed "Hello, world!($)")))))
(expect ["Hello, world!($)"]
        (gen-extract
         (gen-escape-lambda (concat ")" (gen-embed "Hello, world!($)")))))


;;--------------------------------


;; Q: literal strings

(expect " x "
        (c1 ["Q" " x "]))

(expect "xyz"
        (c1 ["Q" "xyz"]))

(expect "$$"
        (c1 ["Q" "$"]))

(expect "$(info $(if ,,,))"
        (c1 ["F" "info" ["Q" ","]]))

;; V: variable reference

(expect "$a"
        (c1 "V a"))

(expect "$(foo)"
        (c1 "V foo"))


;; F: buitin function call

(expect "$(value FUNC)"
        (c1 ["F" "value" "Q FUNC"]))

(expect "$(info a)"
        (c1 ["F" "info" "Q a"]))

;; and & or:  protect against trimming

(expect "$(or $(if ,, a ))"
        (c1 ["F" "or" ["Q" " a "]]))

(expect "$(and $(if ,,\na\n))"
        (c1 ["F" "and" ["Q" "\na\n"]]))

;; ...and execute compiled `and` code
(expect "2 " ((C1 "(and 1 \"2 \")")))
(expect "\n2" ((C1 "(and 1 \"\n2\")")))
(expect "2\n" ((C1 "(and 1 \"2\n\")")))


;; f: user function call

(expect "$(call fn)"
        (c1 ["f" "fn"]))

(expect "$(call fn,1)"
        (c1 ["f" "fn" "Q 1"]))

;; many args
(expect "$(call fn,1,2,3,4,5,6,7,8,9 a b!0)"
        (c1 ["f" "fn" "Q 1" "Q 2" "Q 3" "Q 4" "Q 5"
             "Q 6" "Q 7" "Q 8" "Q 9" "Q a" "Q b!0"]))

(expect "$(call fn,1,2,3,4,5,6,7,8,9 $] $(call ^d,$v))"
        (c1 ["f" "fn" "Q 1" "Q 2" "Q 3" "Q 4" "Q 5"
             "Q 6" "Q 7" "Q 8" "Q 9" "Q )" "V v"]))


;; U: up-values (or local arguments)

(expect "$3"          (c1-U "U 3 0"))
(expect "($.^=3)"     (c1-U "U 3 1"))
(expect "($.^^=3,2)"  (c1-U "U 3 2"))


;; Y: anonymous function call

(expect "$(call ^Y,,,,,,,,,,$1)"
        (c1 ["Y" "R $1"]))

(expect "$(call ^Y,a,,,,,,,,,$1)"
        (c1 ["Y" "R $1" "Q a"]))

(expect "$(call ^Y,a,b,c,d,e,f,g,h,i j,$1)"
        (c1 ["Y" "R $1" "Q a" "Q b" "Q c" "Q d" "Q e"
             "Q f" "Q g" "Q h" "Q i" "Q j" ]))


;; C: concatenate

(expect "abc"
        (c1 ["C" "Q a" "Q b" "Q c"]))


;; R: raw object code

(expect "a$ b"
        (c1 ["R" "a$ b"]))


;; B: sequence

(expect "$(and X1,$[)"
        (c1 ["B" ["Q" "X"] ["Q" "("]]))


;; X: nested function

;; (lambda (args...) body) -->  ["X" <form>]
;;
;; ["X" exp] is similar to ["Q" (c1 exp)], because the nested lambda
;; expression will evaluate (un-escape) to a function body.  So, "$$" and
;; "$1" references in (c1 exp) become "$$$$" and "$$1".
;;
;; (info (lambda (x) (concat "$," x))) --> $(info $$$$,$$1) ==> "$$,$1"
;;
;; But it is not exactly the same, since the function body can reference
;; upvalues (captured variables), which do NOT get quoted. Instead,
;; "escaping" turns these references -- eg. "($.^=1)" -- into code that
;; escapes the variable at runtime -- e.g. "$(call escape,$1)".
;;

(expect "$$$$"                 (c1 ["X" "Q $"]))
(expect "$$a"                  (c1 ["X" "V a"]))
(expect "$$(value FUNC)"       (c1 ["X" ["F" "value" "Q FUNC"]]))
(expect "$$1"                  (c1 ["X" ["R" "$1"]]))

(expect "$$(call ^e,$$1)"      (c1 ["X" ["X" ["R" "($.^=1)"]]]))
(expect "$$$$1"                (c1 ["X" ["X" ["R" "$1"]]]))

(expect "$$(call ^e,$$1,2)"         (c1 ["X" ["X" ["X" ["R" "($.^^=1,2)"]]]]))
(expect "$$$$(call ^e,$$$$1)"       (c1 ["X" ["X" ["X" ["R" "($.^=1)"]]]]))
(expect "$$$$$$$$1"                 (c1 ["X" ["X" ["X" ["R" "$1"]]]]))


;;--------------------------------------------------------------
;; block-level expressions
;;--------------------------------------------------------------

;; begin

(expect "$(and xyz1,pdq1,yui)"  ; "$(if xyz,)$(if pdq,)yui"
        (C1 "(begin \"xyz\" \"pdq\" \"yui\")"))

(expect "$(if 1,2,yuf)"
        (C1 "(if 1 2 (begin \"yuf\"))"))

(expect "$(and $(info a)1,$(call print,hi)1,$(call ^set,var,val))"
        (C1 "(begin (info \"a\") (print \"hi\") (^set \"var\" \"val\"))"
            (append (hash-bind "print" ["F" "print"])
                    (hash-bind "^set" ["F" "^set"])
                    base-env)))

(expect "a$$b := A$$B\n"
        (c1-file ["f" "^set" "Q a$b" "Q A$B"]))

;;--------------------------------------------------------------
;; end-to-end tests

(expect "$$" (CX "\"$\""))
(expect "@"  (CX "\"@\""))
(expect "$(or $(if ,, A ))" (CX "(or \" A \")"))
(expect "$(call ^Y,3,5,,,,,,,,$$1$$2)" (CX "(\"$1$2\" 3 5)"))

(expect (concat "$("
                (gen-global-name "@G@1H")
                ")")
        (CX "(declare @G@1H) @G@1H"))


;; c1-E

(expect ["E.1 undef"]
        (gen-extract (c1 ["F" "wildcard" "E.1 undef"])))


;; c1-file
(expect "x := crank\n"
    (CXF "(declare (^set a b) &global) (^set \"x\" \"crank\")"))

(expect "$(if $(call ^set,x,1,$(info 2)),)\n"
    (CXF "(declare (^set a b) &global) (^set \"x\" 1 (info 2))"))

;; discard return values in file syntax

;; non-void function
(expect "$(if $(shell ls),)\n" (CXF "(shell \"ls\")"))

;; void function
(expect "$(error hi)\n" (CXF "(error \"hi\")"))

;; eval of literal
(expect "x=$1\n"
        (CXF "(eval \"x=$1\")"))


;;--------------------------------------------------------------
;; assignments vs. expressions & escaping
;;--------------------------------------------------------------

;; c1-set and c1-fset
(expect "x := $  $$ \n"
        (c1-file-set "x" " $$ "))

(expect "f = $\n"
        (c1-file-fset "f" "$$"))  ;; "$$" expands to "$" == $(value f)

(expect (concat "$(call " "^fset" ",f,$(foo))\n")
        (c1-file-fset "f" "$(foo)"))

(expect "define f\n $1\n$2 \nendef\n"
        (c1-file-fset "f" " $$1\n$$2 "))

(expect "define f\n$   define\n$ endef\nendef\n"
        (c1-file-fset "f" "  define\nendef"))

(expect "f = $$\n"
        (CXF (concat "(call \"" "^fset" "\" \"f\" \"$$\")")))

(expect (concat "$(call " "^fset" ",f,$(words 1))\n")
        (CXF (concat "(call \"" "^fset" "\" \"f\" (words 1))")))

(expect (concat "$(call " "^fset" ",f,$(" "^av" "))\n")
        (CXF (concat "(call \"" "^fset" "\" \"f\" *args*)")))

(expect "x := \\#$!\\\n"
        (CXF (concat "(call \"" "^set" "\" \"x\" \"#\n\\\\\")")))

(expect "h$& := \\#\n"
        (CXF (concat "(call \"" "^set" "\" \"h#\" \"#\")")))

(expect "$(info #)\n"
        (CXF "(info \"#\")"))

(foreach SCAM_DEBUG "-"
  (expect "$(call f,1)" (CX "(declare (f) &global) (f 1)")))
