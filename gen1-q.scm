;; tests for gen1

(require "core")
(require "parse")
(require "escape")
(require "gen")
(require "gen0")
(require "gen1")

;;--------------------------------

(define (C1 text env)
  (c1 (first (gen0 (parse-text text) env))))

;; parse and compile first expression to exe
(define (CX text env is-file)
  (nth 2 (gen1 (gen0 (parse-text text)) is-file)))

;; parse and compile to file syntax
(define (CXT text env)
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

(expect "$(call fn,1,2,3,4,5,6,7,8,9 $(\\R) $(call ^d,$v))"
        (c1 ["f" "fn" "Q 1" "Q 2" "Q 3" "Q 4" "Q 5"
             "Q 6" "Q 7" "Q 8" "Q 9" "Q )" "V v"]))


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

(expect "$(and X1,$(\\L))"
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

(expect "$$(call ^e,$$1,2)"    (c1 ["X" ["X" ["X" ["R" "($.^^=1,2)"]]]]))
(expect "$$$$(call ^e,$$$$1)"  (c1 ["X" ["X" ["X" ["R" "($.^=1)"]]]]))
(expect "$$$$$$$$1"            (c1 ["X" ["X" ["X" ["R" "$1"]]]]))


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
            (hash-bind "print" ["F" "print"]
                  (hash-bind "^set" ["F" "^set"]))))

(expect "a$$b := A$$B\n"
        (c1-file ["f" "^set" "Q a$b" "Q A$B"]))

;;--------------------------------------------------------------
;; end-to-end tests

(expect "$$" (CX "\"$\""))
(expect "@"  (CX "\"@\""))
(expect "$(or $(if ,, A ))" (CX "(or \" A \")"))
(expect "$(call ^Y,3,5,,,,,,,,$$1$$2)" (CX "(\"$1$2\" 3 5)"))

(expect "$(@G@1H)" (CX "(declare @G@1H) @G@1H"))

;;--------------------------------------------------------------
;; c1-demote
;;(expect "Q a" (c1-demote "Q a"))
;;(expect "Q !10" (c1-demote "Q !0"))
;;(expect "Q $" (c1-demote "Q $"))
;;;(expect ["F" "Q call" "Q demote" "F Q!0xx"] (c1-demote "F Q!0xx"))
;;(expect "Q ab" (c1-concat "Q!0a Q!0b"))
;;(expect "C Q!0ab F!0f Q!0cd" (c1-concat "Q!0a Q!0b F!0f Q!0c Q!0d"))
;;--------------------------------------------------------------


;; c1-E

(expect ["E.1 undef"]
        (gen-extract (c1 ["F" "wildcard" "E.1 undef"])))


;; c1-file
(expect "x := crank\n"
    (CXT "(declare (^set a b)) (^set \"x\" \"crank\")"))

(expect "$(if $(call ^set,x,1,$(info 2)),)\n"
    (CXT "(declare (^set a b)) (^set \"x\" 1 (info 2))"))

;; discard return values in file syntax

;; non-void function
(expect "$(if $(shell ls),)\n" (CXT "(shell \"ls\")"))

;; void function
(expect "$(error hi)\n" (CXT "(error \"hi\")"))

;; eval of literal
(expect "x=$1\n"
        (CXT "(eval \"x=$1\")"))


;;--------------------------------------------------------------
;; assignments vs. expressions & escaping
;;--------------------------------------------------------------

;; c1-set and c1-fset
(expect "x := $  $$ \n"
        (c1-set "x" " $$ "))

(expect "f = $\n"
        (c1-fset "f" "$$"))  ;; "$$" expands to "$" == $(value f)

(expect "$(call ^fset,f,$(foo))\n"
        (c1-fset "f" "$(foo)"))

(expect "define f\n $1\n$2 \nendef\n"
        (c1-fset "f" " $$1\n$$2 "))

(expect "define f\n$   define\n$ endef\nendef\n"
        (c1-fset "f" "  define\nendef"))

(expect "f = $$\n"
        (CXT "(call \"^fset\" \"f\" \"$$\")"))

(expect "$(call ^fset,f,$(words 1))\n"
        (CXT "(call \"^fset\" \"f\" (words 1))"))

(expect "$(call ^fset,f,$(*args*))\n"
        (CXT "(call \"^fset\" \"f\" *args*)"))

(expect "x := \\#$(\\n)\\\n"
        (CXT "(call \"^set\" \"x\" \"#\n\\\\\")"))

(expect "h$(\\H) := \\#\n"
        (CXT "(call \"^set\" \"h#\" \"#\")"))

(expect "$(info #)\n"
        (CXT "(info \"#\")"))

(foreach SCAM_DEBUG "-"
  (expect "$(call f,1)" (CX "(declare (f)) (f 1)")))
