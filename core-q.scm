;;--------------------------------
;; general purpose function tests
;;--------------------------------

(require "core" &private)

(if (eq? 1 2)
    (error "eq not working"))

(if (eq? "1 2" (nth 1 ["1 2"]))
    nil
    (error "promote/demote not working"))

(set-global "GG" "$")
(expect "simple" (flavor "GG"))
(expect "$" (value "GG"))

(set-rglobal "FF" "$")
(expect "recursive" (flavor "FF"))
(expect "$" (value "FF"))

(expect "" (eq? "" "a"))
(expect "" (eq? "a" ""))
(expect "" (eq? "a" "aa"))
(expect "1" (eq? "a" "a"))
(expect "1" (eq? "" ""))

(expect 5678 (identity 5678))

(expect 1 (xor 1 ""))
(expect 2 (xor "" 2))
(expect "" (xor 1 2))

(expect "x A!0B" (conj "x" "A B"))

(expect "c" (last "a b c"))
(expect "a b" (butlast "a b c"))

(expect [[1 2]] (select-vec (lambda (x) (word 2 x)) [[1 2] 3 2]))

(expect "2 23" (select-words (lambda (n) (findstring 2 n)) "1 2 3 23"))

(expect "1 2" (vec-or ["" "1 2" 3]))
(expect "" (vec-or ["" "" "" ""]))

(expect "" (indices ""))
(expect [1 2 3] (indices "a b c"))

(expect "2" (numeric? "2"))
(expect "-1" (numeric? "-1"))
(expect "" (numeric? "+1"))
(expect "" (numeric? "--1"))
(expect "0.1" (numeric? "0.1"))
(expect "21." (numeric? "21."))
(expect "" (numeric? ".1"))
(expect "" (numeric? "+1"))
(expect "" (numeric? "-"))
(expect "" (numeric? "x"))
(expect "" (numeric? "1+"))
(expect "" (numeric? "1.1."))
(expect "" (numeric? "1x0"))
(expect "1e7" (numeric? "1e7"))
(expect "1E7" (numeric? "1E7"))
(expect "1e-7" (numeric? "1e-7"))
(expect "1e+7" (numeric? "1e+7"))
(expect "-1.2e-7" (numeric? "-1.2e-7"))
(expect "" (numeric? "1e"))
(expect "" (numeric? "1e+"))
(expect "" (numeric? "1e-"))
(expect "" (numeric? " 2 "))

(expect 1 (word-index? 1))
(expect "" (word-index? 1.0))
(expect "" (word-index? 1e5))
(expect "" (word-index? -1))
(expect "" (word-index? 0))

(expect "[1 \"a b\"] --> 1 a!0b" (sprintf "%q --> %s" [1 "a b"] [1 "a b"]))
(expect "nada" (sprintf "nada" "ignored"))
(expect "!P!. a !\t !0 x" (sprintf "!P!.%s!0%sx" " a !\t " " " "ignored"))
(expect "a%b%c" (sprintf "a%%b%s" "%c"))
(expect "\"\"A" (sprintf "%qA" ""))
(expect "abc" (sprintf "%s" "abc" "def"))


(expect "" (reverse ""))
(expect [3 2 1] (reverse [1 2 3]))
(expect [11 10 9 8 7 6 5 4 3 2 1] (reverse [1 2 3 4 5 6 7 8 9 10 11]))
(expect ["a\nb" "c\nd"] (reverse "c\nd a\nb"))

(expect "a.b.c" (while (lambda (x) (findstring ".." x))
                  (lambda (x) (subst ".." "." x))
                  "a..b...c"))

(expect "1,2,3" (concat-vec [1 2 3] ","))

;; append

(expect "" (append))
(expect "a b c" (append "a" "" "b" "" "" "c"))
(expect "a b c" (append "" "a" "" "b" "" "c"))
(expect "a b c!1 d!1 e!1" (append "" "" "" "a" "b" "" "" "c!1" "d!1" "e!1"))


;; dictionary functions

(expect "a % c" (dict-key {"a % c": " d % "}))
(expect " d % " (dict-value {"a % c": " d % "}))

(expect "!0!=x" (dict-find " " {" ": "x"}))
(expect "x!=M" (dict-find "x" {x: "M", x: "K"}))

(expect " " (dict-get "" {a: "b", "": " ", x: "y"}))
(expect "b" (dict-get "a" {a: "b", a:2 }))
(expect "%" (dict-get "" {"": "%"} "default"))
(expect "" (dict-get "%" {"%": ""} "default"))
(expect "default" (dict-get "x" {"": ""} "default"))
(expect "val1" (dict-get "x%x" {"x%x": "val1", "x%x": "%"}))

(expect {a:1, c:3} (dict-remove "b" {a:1, b:2, c:3}))

(expect {b:"!", a:1, c:3}
        (dict-set "b" "!" {a:1, b:2, c:3}))

(expect {" ":1, b:2, bb:9}
        (dict-compact {" ":1, b:2, b:7, " ":3, bb:9}))

(expect ["%" "!8" "a b" ""]
        (dict-keys {"%": "", "!8": "x", "a b": "%1", "": "x"}))

(expect ["" "%x"]
        (dict-values {"%": "", "": "%x"}))

(expect { a: [1 2 3], b: ["%" "b c" "! " ""] }
        (dict-collate {a: 1, a: 2, b: "%", b: "b c", b: "! ", a: 3, b: ""}))

;; symbol?

(expect "" (symbol? ""))
(expect "a!" (symbol? "a!"))
(expect "" (symbol? "a!0b"))
(expect "" (symbol? "a(b)"))


;; format

(expect "[\"a\" \" \" [\"\"]]" (format "a !0 !1."))
(expect "\" !0 \"" (format " !0 "))
(expect "\"!x\"" (format "!x"))
(expect "[\" \"]" (format "!0"))
(expect "-12" (format "-12"))
(expect "{a: \"b\"}" (format {a:"b"}))
(expect "{\" \": \"\"}" (format {" ":""}))

;; format-custom

(format-add (lambda (str)
              (if (filter "!-!-%" (word 1 str))
                  (concat "[" str "]"))))

(expect "[!-!-abc]" (format "!-!-abc"))

;; format-record

(let-global
 ((^tags (append ^tags
                 { "!:A": ["CtorA" "W" "L"],
                   "!:B": ["CtorB" "W" "S"],
                   "!:V": ["Void"] })))

 (expect "(CtorA [\" \"] [\" \" 1 2 3])"
         (format "!:A !0 !0 1 2 3"))
 (expect "(CtorB [\"!\"] \"!\")"
         (format "!:B !1 !1"))
 (expect "(CtorA \"x\" [])"
         (format "!:A x "))
 (expect "(Void)"
         (format "!:V")))

;; uniq

(expect "a b c" (uniq "a a b a c a b"))

(expect ["" "abc" ""]   (split "/x/" "/x/abc/x/"))
(expect ["" "" "" ""]   (split "a" "aaa"))
(expect ["a \t" "" "}"] (split "{" "a \t{{}"))
(expect ["" " \t{{}"] (split "a" "a \t{{}"))


(expect 1 (1+ 0))
(expect 2  (1+ 1))
(expect 91 (1+ 90))
(expect 92 (1+ 91))
(expect 93 (1+ 92))
(expect 94 (1+ 93))
(expect 95 (1+ 94))
(expect 96 (1+ 95))
(expect 97 (1+ 96))
(expect 98 (1+ 97))
(expect 99 (1+ 98))
(expect 10 (1+ 9))
(expect 200 (1+ 199))

;; Memoize

(define mprefix "")

(define (mtest a b c)
  (concat mprefix c b a))

(memoize (global-name mtest))

(expect 321 (mtest 1 2 3))
(set mprefix 9)
(expect 9753 (mtest 3 5 7))
(expect 321 (mtest 1 2 3))

;; sort-by

(expect ["" "c/a" "c/a !0" "a/c!"]
        (sort-by (lambda (f) (notdir f))
                 ["c/a !0" "a/c!" "c/a" ""]))

;; assoc

(expect [1 2 3]    (assoc 1 [ [2 1 3] [1 2 3] [1] ]))
(expect [1]        (assoc 1 [ [2 1 3] [1] [1 2 3]]))
(expect [1 2 3]    (assoc 1 [ [2 1 3] [1 2 3] ]))
(expect [" " 2]    (assoc " " [ [2 1 3] [" " 2] ]))
(expect [" "]      (assoc " " [ [2 1 3] [" "] ]))
(expect ["!" 2]    (assoc "!" [ [2 1 3] ["!" 2] ]))
(expect ["!"]      (assoc "!" [ [2 1 3] ["!"] ]))
(expect ["%" 2]    (assoc "%" [ [2 1 3] ["%" 2] ]))
(expect ["%a !" 2] (assoc "%a !" [ [2 1 3] ["%a !" 2] ]))
(expect ["%a !"]   (assoc "%a !" [ [2 1 3] ["%a !"] ]))

(expect ["%a !" 2 3]  (assoc-vec ["%a !" 2]
                               [ ["%a" "!" 2] ["%a !" 1 2] ["%a !" 2 3] ]))

;; index-of

(expect 1 (index-of ["!" " " "\t"] "!"))
(expect 2 (index-of ["!" " " "\t"] " "))
(expect 3 (index-of ["!" " " "\t"] "\t"))
(expect 0 (index-of ["!" " " "\t"] "a"))

;; foldl
(expect "((0!)2)" (foldl (lambda (a b) (concat "(" a b ")")) 0 ["!" 2]))
(expect "0" (foldl (lambda (a b) (concat "(" a b ")")) 0 []))
(expect "(01)" (foldl (lambda (a b) (concat "(" a b ")")) 0 [1]))

;; foldr
(expect "(!(20))" (foldr (lambda (a b) (concat "(" a b ")")) 0 ["!" 2]))

;; intersperse
(expect [1 9 "cat dog" 9 3] (intersperse 9 [1 "cat dog" 3]))
(expect ["a b c"] (intersperse 9 ["a b c"]))
