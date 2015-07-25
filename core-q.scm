;;--------------------------------
;; general purpose function tests
;;--------------------------------

(require "core" &private)

(if (eq 1 2)
    (error "eq not working"))

(set-global "GG" "$")
(expect "simple" (flavor "GG"))
(expect "$" (value "GG"))

(set-rglobal "FF" "$")
(expect "recursive" (flavor "FF"))
(expect "$" (value "FF"))

(expect "1" (not ""))
(expect "" (not "x"))

(expect "" (eq "" "a"))
(expect "" (eq "a" ""))
(expect "" (eq "a" "aa"))
(expect "1" (eq "a" "a"))
(expect "1" (eq "" ""))

(expect 5678 (identity 5678))

(expect 1 (xor 1 ""))
(expect 2 (xor "" 2))
(expect "" (xor 1 2))

(expect "! 1" (first ["! 1" 2]))

(expect "b c" (rest "a   b c  "))

(expect "3 4" (rrest "1 2 3 4"))

(expect "4 5" (nth-rest 4 "1 2 3 4 5"))

(expect "x A!0B" (conj "x" "A B"))

(expect "c" (last "a b c"))
(expect "a b" (butlast "a b c"))

(expect ["" 1 ""] (map-call "not" [1 "" 3]))

(expect [[1 2]] (select-vec (lambda (x) (word 2 x)) [[1 2] 3 2]))

(expect "2 23" (select-words (lambda (n) (findstring 2 n)) "1 2 3 23"))

(expect "1 2" (vec-or ["" "1 2" 3]))
(expect "" (vec-or ["" "" "" ""]))

(expect "" (indices ""))
(expect "1 2 3 " (indices "a b c"))


(expect "2" (isnumber "2"))
(expect "-1" (isnumber "-1"))
(expect "0.1" (isnumber "0.1"))
(expect "21." (isnumber "21."))
(expect "" (isnumber ".1"))
(expect "" (isnumber "+1"))
(expect "" (isnumber "-"))
(expect "" (isnumber "x"))
(expect "" (isnumber "1+"))
(expect "" (isnumber "1.1."))
(expect "" (isnumber "1x0"))
(expect "1.2e-7" (isnumber "1.2e-7"))
(expect "-1e7" (isnumber "-1e7"))
(expect "" (isnumber "1e7.1"))
(expect "" (isnumber " 2 "))   ;; format relies on this

(expect "[\"a\" \" \" [\"\"]]" (format "a !0 !1."))
(expect "\" !0 \"" (format " !0 "))
(expect "\"!x\"" (format "!x"))
(expect "[\" \"]" (format "!0"))
(expect "-12" (format "-12"))

(expect "[1 \"a b\"] --> 1 a!0b" (sprintf "%q --> %s" [1 "a b"] [1 "a b"]))
(expect "nada" (sprintf "nada" "ignored"))
(expect "!P!. a !\t !0 x" (sprintf "!P!.%s!0%sx" " a !\t " " " "ignored"))
(expect "a%b%c" (sprintf "a%%b%s" "%c"))

(foreach printf-warn 1  ;; suppress warning message
         (expect "%z" (sprintf "%z" 1)))

(expect "" (reverse ""))
(expect [3 2 1] (reverse [1 2 3]))
(expect [11 10 9 8 7 6 5 4 3 2 1] (reverse [1 2 3 4 5 6 7 8 9 10 11]))
(expect ["a\nb" "c\nd"] (reverse "c\nd a\nb"))

(expect "a.b.c" (while (lambda (x) (findstring ".." x))
                  (lambda (x) (subst ".." "." x))
                  "a..b...c"))

(expect "1,2,3" (concat-vec [1 2 3] ","))

(expect "1" (not (bound? "_xya13")))
(expect "1" (bound? "bound?"))

(expect 0 (count-chars "a" "b"))
(expect 1 (count-chars "b" "b"))
(expect 3 (count-chars "a b c b d e b" "b"))
(expect 4 (count-chars "abc\n\n\ndef\n" "\n"))

(expect 3 (count-words "a b c b d e b" "b"))
(expect 0 (count-words "a b c b d e b" "x"))

(expect "x!=y a!=!." (hash-bind "x" "y" (hash-bind "a" "")))

(expect "a b c" (hash-key (hash-bind "a b c" " d e ")))
(expect " d e " (hash-value (hash-bind "a b c" " d e ")))

(expect "!0!=x" (hash-find " " (hash-bind " " "x")))
(expect "x!=M" (hash-find "x" (hash-bind "x" "M" (hash-bind "x" "K"))))

(expect " " (hash-get "" (hash-bind "a" "b" (hash-bind "" " " (hash-bind "x" "y")))))
(expect "" (hash-get "" (hash-bind "" "") "default"))
(expect "default" (hash-get "x" (hash-bind "" "") "default"))
(expect "val1" (hash-get "x%x" (hash-bind "x%x" "val1" (hash-bind "x%x" "%"))))

(expect (hash-bind " " 1 (hash-bind "b" 2 (hash-bind "bb" 9)))
        (compact (hash-bind " " 1
                            (hash-bind "b" 2
                                       (hash-bind "b" 7
                                                  (hash-bind " " 3
                                                             (hash-bind "bb" 9)))))))

(expect "" (append))
(expect "a b c" (append "a" "" "b" "" "" "c"))
(expect "a b c" (append "" "a" "" "b" "" "c"))
(expect "a b c d" (append "" "" "" "a" "b" "" "" "c" "d"))

(expect "a b c" (uniq "a a b a c a b"))

(expect ["" "abc" ""] (split "/x/" "/x/abc/x/"))

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

(memoize "mtest")

(expect 321 (mtest 1 2 3))
(set mprefix 9)
(expect 9753 (mtest 3 5 7))
(expect 321 (mtest 1 2 3))

;; sort-by

(expect ["c/a" "c/a b" "a/c!"]
        (sort-by (lambda (f) (notdir f))
                 ["c/a b" "a/c!" "c/a"]))

;; type?

(expect "F.1" (type? "A% F%" ["F.1" "Q f"]))

(print "core ok")
