(require "core")
(require "num")
(require "string" &private)


(expect 255 (words all-bytes))

;; gen-polysub

(expect "a$,\\#\nb" ( (gen-polysub ["#"] ["$,\\#\n"]) "a#b" ))
(expect "((" ( (gen-polysub [")"] ["("]) "()" ))
(expect "2.3.1" ( (gen-polysub "a b c" "1 2 3") "b.c.a"))
(expect "4444" ( (gen-polysub "1 2 3" "2 3 4") "1234"))

;; split-chars

(expect [" " "!" "\t" "!" " " "\x0d" "\n"] (split-chars " !\t! \x0d\n"))
(expect ["a" "¢" "€" "￦" "€" "¢" "a"] (split-chars "a¢€￦€¢a"))

;; strlen

(expect 0 (strlen ""))
(expect 4 (strlen "a   "))
(expect 9 (strlen "a\"b'\\x \t\n"))
(expect 1 (strlen "!"))
(expect 2 (strlen "!!"))
(expect 2 (strlen "!0"))
(expect 9 (strlen "ABcd01-_\t"))

;; substring

(expect "abc" (substring 1 3 "abc"))
(expect "a" (substring 1 1 "abc"))
(expect "" (substring 1 0 "abc"))
(expect "" (substring 2 0 "abc"))
(expect "abc" (substring 2 4 "xabcdef"))

;; toupper & tolower

(expect " `A1BZ{ " (toupper " `a1Bz{ "))
(expect " @az[ az" (tolower " @AZ[ az"))

;; string-to-bytes

(string-to-bytes "x")
(expect [32 65 66 67 33 49 32 33 48] (string-to-bytes " ABC!1 !0"))
(expect [255] (string-to-bytes "\xff"))
(expect (range 1 255) (string-to-bytes (concat-vec all-bytes)))

;; string-from-bytes

(expect "A \xFF" (string-from-bytes [65 32 255]))
(expect (concat-vec all-bytes)
        (string-from-bytes (range 1 255)))

;; string-repeat

(expect "Abc" (string-repeat "Abc" 1))
(expect "AbcAbcAbcAbc" (string-repeat "Abc" 4))
(expect "" (string-repeat "Abc" 0))


(print "string ok")
