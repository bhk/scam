(require "core")
(require "num")
(require "string" &private)


(expect 255 (words all-bytes))

;; gen-polysub

(expect "a$,\\#\nb" ( (gen-polysub ["#"] ["$,\\#\n"]) "a#b" ))
(expect "((" ( (gen-polysub [")"] ["("]) "()" ))
(expect "2.3.1" ( (gen-polysub "a b c" "1 2 3") "b.c.a"))
(expect "4444" ( (gen-polysub "1 2 3" "2 3 4") "1234"))

;; string-to-chars

(expect [" " "!" "\t" "!" " " "\x0d" "\n"] (string-to-chars " !\t! \x0d\n"))
(expect ["a" "¢" "€" "￦" "€" "¢" "a"] (string-to-chars "a¢€￦€¢a"))

;; string-len

(expect 0 (string-len ""))
(expect 4 (string-len "a   "))
(expect 9 (string-len "a\"b'\\x \t\n"))
(expect 1 (string-len "!"))
(expect 2 (string-len "!!"))
(expect 2 (string-len "!0"))
(expect 9 (string-len "ABcd01-_\t"))

;; string-slice

(expect "abc" (string-slice 1 3 "abc"))
(expect "a" (string-slice 1 1 "abc"))
(expect "" (string-slice 1 0 "abc"))
(expect "" (string-slice 2 0 "abc"))
(expect "abc" (string-slice 2 4 "xabcdef"))

;; string-upper
;; string-lower

(expect " `A1BZ{ " (string-upper " `a1Bz{ "))
(expect " @az[ az" (string-lower " @AZ[ az"))

;; string-to-bytes

(string-to-bytes "x")
(expect [32 65 66 67 33 49 32 33 48] (string-to-bytes " ABC!1 !0"))
(expect [255] (string-to-bytes "\xff"))
(expect (range 1 255) (string-to-bytes (concat-vec all-bytes)))

;; strings-from-bytes

(expect ["A" " " "" "\xFF"] (strings-from-bytes [65 32 0 255]))

;; string-from-bytes

(expect "A \xFF" (string-from-bytes [65 32 255]))
(expect (concat-vec all-bytes)
        (string-from-bytes (range 1 255)))

;; string-repeat

(expect "Abc" (string-repeat "Abc" 1))
(expect "AbcAbcAbcAbc" (string-repeat "Abc" 4))
(expect "" (string-repeat "Abc" 0))
