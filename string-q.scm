(require "core.scm")
(require "string.scm" &private)


(expect 255 (words all-bytes))

;; gen-polysub

(expect " 12 3 21 " ( (gen-polysub [" 1 "] [" 3 "]) " 12 1 21 "))
(expect "a$,\\#\nb" ( (gen-polysub ["#"] ["$,\\#\n"]) "a#b" ))
(expect "((" ( (gen-polysub [")"] ["("]) "()" ))
(expect "2.3.1" ( (gen-polysub "a b c" "1 2 3") "b.c.a"))
(expect "4444" ( (gen-polysub "1 2 3" "2 3 4") "1234"))
(expect "4.6" ( (gen-polysub "5" "." (lambda (a b) b)) "123" "456"))
(expect "456" ( (gen-polysub nil nil (lambda (a b) b)) "123" "456"))

;; string-to-chars

(expect [] (string-to-chars ""))
(expect [" " "!" "\t" "!" " " " " "\x0d" "\n"] (string-to-chars " !\t!  \x0d\n"))
(expect ["a" "¢" "€" "￦" "€" "¢" "a"] (string-to-chars "a¢€￦€¢a"))
;; When not UTF8, the vector should still contain bytes from the original string
(expect ["\x80\x81 " "\xA1"] (string-to-chars "\x80\x81 \xA1"))

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

(expect ["\t" "A" "\n" "\x80" "\xa1" "\xd1" "\xff"]
        (string-to-bytes "\tA\n\x80\xA1\xD1\xFF"))

;; string-to-bytecodes

(expect [] (string-to-bytecodes ""))
(expect [97] (string-to-bytecodes "a"))
(expect [32 65 66 67 33 49 32 33 48] (string-to-bytecodes " ABC!1 !0"))
(expect [255] (string-to-bytecodes "\xff"))
(expect (urange 1 255) (string-to-bytecodes (concat-vec all-bytes)))

;; bytes-from-bytecodes

(expect ["A" " " "" "\xFF"] (bytes-from-bytecodes [65 32 0 255]))

;; string-from-bytecodes

(expect "A \xFF" (string-from-bytecodes [65 32 255]))
(expect (concat-vec all-bytes)
        (string-from-bytecodes (urange 1 255)))

;; string-repeat

(expect "Abc" (string-repeat "Abc" 1))
(expect "AbcAbcAbcAbc" (string-repeat "Abc" 4))
(expect "" (string-repeat "Abc" 0))
(expect "" (string-repeat "" 3))
