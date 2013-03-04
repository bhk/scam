;; Tests for gen0

(require "core")
(require "parse")
(require "gen" &private)


;; bind-sym

(expect (bind "f" "F f")
        (bind-sym "S f" "F" ""))

(expect (bind "f" "F f p DEFN")
        (bind-sym "S f" "F" "p" "DEFN"))

;; after, env-rewind

(expect "b c a"  (after "a" "a b c a"))
(expect "c d e"  (after "c" "a b c c d e"))
(expect ""       (after "f" "a b c d e f"))
(expect ""       (after "z" "a b c d e f"))

(expect (bind "a" "asdf")
        (env-rewind-M (bind "x" 1
                            (bind "m" "-"
                                  (bind "a" "asdf")))
                      "m"))

(expect (append
         (bind "$" "$$")
         (bind "f" ["F" "f"])
         (bind "a" "asdf"))
        (env-rewind (bind "x" 1
                          (bind "$" "$$"
                                (bind "f" ["F" "f" "" ""]
                                      (bind "a" "asdf"))))
                    "f"
                    ["F" "f" "priv" "defn"]))



;; gensym

(expect "foo&"   (gensym-name "foo"))
(expect "S foo&" (gensym "S foo"))
(expect "S foo&1" (gensym "S foo" (bind (symbol-name (gensym "S foo")) "V x")))


;; gen-error

(expect ["E.12" "Msg: hello error"]
        (gen-error "Q.12 x" "Msg: %s %s" "hello" "error"))


;; env-export & env-import

(define (export-round-trip env flag filename)
  (env-import
   (env-parse
    (concat "# comment\n" (env-export env) "# F F F F F F\n"))
   flag
   filename))


;; import only public members

(expect (append (bind "f" "F f i")
                (bind "v" "V X i")
                (bind "I" ["F" "I" "iFile Name.min" ["a b" "S a"]])
                (bind "m" ["M" "Q 1" "iFile Name.min"])
                (bind "a:n\n,x" "V xyz i"))

        (export-round-trip
         (append (bind "f" "F f")
                 (bind "v" "V X")
                 (bind "I" ["F" "I" "" ["a b" "S a"]])
                 (bind "g" "F g p")  ;; private
                 (bind "g" "F g i")  ;; imported
                 (bind "m" ["M" "Q 1" ""])
                 (bind "a:n\n,x" "V xyz"))
         ""
         "File Name.min"))


;; import public AND private members

(expect (append (bind "f" "F f")
                (bind "v" "V X")
                (bind "I" ["F" "I" "i" ["a b" "S a"]])
                (bind "g" "F g p")
                (bind "g" "M g i")  ;; imported
                (bind "a:n\n,x" "V xyz"))

        (export-round-trip
         (append (bind "f" "F f")
                 (bind "v" "V X")
                 (bind "I" ["F" "I" "i" ["a b" "S a"]])
                 (bind "g" "F g p")  ;; private
                 (bind "g" "M g i")  ;; imported macro
                 (bind "a:n\n,x" "V xyz"))
         1
         "File Name.min"))
