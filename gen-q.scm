;; Tests for gen0

(require "core")
(require "parse")
(require "gen" &private)


;; after, env-rewind

(expect "b c a"  (after "a" "a b c a"))
(expect "c d e"  (after "c" "a b c c d e"))
(expect ""       (after "f" "a b c d e f"))
(expect ""       (after "z" "a b c d e f"))

(expect (hash-bind "a" "asdf")
        (env-rewind-M (hash-bind "x" 1
                            (hash-bind "m" "-"
                                  (hash-bind "a" "asdf")))
                      "m"))

(expect (append
         (hash-bind "$" "$$")
         (hash-bind "f" ["F" "f"])
         (hash-bind "a" "asdf"))
        (env-rewind (hash-bind "x" 1
                          (hash-bind "$" "$$"
                                (hash-bind "f" ["F" "f" "" ""]
                                      (hash-bind "a" "asdf"))))
                    "f"
                    ["F" "f" "priv" "defn"]))



;; gensym

(expect "foo&"   (gensym-name "foo"))
(expect "S foo&" (gensym "S foo"))
(expect "S foo&1" (gensym "S foo" (hash-bind (symbol-name (gensym "S foo")) "V x")))


;; gen-error

(expect ["E.12" "Msg: hello error"]
        (gen-error "Q.12 x" "Msg: %s %s" "hello" "error"))


;; env-compress

(for s [",.;[]\!1!0!11!10!021!10 !. !@#$%^&*()_+=|}{\\][/.,?><';\":`~,i x!=F!0x v!=V!0x"]
     (expect s (env-expand (env-compress s))))


;; env-export & env-import

(define (export-round-trip env flag filename)
  (env-import
   (env-parse [ "# comment"
                (subst "\n" "" (env-export env))
                "# F F F F F F"])
   flag
   filename))


;; import only public members

(expect (append (hash-bind "f" "F f i")
                (hash-bind "v" "V X i")
                (hash-bind "I" ["F" "I" "iMOD" ["a b" "S a"]])
                (hash-bind "m" ["M" "Q 1" "iMOD"])
                (hash-bind "a:n\n,x" "V xyz i"))

        (export-round-trip
         (append (hash-bind "f" "F f")
                 (hash-bind "v" "V X")
                 (hash-bind "I" ["F" "I" "" ["a b" "S a"]])
                 (hash-bind "g" "F g p")  ;; private
                 (hash-bind "g" "F g i")  ;; imported
                 (hash-bind "m" ["M" "Q 1" ""])
                 (hash-bind "a:n\n,x" "V xyz"))
         ""
         "MOD"))


;; import public AND private members

(expect (append (hash-bind "f" "F f")
                (hash-bind "v" "V X")
                (hash-bind "I" ["F" "I" "i" ["a b" "S a"]])
                (hash-bind "g" "F g p")
                (hash-bind "g" "M g i")  ;; imported
                (hash-bind "a:n\n,x" "V xyz"))

        (export-round-trip
         (append (hash-bind "f" "F f")
                 (hash-bind "v" "V X")
                 (hash-bind "I" ["F" "I" "i" ["a b" "S a"]])
                 (hash-bind "g" "F g p")  ;; private
                 (hash-bind "g" "M g i")  ;; imported macro
                 (hash-bind "a:n\n,x" "V xyz"))
         1
         "File Name.min"))
