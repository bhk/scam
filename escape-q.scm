;;--------------------------------
;; escaping tests
;;--------------------------------

(require "core.scm")
(require "escape.scm" &private)

(expect ""                      (protect-arg ""))
(expect "()"                    (protect-arg "()"))
(expect "(!1. )"                (protect-arg "(!1. )"))
(expect "(,)"                   (protect-arg "(,)"))
(expect "((,)(a)((b)) )"        (protect-arg "((,)(a)((b)) )"))
(expect "$(if ,,(,),)"          (protect-arg "(,),"))
(expect "$]$["                  (protect-arg ")("))
(expect "()$["                  (protect-arg "()("))
(expect "$]()"                  (protect-arg ")()"))
(expect "$(if ,,(a),(,)$[)"     (protect-arg "(a),(,)("))
(expect "$(if ,,,)"             (protect-arg ","))

(expect ""         (protect-ltrim ""))
(expect "$  b"     (protect-ltrim " b"))
(expect "$ \t"     (protect-ltrim "\t"))
(expect "x y "     (protect-ltrim "x y "))

(expect ""             (protect-trim ""))
(expect "$(if ,,\na)"  (protect-trim "\na"))
(expect "$(if ,,a\n)"  (protect-trim "a\n"))
(expect "a\nb"         (protect-trim "a\nb"))

(expect "x$\""        (protect-lhs "x#"))
(expect "$(if ,,x=)"  (protect-lhs "x="))

(expect "abc\ndef" (protect-define "abc\ndef"))
(expect "$ define\n$ endef extra\\$ \n\\$ "
        (protect-define "define\nendef extra\\\n\\"))
