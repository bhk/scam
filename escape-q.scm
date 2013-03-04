;;--------------------------------
;; escaping tests
;;--------------------------------

(require "core")
(require "escape" &private)

(expect "()"                    (protect-arg "()"))
(expect "(!1. )"                (protect-arg "(!1. )"))
(expect "(,)"                   (protect-arg "(,)"))
(expect "$(if ,,(,),)"          (protect-arg "(,),"))
(expect "$(\\R)$(\\L)"          (protect-arg ")("))
(expect "()$(\\L)"              (protect-arg "()("))
(expect "$(if ,,(a),(,)$(\\L))"  (protect-arg "(a),(,)("))


(expect "" (findstring "!" (check-balance "a(b)c()")))
(expect "" (findstring "!" (check-balance "()")))
(expect "!" (findstring "!" (check-balance "(")))
(expect "!" (findstring "!" (check-balance ")")))
(expect "!" (findstring "!" (check-balance "a(b)c(")))
(expect "!" (findstring "!" (check-balance "a)b)c")))
(expect "!" (findstring "!" (check-balance "a(b(c")))


(expect "$  b"     (protect-ltrim " b"))
(expect "$ \t"     (protect-ltrim "\t"))
(expect "x y "     (protect-ltrim "x y "))


;; todo: eliminate escape-arg; just use protect-arg
(define (escape-arg str)
  (protect-arg (subst "$" "$$" str)))

(expect "a$$b"                 (escape-arg "a$b"))
(expect "a(b,)c"               (escape-arg "a(b,)c"))
(expect "a$(\\L)b"             (escape-arg "a(b"))
(expect "a$(\\R)b"             (escape-arg "a)b"))
(expect "a(x,y)bc(d,f)e"       (escape-arg "a(x,y)bc(d,f)e"))
(expect "$(if ,,a(xy)b,c(d)e)" (escape-arg "a(xy)b,c(d)e"))


(expect "x$(\\H)"     (protect-lhs "x#"))
(expect "$(if ,,x=)"  (protect-lhs "x="))

;; One alternative:
;; (expect "a$(word 1,( ))b"      (escape-arg "a(b"))

(print "escape ok")
