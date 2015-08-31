(require "core")
(eval "override SCAM_TRACE :=")
(require "trace" &private)

(expect "........0" (trace-digits ""))
(expect "........1" (trace-digits "o"))
(expect "......121" (trace-digits "oiooio"))
(expect ".......10" (trace-digits "oooooooooo"))

;; counting

(define (count-of fname)
  (subst "." ""
         (trace-digits (value (concat "^K_" fname)))))

(define (cfunc a)
  &global
  (if (rest a)
      (cfunc (rest a))))

(trace "cfunc:c foo")
(expect *traces-active* "cfunc:c")

(cfunc "1")
(expect 1 (count-of "cfunc"))
(cfunc "1 2 3 4 5 6 7 8")
(expect 9 (count-of "cfunc"))
(cfunc "1")
(expect 10 (count-of "cfunc"))

;; (trace-check)

(define (foo a)
  &global
  (concat a a))

(expect *traces-active* "cfunc:c")
(trace-check)
(expect *traces-active* "cfunc:c foo:t")

;; suppress (trace-dump) message
(set *traces-active* nil)
(set *traces* nil)
