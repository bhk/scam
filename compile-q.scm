;; tests for compile

(require "core")
(require "gen")
(require "io")
(require "compile" &private)


;;--------------------------------------------------------------
;; compile-text
;;--------------------------------------------------------------

;; for file
(let ((o (compile-text "(define a &global 1)" "" "(test)" "test.tmp")))
  (define `errors (nth 1 o))
  (define `exe (nth 2 o))
  (define `env-out (nth 3 o))

  (expect "" errors)
  (expect "a := 1\n" exe)
  (expect (hash-get "a" env-out)
          (EVar "a" "p")))


;; for eval
(let ((o (compile-text "(define a &global 1)" "" "(test)" "")))
  (define `errors (nth 1 o))
  (define `exe (nth 2 o))
  (define `exports (nth 3 o))

  (expect "" errors)
  (expect "$(call ^set,a,1)" exe)
  (expect (hash-bind "a" (EVar "a" "p")) (word 1 exports)))


(expect "a := 1\nb := 2\n"
        (nth 2 (compile-text (concat "(define a &global 1) "
                                     "(define b &global 2)")
                             "" "(test)" "test.tmp")))


(define *written* "")
(define `wname (first *written*))
(define `wtext (nth 2 *written*))

(let-global ((write-file (lambda (name data) (set *written* [name data])))
             (read-file (lambda (name) "(define x 1)")))

            (compile-file "foo.scm" "xx/foo.min" "rt"
                          nil nil nil)
            (expect "xx/foo.min" wname)
            (expect 1 (see "# Exports: x" wtext))
            (expect 1 (see "x := 1" wtext)))
