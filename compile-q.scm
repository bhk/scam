;; tests for compile

(require "compile" &private)


;;--------------------------------------------------------------
;; compile-text / compile-forms
;;--------------------------------------------------------------

;; for file
(expect ["" "a := 1\n" "# Exports: a=V,a\n"]
        (compile-text "(define a 1)" "" "(test)" "test.tmp"))

;; for eval
(expect ["" "$(call ^set,a,1)" (bind "a" "V a")]
        (compile-text "(define a 1)" "" "(test)" ""))

(expect "a := 1\nb := 2\n"
        (nth 2 (compile-text "(define a 1) (define b 2)" "" "(test)" "test.tmp")))


(define *written* "")
(define `wname (first *written*))
(define `wtext (nth 2 *written*))

(let-global ((write-file (lambda (name data) (set *written* [name data])))
             (read-file (lambda (name) "(define x 1)")))

            (compile-file "foo.scm" "xx/foo.min")
            (expect "xx/foo.min" wname)
            (expect 1 (see "# Exports: x" wtext))
            (expect 1 (see "x := 1" wtext)))


