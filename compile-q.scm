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
  (expect "" (dict-get "errors" o))
  (expect "a := 1\n" (dict-get "code" o))
  (expect (dict-get "a" (dict-get "env" o))
          (EVar "a" "p")))

;; for eval
(let ((o (compile-text "(define a &global 1)" "" "(test)" "")))
  (expect "" (dict-get "errors" o))
  (expect "$(call ^set,a,1)" (dict-get "code" o))
  (expect { a: (EVar "a" "p") }
          (dict-get "env" o)))


(expect "a := 1\nb := 2\n"
        (dict-get "code"
                  (compile-text (concat "(define a &global 1) "
                                        "(define b &global 2)")
                                "" "(test)" "test.tmp")))


;; require & use
(declare ^require &global)

(let-global ((locate-module (lambda (f name) (concat "'" name)))
             (^require (lambda () nil))
             (env-import (lambda () nil)))
  (let ((o (compile-text "(require \"r\")(use \"u\")(use \"v\")" "" "(test)" "test.tmp")))
    (expect "" (dict-get "errors" o))
    (expect "'r" (dict-get "require" o))
    (expect "'u 'v" (dict-get "use" o))
    (expect 1 (see "$(call ^require,'r)\n"
                   (dict-get "code" o)))))


(define *written* "")
(define `wname (first *written*))
(define `wtext (nth 2 *written*))


;; compile-file

(define harness-FS
  { "foo.scm": "(define x 1)",
    ".scam/runtime.min": "# empty" })

(define (harness-read-file name)
  (dict-get name harness-FS))

(define (harness-read-lines name)
  (split "\n" (harness-read-file name)))

(let-global ((write-file (lambda (name data) (set *written* [name data]) "OK"))
             (read-file harness-read-file)
             (read-lines harness-read-lines))

  (compile-file "foo.scm" ".out/foo.min" "rt" "C")
  (expect ".out/foo.min" wname)
  (expect 1 (see "# Exports: x" wtext))
  (expect 1 (see "x := 1" wtext)))
