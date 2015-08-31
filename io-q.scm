(require "core")
(require "io")

;; File I/O

(define `thisfile (lastword MAKEFILE_LIST))

(expect " a\\b\n\t cX\n"
        (let ((tmpfile (concat thisfile "-rwtest")))
          (write-file tmpfile " a\\b\n\t c")
          (shell (concat "echo X >> " tmpfile))
          (read-file tmpfile)))


;; read-file

(define io-q-lines
  [ "1 tab:\tb"
    "2 a !!  c"
    ""
    "4"
    ""
    ""
    "7"])

(define io-q (concat (concat-vec io-q-lines "\n") "\n"))

(expect io-q (read-file "test/io-q.txt"))
(expect io-q-lines (read-lines "test/io-q.txt"))
(expect (wordlist 2 5 io-q-lines) (read-lines "test/io-q.txt" 2 5))


(print "io ok")
