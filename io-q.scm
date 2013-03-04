(require "core")
(require "io")

;; File I/O

(define `thisfile (lastword MAKEFILE_LIST))

(expect " a\\b\n\t cX\n"
        (let ((tmpfile (concat thisfile "-rwtest")))
          (write-file tmpfile " a\\b\n\t c")
          (shell (concat "echo X >> " tmpfile))
          (read-file tmpfile)))


(print "io ok")
