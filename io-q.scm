(require "core")
(require "io" &private)


;; shell!

(expect "  \t \n \n"
        (shell! "echo $'  \\t \\n '"))

;; write

(expect 1 (see "Bad file descriptor" (write 543 "hi")))

;; concat-groups

(expect "abc def ghi j"
        (filter "%"
                (concat-groups "a b c d e f g h i j" 3)))

;; write-file & read-file

(define `thisfile (lastword MAKEFILE_LIST))

(expect " a\\b\n\t cX\n"
        (let ((tmpfile (concat thisfile "-rwtest")))
          (expect "OK" (write-file tmpfile " a\\b\n\t c"))
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

(expect nil (read-lines "test/nonexist"))

;; file-exists?

(expect "io.scm" (file-exists? "io.scm"))
(expect nil (file-exists? "io.scm.not"))

;; clean-path

(define `(cp-test a b)
  (expect (clean-path a) b))

(cp-test "." ".")
(cp-test "./" "./")
(cp-test "/." "/.")
(cp-test "/" "/")
(cp-test "a" "a")
(cp-test "a/" "a/")
(cp-test "a//b" "a/b")
(cp-test "a/.." ".")
(cp-test "a/../" "./")
(cp-test "a/../b/" "b/")
(cp-test "a/../../b/" "../b/")
(cp-test "a/.././../b/../c" "../c")
(cp-test "a/b/c/../../b/" "a/b/")
(cp-test "a/b/c/../../../b/" "b/")
(cp-test "a/b c/d/.." "a/b c")

;; resolve-path

(expect "/a" (resolve-path "dir" "/a/b/.."))
(expect "dir/a" (resolve-path "dir" "a/b/.."))

;; escape-path

(expect "foo" (escape-path "foo"))
(expect "+Tfoo" (escape-path "~foo"))
(expect "+/foo" (escape-path "/foo"))

(define `(escape-rt str)
  (expect str (unescape-path (escape-path str)))
  (expect 1 (words (escape-path (concat "x" str "x")))))

(escape-rt "/../..a$*!#//x")
(escape-rt "+ !#\\$:;=%~*?|\t\n")
