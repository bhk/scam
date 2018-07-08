(require "core.scm")
(require "io.scm" &private)

(define SOURCE_DIR (dir (current-file)))
(define TMP_DIR (or (value "TEST_DIR")
                    (concat SOURCE_DIR ".out/")))

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
          (expect nil (write-file tmpfile " a\\b\n\t c"))
          (shell (concat "echo X >> " tmpfile))
          (read-file tmpfile)))

(define `non-file (concat TMP_DIR "io-q-dir"))
(shell (concat "mkdir -p " (quote-sh-arg non-file)))
(expect 1 (see "directory" (write-file non-file "xyz")))
;; ensure it cleaned up the temp file
(expect nil (file-exists? (concat non-file "_[tmp]")))

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

(define `test-file (concat SOURCE_DIR "test/io-q.txt"))

(expect io-q (read-file test-file))
(expect io-q-lines (read-lines test-file))
(expect (wordlist 2 5 io-q-lines) (read-lines test-file 2 5))

(expect nil (read-lines (concat SOURCE_DIR "does-not-exist")))

;; file-exists?

(expect (current-file) (file-exists? (current-file)))
(expect nil (file-exists? (concat SOURCE_DIR "does-not-exist")))

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
(expect "+./foo" (escape-path "../foo"))

(define `(escape-rt str)
  (expect str (unescape-path (escape-path str)))
  (expect 1 (words (escape-path (concat "x" str "x")))))

(escape-rt "/../..a$*!#//x")
(escape-rt "+ !#\\$:;=%~*?|\t\n")
