(require "core")
(require "getopts")


(let ((o (getopts ["--arg" "x y z" "a b" "-f" "c"]
                  "--arg= -f")))

  (expect ["a b" "c"] (nth 1 o))
  (expect "x y z" (hash-get "--arg" (nth 2 o)))
  (expect "1" (hash-get "-f" (nth 2 o))))


(let ((o (getopts ["--arg" "x y z" "a b" "-f" "c"]
                  "--arg="
                  (lambda (opt) (concat "ERR:" opt)))))
  (expect "ERR:-f" o))
