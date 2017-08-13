(require "core")
(require "getopts")


(let ((o (getopts ["--arg" "x y z" "a b" "-f" "c"]
                  "--arg= -f"
                  nil)))
  (define `files (nth 1 o))
  (define `omap (nth 2 o))

  (expect ["a b" "c"] files)
  (expect "x y z" (dict-get "arg" omap))
  (expect "1" (dict-get "f" (omap))))


(let ((o (getopts ["--arg" "x y z" "a b" "-f" "c"]
                  "--arg="
                  (lambda (opt) (concat "ERR:" opt)))))
  (expect "ERR:-f" o))


(let ((o (getopts ["-b" "B" "-x" "a b" "-a" "c"]
                  "-a= -b= -x=..."
                  nil)))
  (define `files (nth 1 o))
  (define `omap (nth 2 o))

  (expect "B" (dict-get "b" omap))
  (expect nil (dict-get "a" omap)))
