(require "core")
(require "getopts")


(let ((o (getopts ["--arg" "x y z" "a b" "-f" "c"]
                  "--arg= -f")))
  (define `files (nth 1 o))
  (define `omap (nth 2 o))

  (expect ["a b" "c"] files)
  (expect "x y z" (hash-get "arg" omap))
  (expect "1" (hash-get "f" (omap))))


(let ((o (getopts ["--arg" "x y z" "a b" "-f" "c"]
                  "--arg="
                  (lambda (opt) (concat "ERR:" opt)))))
  (expect "ERR:-f" o))


(let ((o (getopts ["-b" "B" "-x" "a b" "-a" "c"]
                  "-a= -b= -x=...")))
  (define `files (nth 1 o))
  (define `omap (nth 2 o))

  (expect "B" (hash-get "b" omap))
  (expect nil (hash-get "a" omap)))
