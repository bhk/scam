(require "core")
(require "clocker")
(require "parse")
(require "gen")
(require "gen0")
(require "compile")
(require "io")
(require "num")
(require "gen1")


(define (read-file-assert file)
  (or (read-file file)
      (error (concat "file " file " not found"))))


(define (clock-gen1 file)
  (define text (read-file-assert file))
  (set text (subst "require \"" "require \"'" text))
  (define asts (parse-text text))
  (define env (compile-prelude ""))
  (define nodes (c0-block-cc env asts (lambda (e n) n)))

  (let ((o (gen1 nodes 1)))
    (expect nil (dict-get "errors" o)))

  (clk-show (concat "gen1 " file)
            (gen1 nodes 1)))


(define (main argv)
  (print "c1 benchmarks")
  (let ((totals (foreach file (or argv "../num.scm")
                         (clock-gen1 file))))
    (printf "total: %s  (%s)" (sum totals) (concat-vec totals " + ")))

  nil)
