(require "core")
(require "clocker.scm")
(require "parse")
(require "gen")
(require "gen0")
(require "compile")
(require "io")
(require "math")
(require "gen1")


(define (read-file-assert file)
  (or (read-file file)
      (error (.. "file " file " not found"))))


(define (clock-gen1 file)
  (define text (read-file-assert file))
  ;; convert source references to builtin lib references to
  ;; avoid building other files:
  (set text (subst ".scm\")" "\")" text))
  (define asts (parse-text text))
  (define env (compile-prelude file))
  (define nodes (c0-block-cc env asts (lambda (e n) n)))

  (let ((o (gen1 nodes 1)))
    (expect nil (dict-get "errors" o)))

  (clk-show (.. "gen1 " file)
            (gen1 nodes 1)))


(define (main argv)
  (define `default-sources
    (addprefix (dir (current-file)) "../mcore.scm"))

  (print "c1 benchmarks")
  (let ((totals (foreach (file (or argv default-sources))
                  (clock-gen1 file))))
    (printf "total: %s  (%s)" (sum totals) (concat-vec totals " + ")))

  nil)
