(require "core")
(require "clocker.scm")
(require "parse")
(require "gen")
(require "gen0")
(require "gen1")
(require "compile")
(require "io")
(require "math")


(define (read-file-assert file)
  (or (read-file file)
      (error (.. "file " file " not found"))))


(define (clock-c0 file)
  ;; try to prevent `require` from compiling OTHER files...
  (define text (subst "\"core\"" "\"'core\"" (read-file-assert file)))
  (define ast (parse-text text))
  (define env (compile-prelude file))

  (let-global ((*compile-subject*  (penc text))
               (*compile-file*     file))
    (clk-show (.. "c0: " file)
              (c0-block-cc env ast nil))))


(define (main argv)
  (print "c0 benchmarks")
  (let ((totals (foreach (file argv)
                  (clock-c0 file))))
    (printf "total: %s  (%s)" (sum totals) (concat-vec totals " + ")))

  nil)
