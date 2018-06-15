(require "core")
(require "clocker")
(require "parse")
(require "num")
(require "io")


(define (read-file-assert file)
  (or (read-file file)
      (error (concat "file " file " not found"))))


(define (clock-parse file)
  (define text (read-file-assert file))
  (clk-show (concat "parse " file)
            (parse-text text)))


(define (main argv)
  (set default-duration 50)
  (print "parse benchmarks (source files as arguments)")
  (let ((totals (foreach w (or argv "../macros.scm")
                         (clock-parse w))))
    (printf "total: %s  (%s)" (sum totals) (concat-vec totals " + ")))

  nil)
