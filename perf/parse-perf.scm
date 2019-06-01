(require "core")
(require "clocker.scm")
(require "parse")
(require "math")
(require "io")


(define (read-file-assert file)
  (or (read-file file)
      (error (.. "file " file " not found"))))


(define (clock-parse file)
  (define text (read-file-assert file))
  (clk-show (.. "parse " file)
            (parse-text text)))


(define (main argv)
  (define `default-sources
    (addprefix (dir (current-file)) "../macros.scm"))

  (set default-duration 50)
  (print "parse benchmarks (source files as arguments)")
  (let ((totals (foreach w (or argv default-sources)
                         (clock-parse w))))
    (printf "total: %s  (%s)" (sum totals) (concat-vec totals " + ")))

  nil)
