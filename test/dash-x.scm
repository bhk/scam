#!/usr/bin/env scam --quiet --
;; Immediate mode ("scam FILE") test
;;
;; - The initial "hashbang" line should be ignored.
;; - Requires bundled files.
;; - Is given "file" arguments properly in argv.
;; - When a number is returned, it is treated as the exit code.
;;

(require "core")
(require "math")

(define (conc vec delim)
  (if vec
      (.. (first vec)
          (if (word 2 vec) delim)
          (conc (rest vec) delim))))

(define (main argv)
  (define `[a b c] argv)

  (or
   ;; return exit code
   (if (eq? "--exit" a)
       b)

   (begin
     (print (concat-vec argv ":") ":" (^ (words argv) 1))
     0)))
