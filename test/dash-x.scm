#!/usr/bin/env scam -x
;; "scam -x SOURCE" test file
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
      (concat (first vec)
              (if (word 2 vec) delim)
              (conc (rest vec) delim))))

(define (main argv)
  ;; return exit code (check content and lenth of words
  (print (conc (append (^ (nth 1 argv) 2)
                       argv)
               ":"))
  0)
