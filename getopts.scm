;; getopts.scm: parse command-line options
;;
;; Synopsis:
;;
;;   (getopts "a b -f --arg x" "-f --arg=")  -->  [["a" "b"] {"-f"=1 "-arg"="x"}]
;;
;; (getopts args opts err)
;;   args = arguments array
;;   opts = list of option specifiers:
;;        -OPTIONNAME    => flag
;;        -OPTIONNAME=   => option that takes an argument
;;   err = function to be called when an unrecognized option is found:
;;            (err option-string)
;;
;;   result = [files omap]
;;      omap :: name -> value
;;
;; File names and options can appear in any order.  Option names may not
;; contain '%' or '!' characters or whitespace.  File names may contain any
;; characters.
;;

(require "core")

(define (getopts args opts err files omap)
  (define opt (word 1 args))
  (define file (nth 1 args))

  (cond
   ;; done?
   ((not args)
    [files omap])

   ;; file
   ((not (filter "-%" opt))
    (getopts (rest args) opts err (conj files file) omap))

   ;; flag
   ((filter opt opts)
    (getopts (rest args) opts err files (hash-bind opt 1 omap)))

   ;; option=
   ((filter (concat opt "=") opts)
    (getopts (rest (rest args)) opts err files (hash-bind opt (nth 2 args) omap)))

   ;; error
   (else
    (err opt (rest args)))))
