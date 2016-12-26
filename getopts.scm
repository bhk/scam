;;--------------------------------
;; getopts.scm: parse command-line options
;;--------------------------------

(require "core")

;; Synopsis:
;;
;;   (getopts "a b -f --arg x" "-f --arg=")  -->  [["a" "b"] {"f": 1; "arg": "x"}]
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

;; opts0 = options with no arguments (flags)
;; opts1 = options with one argument
;; opts0 = options that consume the rest of the arguments
;;
(define (getopts-loop opts0 opts1 optsN err args files ovalues)
  (define opt (word 1 args))
  (define file (nth 1 args))

  (define `(recur a f m)
    (getopts-loop opts0 opts1 optsN err a f m))

  (define `(add-value value)
    (hash-bind (patsubst "-%" "%" (patsubst "-%" "%" opt)) value ovalues))

  (cond
   ;; done?
   ((not args)
    [files ovalues])

   ;; file
   ((not (filter "-%" opt))
    (recur (rest args) (conj files file) ovalues))

   ;; option
   ((filter opts0 opt)
    (recur (rest args) files (add-value 1)))

   ;; option=
   ((filter opts1 opt)
    (recur (rrest args) files (add-value (nth 2 args))))

   ;; option=...
   ((filter optsN opt)
    (recur nil files (add-value (nth-rest 2 args))))

   ;; error
   (else
    (err opt (rest args)))))


(define (getopts args opts err)
  (getopts-loop (filter-out "%= %=..." opts)
                (patsubst "%=" "%" (filter "%=" opts))
                (patsubst "%=..." "%" (filter "%=..." opts))
                err
                args
                nil
                nil))
