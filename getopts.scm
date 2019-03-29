;; # getopts: Command Line Options Parser

(require "core.scm")

(data GetoptsError
  &public
  ;; Option OPT consumes an argument but was seen at the end of argv
  (MissingArg opt)
  ;; An argument began with `-` but did not match a specified option.
  (BadOption arg))


(define (opt-pair opt value)
  (define `opt-name (patsubst "-%" "%" (patsubst "-%" "%" opt)))
  { =opt-name: value })


;; OPTS0 = options with no arguments (flags)
;; OPTS1 = options with one argument
;; ARGV = remaining arguments
;; OUT = result dictionary so far
;;
(define (getopts-loop opts0 opts1 argv ?out)
  ;; options may not contain whitespace or "!", so first=firstword
  (define `opt (word 1 argv))

  (define `(recur new-value next-index)
    (getopts-loop opts0 opts1
                  (nth-rest (or next-index 2) argv)
                  (append out new-value)))

  (cond
   ;; done
   ((not opt)
    out)

   ;; stop processing
   ((filter "--" opt)
    (append out (foreach w (rest argv)
                         { *: (promote w) })))

   ;; option
   ((filter opts0 opt)
    (recur (opt-pair opt 1) 2))

   ;; option=
   ((filter opts1 opt)
    (if (word 2 argv)
        (recur (opt-pair opt (nth 2 argv)) 3)
        (append out { !: (MissingArg opt) })))

   ;; bad arg
   ((filter "-%" opt)
    (recur {!: (BadOption opt)} 2))

   ;; non-option arg
   (else
    (recur {*: (promote opt)} 2))))


;; Parse command line options.
;;
;; ARGV = arguments vector\
;; OPTS = a string of option specifiers\
;; Result = a dictionary describing options, non-option arguments, and errors.
;;
;; Any non-option command-line arguments will appear in the result in a
;; vector bound to the key `"*"`.  In other words, `(dict-get "*" RESULT)`
;; yields all of the non-option arguments.
;;
;; Option specifiers in OPTS may begin with `"-"` or `"--"`.  Leading dashes
;; are not included in the dictionary keys.  Option names may not contain
;; `%`, `!`, `*`, or whitespace.
;;
;; Options can appear multiple more times.  If not seen, the option name
;; will not appear in the result.  Otherwise, it will be bound to a vector
;; that contains one value per occurrence:
;;
;;  - If an option specifier ends in `"="`, this indicates that the option
;;    consumes an argument.  Its values will be the consumed arguments.
;;
;;  - If an option does not end in `"="`, its values will all be `1`.
;;
;; If `--` is seen in ARGV, all elements following `--` are treated as
;; non-option arguments.  Otherwise, options can appear in any order, before
;; and after non-option arguments.
;;
;; If errors are encountered, a `"!"` entry in the dictionary will exist,
;; containing `GetoptsError` records.
;;
;;  - `(MissingArg OPT)` : Option specifier OPT takes an argument but was found
;;    in last element of argv.
;;  - `(BadOption ARG)` : Argument ARG began with "-" but did not match any
;;    option specifiers.
;;
;; Example:
;;
;;     (getopts ["a" "-f" "--g" "x" "b"]      ;; command line as in `argv`
;;              "-f --g= -h")                 ;; option description
;;     {f: 1, g: "x", *: ["a" "b"] }          ;; result
;;
(define (getopts argv opts)
  &public
  (dict-collate
   (getopts-loop (filter-out "%=" opts)
                 (filtersub "%=" "%" opts)
                 argv)))
