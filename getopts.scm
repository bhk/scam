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
;; ARGV = argument vector, as supplied to `main`\
;; OPTS = a string of option specifiers\
;; Result = a dictionary
;;
;; The resulting dictionary maps each option name to a *vector* of values
;; supplied for that option.  Options can appear zero or more times.  If not
;; seen, the option name will not appear as a key in the dictionary.
;;
;; The key `"*"` holds elements in ARGV that were not options or option
;; arguments.  In other words, `(dict-get "*" RESULT)` yields all of the
;; non-option arguments.
;;
;; If errors were encountered, the key `"!"` holds a vector of
;; `GetoptsError` records:
;;
;;  - `(MissingArg OPT)` : Option specifier OPT takes an argument but was found
;;    in last element of argv.
;;  - `(BadOption ARG)` : Argument ARG began with "-" but did not match any
;;    option specifiers.
;;
;; Words in OPTS begin with `-` or `--` and may end with `=`.  Leading
;; dashes and the trailing `=` are not included in the dictionary keys.
;; Option names may not contain `%`, `!`, `*`, or whitespace.
;;
;; If an option specifier ends in `"="`, the value will be the next argument
;; in ARGV (which will be consumed).  Otherwise, the value `1` is supplied
;; for each occurrence of the option.
;;
;; If `--` is seen in ARGV, all elements following `--` are treated as
;; non-option arguments.  Otherwise, options can appear in any order, before
;; and after non-option arguments.
;;
;; Example:
;;
;;     > (getopts ["a" "-f" "-f" "--g" "x" "b c" "--" "-f"]
;;     +          "-f --g= -h")
;;     {*: ["a" "b c" "-f"], f: [1 1], g: "x"}
;;
(define (getopts argv opts)
  &public
  (dict-collate
   (getopts-loop (filter-out "%=" opts)
                 (filtersub "%=" "%" opts)
                 argv)))
