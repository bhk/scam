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
    (append out (foreach (w (rest argv))
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
;; `ARGV` is a vector of command line arguments that will be parsed as a
;; sequence of options, option values, and/or non-option arguments.
;; Generally, options may appear in any order, before or after non-option
;; arguments, but an argument of `--` indicates that all subsequent
;; arguments are to be treated as non-option arguments.
;;
;; The result is a dictionary that maps each option name to a *vector* of
;; values, or nil.  If an option was specified more than once in `OPTS`, the
;; vector will hold one eleent for each occurrence.
;;
;; The key `"*"` holds all non-option arguments.
;;
;; If errors were encountered, the key `"!"` holds a vector of
;; `GetoptsError` records:
;;
;;  - `(MissingArg OPT)` : Option specifier `OPT` describes an option that
;;    takes an argument, but the option appeared in the last element of
;;    `ARGV`, so its argument is missing.
;;  - `(BadOption ARG)` : Argument `ARG` began with "-" but did not match any
;;    option specifiers.
;;
;; `OPTS` describes the options that may be provided and whether or not
;; values are expected with them.  Each word in `OPTS` is an "option
;; specifier"; it begins with `-` or `--`, followed by an option name, and
;; optionally ending with `=`.  Option names may not contain `%`, `!`, `*`,
;; or whitespace.
;;
;; If an option specifier ends in `"="`, it indicates that the option (when
;; found in `ARGV`) will be followed by another argument that contains the
;; value to be associated with the option.  When an option specifier does
;; not end in `"="`, value `1` is supplied for each occurrence of the
;; option.
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
