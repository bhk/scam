;; Usage:
;;
;; This module instruments functions at run time to log and report
;; tracing data.  It can be used in two ways:
;;
;;  * Assigning `SCAM_TRACE` on the command line or in the environment
;;    before running a SCAM program will enable tracing during the execution
;;    of the program's `main` function.  The value of SCAM_TRACE is a
;;    "trace specification" (described below).
;;
;;  * Any SCAM program can require the "trace" module and call the `trace`
;;    function to instrument functions.  `trace` accepts a trace
;;    specification as an argument.  The function `trace-dump` can be
;;    called to display function invocation counts (when that kind of
;;    tracing has been requested).
;;
;; A trace specification is one or more space-delimited words, each of which
;; has following format:
;;
;;      fnpat [ ":" action ]*
;;
;; `fnpat` is a string used to match defined functions (see "Function
;; Matching", below).  If empty, it defaults to "%" (matching all
;; functions).  If no actions are given, "t" is used.  Supported actions
;; are:
;;
;;    "t" : log each function name and arguments whenever it is invoked, and
;;          log its return value when it returns.
;;    "c" : count the number of times that each function is invoked.
;;    "p" : Prefix a user-defined string to each function body.  The
;;          prefix is given in the `SCAM_PRE` variable.
;;    "x<N>" or "X<N>: repeat each function <N> times every time it is
;;          invoked.  <N> may be a positive integer, or the empty string
;;          (which is treated as "11").  "x..." instruments the function in
;;          a way that will fail if the function recurses.  "X..." supports
;;          recursion but has slightly higher overhead.
;;    "v" : output the name of functions matching fnpat.
;;
;; Some example trace specifications:
;;
;;    "foo"  =>  log arguments and return values every time `foo` is invoked.
;;    "%:c"  =>  count the number of invocations of every function.
;;    "foo:t:c"  ==> log invocations of `foo` and count them.
;;
;; Function matching:
;;
;;    Function names can include the wildcard character ("%"), in which case
;;    it will be matched with the set of functions that have been defined
;;    *after* the trace module was loaded.  Explicitly named functions will
;;    always be instrumented, even if the function was defined before the
;;    trace module was loaded.
;;
;; Some example command lines:
;;
;;    This counts the number of times all functions are executed
;;    when compiling num.scm:
;;
;;       % SCAM_TRACE=':c' bin/scam num.scm
;;


(declare SCAM_TRACE)
(declare SCAM_DEBUG)
(declare SCAM_PRE)
(declare .VARIABLES)
(define *trace-V0* "")

(define (undef? name)
  (filter "undefined" (flavor name)))

;; avoid dependency on `core.scm`; use this instead of `rest`
(define `(cdr words)
  (wordlist 2 99999999 words))

;; command-line values are recursive (!) and also they are evaluated
;; by make after rules are processed, so SCAM_PRE will be executed
;; an extra time after the program completes unless we do this...
;;
(eval "override SCAM_PRE := $(value SCAM_PRE)")

;; ^K: increment the count of invocations of $0
;;
;; This counting system requires just patsubst+concat+subst for each increment.
;; Base 4 is optimal for size: digit len = (4+1)/2 ; num digits = 1/Ln(4)
;; Base 10 is easy to convert to decimal ASCII, however.
;;
(eval "^K = $(eval ^K_$0:=$(subst ioooooooooo,oi,$(^K_$0:o%=io%)o))")


(define (trace-digits k)
  ;; normalize
  (if (if (findstring "i" k) "" 1)
      (trace-digits (concat "i" k))
      (if (findstring "ioooooooooo" k)
          (trace-digits (subst "ioooooooooo" "oi" k))

          (let&
           ((digits (foreach d (subst "i" " i" k)
                             (words (subst "o" " o" "i" "" d)))))
           (subst " " ""
                  (wordlist (words (subst "i" " i" k))
                            99
                            (concat ". . . . . . . . " digits)))))))


;; Convert numbers to sortable strings:  LOG10.DIGITS
;; Lexicographical sort == numeric sort
(define (trace-n2a count)
  (if (if (filter "i%" count) "" 1)
      (trace-n2a (concat "i" count))
      (if (findstring "ioooooooooo" count)
          (trace-n2a (subst "ioooooooooo" "oi" count))
          (concat
           (subst 10 "A" (words (subst "i" " i" count)))
           "!0"
           (subst " " ""
                  (foreach d (subst "i" " i" count)
                           (words (subst "o" " o" "i" "" d))))))))


(define (list-of n lst)
  (if (word n lst)
      lst
      (list-of n (concat lst " x"))))


;; Return a function body that invokes the named function multiple times,
;; and returns the result of the last invocation.
;;
;;   fname = name of function to call <reps> times (the original definition)
;;   reps = number of repetitions
;;   recur = if true, recursion is expected.
;;
(define (trace-repeater fname reps recur)
  (subst "NAME" fname
         (subst "N-1" (cdr (list-of (or reps 11)))
                (if recur
                    ;; more complicated when recursion must be supported
                    "$(if $(^X),$(call if,,,$(value NAME)),$(if $(foreach ^X,N-1,$(if $(NAME),)),)$(foreach ^X,0,$(NAME)))"

                    ;; simpler when we assume it will not recurse
                    "$(NAME)$(if $(foreach ^xx,N-1,$(NAME)),)"))))


(define (trace-warn str)
  (info (concat "TRACE: " str)))


;; Return functions identified by `pat`.
;;
(define (trace-match-funcs pat warn)
  (or (foreach v (concat pat " " (filter pat (filter-out
                                              *trace-V0*
                                              (subst "%" "()" .VARIABLES))))
               (if (filter "recur%" (flavor v))
                   v))
      (warn (concat "no functions match '" pat "'"))))


;; Apply one type of instrumentation to one function.  Return new function
;; body.
;;
(define (trace-instrument action name defn warn)
  (if (findstring "T" SCAM_DEBUG)
      (print "instrumenting: " name " for " action))
  (cond
   ;; display matching names
   ((filter "v" action)
    (warn (concat "instrumenting '" name "'"))
    defn)

   ;; count invocations
   ((filter "c" action) (concat "$(^K)" defn))

   ;; multiply invocations
   ((filter "x% X%" action)
    (set-rglobal (concat name "~0~") defn)
    (trace-repeater (concat name "~0~")
                    (patsubst "x%" "%" (subst "X" "x" action))
                    (filter "X%" action)))

   ;; prefix
   ((filter "p" action)
    (concat
     (or SCAM_PRE
         (warn (concat "SCAM_PRE undefined; needed for " name ":p")))
     defn))

   ;; trace invocations and arguments
   ((filter "t" action)
    (subst "CODE" defn
           "$(info --> ($0$(^ta)))$(call ^tp,<-- $0:,CODE)"))

   (else (begin
           (warn (concat "Unknown action: '" action "'"))
           defn))))


;; Instrument functions as described in spec
;;
(define (trace spec warn)
  (foreach
   w spec
   (foreach
    name (trace-match-funcs (firstword (subst ":" " % " w))
                            (or warn trace-warn))
    (foreach
     action (or (cdr (subst ":" " " (concat "." w))) "t")
     (let ((catvar (concat "*traced*-"
                           (patsubst "x%" "x" (subst "X" "x" action)))))

       (if (filter name (value catvar))
           "" ; already instrumented in this manner
           (begin
             (set-global catvar (concat (value catvar) " " name))
             (set-rglobal name (trace-instrument action name (value name)
                                           (or warn trace-warn))))))))))


(define (trace-rev lst)
  (if lst
      (concat (trace-rev (cdr lst)) " " (firstword lst))))

(define (trace-dump)
  (if (value "*traced*-c")
      (begin
        (trace-warn "function invocations")
        (foreach r
                 (trace-rev (sort
                             (foreach V (filter "^K_%" .VARIABLES)
                                      (concat (trace-digits (value V))
                                              (patsubst "^K_%" "::%" V)))))
                 (let& ((lst (subst "::" " " r))
                        (count (subst "." " " (word 1 lst)))
                        (name (word 2 lst)))
                 (trace-warn (concat count " : " name)))))))


(set *trace-V0* (subst "%" "()" .VARIABLES))
