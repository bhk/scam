;;----------------------------------------------------------------
;; trace.scm: tracing and debugging facilities
;;----------------------------------------------------------------

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
;;    Note that instrumentation of functions is not performed until after a
;;    module is loaded.  When code within a module that executes at load
;;    time -- i.e. during the call to `require` -- functions defined within
;;    that module will not be subject to tracing.
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
;; functions).  For each matching function, every specified action is
;; performed.
;;
;; Each `action` is one of the strings listed below.  If no actions are
;; given, `t` is assumed.
;;
;;    "t" : print the function name and arguments on entry and its return
;;          value on exit.
;;    "c" : count the number of times that the function is invoked.
;;    "p" : Prefix a user-defined string to the function body.  The
;;          prefix is given in the `SCAM_PRE` variable.
;;    "x<N>" or "X<N>: repeat the function <N> times every time it is
;;          invoked.  <N> may be a positive integer, or the empty string
;;          (which is treated as "11").  "x..." instruments the function in
;;          a way that will fail if the function recurses.  "X..." supports
;;          recursion but has slightly higher overhead.
;;    "v" : output the name and value of the function as it is instrumented.
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

(declare SCAM_TRACE &global)
(declare SCAM_PRE &global)
(declare .VARIABLES &global)


;; Variables to ignore when matching wildcards
(define *trace-ignore-vars* "")

(define `variables
  (filter-out *trace-ignore-vars* (subst "%" "()" .VARIABLES)))


;; Current active trace specs
;;
(define *traces* "")


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
  (if (not (findstring "i" k))
      (trace-digits (concat "i" k))
      (if (findstring "ioooooooooo" k)
          (trace-digits (subst "ioooooooooo" "oi" k))

          (let& ((digits (foreach d (subst "i" " i" k)
                           (words (subst "o" " o" "i" "" d)))))
            (subst " " ""
                   (wordlist (words (subst "i" " i" k))
                             99
                             (concat ". . . . . . . . " digits)))))))


;; Convert numbers to sortable strings:  LOG10.DIGITS
;; Lexicographical sort == numeric sort
(define (trace-n2a count)
  (if (not (filter "i%" count))
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
         (subst "N-1" (rest (list-of (or reps 11)))
                (if recur
                    ;; more complicated when recursion must be supported
                    "$(if $(^X),$(call if,,,$(value NAME)),$(if $(foreach ^X,N-1,$(if $(NAME),)),)$(foreach ^X,0,$(NAME)))"

                    ;; simpler when we assume it will not recurse
                    "$(NAME)$(if $(foreach ^xx,N-1,$(NAME)),)"))))


(define (trace-info str a b c d)
  (print "TRACE: " str a b c d))

;; Return functions identified by `pat`.  `pat` matches non-ignored
;; variables if it contains '%' ... otherwise an exact match is required.
;;
;;
(define (trace-match-funcs pat)
  (foreach v (if (findstring "%" pat)
                 (filter pat variables)
                 pat)
           (if (filter "recur%" (flavor v))
               v)))


;; Apply one type of instrumentation to one function.  Return new function
;; body.
;;
(define (trace-instrument action name defn)
  (cond
   ;; display matching names
   ((filter "v" action)
    (trace-info name " [" (flavor name) "] = " (value name))
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
         (trace-info "SCAM_PRE undefined; needed for " name ":p"))
     defn))

   ;; trace invocations and arguments
   ((filter "t" action)
    (subst "CODE" defn
           "$(info --> ($0$(^ta)))$(call ^tp,<-- $0:,CODE)"))

   (else (begin
           (trace-info "Unknown action: '" action "'")
           defn))))


(define *traces-active* "")

;; Instrument all functions that are not already instrumented.
;;
(define (trace-check)
  (define `(check name action)
    (define `id (concat name ":" (patsubst "x%" "x" (subst "X" "x" action))))
    (if (not (filter id *traces-active*))
        (begin
          (set-rglobal name (trace-instrument action name (value name)))
          id)))

  (define `new-ids
    (foreach w *traces*
             (foreach name (trace-match-funcs (firstword (subst ":" " % " w)))
                      (foreach action (or (rest (subst ":" " " (concat "." w))) "t")
                               (check name action)))))

  (set *traces-active* (strip (concat *traces-active* " " new-ids))))


(define (trace-rev lst)
  (if lst
      (concat (trace-rev (rest lst)) " " (firstword lst))))


(define (trace-dump)
  ;; warn of non-matched patterns
  (define `(extract-names specs)
    (foreach s specs (word 1 (subst ":" " " s))))

  (foreach s (extract-names *traces*)
           (if (not (filter s (extract-names *traces-active*)))
               (trace-info "spec '" s "' did not match any functions.")))

  (if (filter "%c" *traces-active*)
      (begin
        (trace-info "function invocations")
        (foreach r
                 (trace-rev (sort
                             (foreach V (filter "^K_%" .VARIABLES)
                                      (concat (trace-digits (value V))
                                              (patsubst "^K_%" "::%" V)))))
                 (let& ((lst (subst "::" " " r))
                        (count (subst "." " " (word 1 lst)))
                        (name (word 2 lst)))
                   (trace-info count " : " name))))))


;; Add a trace spec to the active set or traces.
;;
(define (trace spec)
  (set *traces* (concat *traces* " " spec))
  (trace-check))


;; Ingore all variables defined in this module and earlier.
(set *trace-ignore-vars* variables)

;; Establish environment-specified traces
(trace SCAM_TRACE)
(add-hook "load" (global-name trace-check))
(add-hook "exit" (global-name trace-dump))
