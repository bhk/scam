;; # trace: Tracing and Profiling
;;
;; The `trace` module performs run-time instrumentation of functions.
;;
;; The function [`trace`](#trace-specs) can be called to instrument
;; functions at any point in time.
;;
;; The [`tracing`](#tracing-specs-expr) macro can be used to perform tracing
;; with a limited duration.
;;
;; When the `trace` module is *present*, it also recognizes `SCAM_TRACE` and
;; enhances the output of the [`?` special form](#-fn-args) for call-site
;; tracing.
;;
;; ## `SCAM_TRACE`
;;
;; If the `SCAM_TRACE` environment variable is set, the "trace" module, when
;; present, will call `(trace SCAM_TRACE)` on program startup, prior to the
;; loading of the main module.
;;
;; Regarding whether the `trace` module is present: The `trace` module is
;; present in the SCAM compiler, and therefore also in programs that are run
;; by the SCAM compiler in [immediate mode](reference.md#the-scam-compiler)
;; (which includes `-q` test programs).  In the case of executables that
;; have been built by the SCAM compiler, it is present only if included --
;; anywhere in that program -- with `(require "trace")`.
;;
;; ## Trace Specifications
;;
;; The `SPECS` argument passed to `trace` or `tracing` or contained in
;; `SCAM_TRACE` specifies which functions are to be instrumented and what
;; information is to be reported.
;;
;;     SPECS := SPEC (` ` SPEC)*
;;     SPEC := NAME (`:` MODE)?
;;
;; In its simplest form, it is a list of function names.
;;
;; Names that include a `%` character are treated as wildcards that match
;; currently-defined functions.  Additionally, the name may be enclosed in
;; double-quotes to indicate the [native name](#native-name-var) of a
;; function.
;;
;; Names may be followed by a `:` character followed by a *mode*.  Possible
;; modes are:
;;
;;  - `t` : Print the function name and arguments when it is called and its
;;          return value when it returns.  This is the default mode.
;;
;;  - `f` : Print just the function name on entry and exit.
;;
;;  - `c` : Count the number of times that the function is invoked.
;;          Function counts will be written to stdout when tracing is
;;          removed.  This can occur when `(tracing ...)` completes, or when
;;          the program exits.
;;
;;  - `xN` : Evaluate the function body N times each time the function is
;;          invoked.  N must be a positive number or the empty string
;;          (which is treated as 11).
;;
;;  - `-` : Exclude the function(s) from instrumentation.  Any functions
;;          matched by this entry will be skipped even when they match other
;;          entries in this specification string.  This does not depend on
;;          the ordering of entries.  For example, `(trace "a% %z:-")` will
;;          instrument all functions whose names begin with `a` except for
;;          those whose names end in `z`.
;;
;; Some caution must be exercised when tracing functions, especially with
;; wildcards:
;;
;;  1. Tracing a function *while it is executing* can run afoul of a bug in
;;     GNU Make and cause a fatal exception.  This should not occur when you
;;     are using `SCAM_TRACE` or calling tracing functions from the
;;     top-level of a module (unless native names are used) or from the
;;     REPL.
;;
;;  2. Tracing a function that is used by the tracing infrastructure itself
;;     can lead to infinite recursion.  This can only occur if you use
;;     native naming to match system-provided functions, or use wildcards
;;     with native names.
;;
;; The intent of `x` instrumentation is to cause the function to consume
;; more time by a factor of N (for profiling purposes).  It does this by
;; repeatedly executing the function one each invocation, returning only the
;; first result.  When `x` is used with recursive functions, the
;; multiplication of work only occurs at the outermost calls, which should
;; produce the desired effect.  The `x` mode should be used only with
;; functions that operate without side effects.  If your code employs side
;; effects, then this might break your program and it might not provide
;; meaningful information anyway.

(require "core.scm")


(define `(defined? var)
  (filter-out "u%" (native-origin var)))

;; Get native (prefixed) name or pattern for user-facing NAME.  User
;; namespace is the default assumption.
;;
;;     X   -> 'X
;;     `X  -> `X
;;     "X" -> X
;;
(define (trace-prefix name)
  (or (filter "`%" name)
      (if (filter "\"%\"" name)
          (patsubst "\"%\"" "%" name)
          (.. "'" name))))


;; Get user-facing function name for VAR.
;;
(define (trace-unprefix var)
  (or (filtersub "'%" "%" var)
      (filter "`%" var)
      (.. "\"" var "\"")))


(define (trace-is-func var)
  (if (filter "filerec%" (.. (native-origin var) (native-flavor var)))
      var))

;;------------------------------------------------------------------------
;; trace indent
;;------------------------------------------------------------------------

(define TI nil)
(define `nnTI (native-name TI))

(define (TI++)
  (.. TI (native-eval (.. nnTI ":=$(" nnTI ") "))))

(define (--TI)
  (.. (native-eval (.. nnTI ":= $" TI)) TI))


;;------------------------------------------------------------------------
;; `?` macro support:  override E? and R?
;;------------------------------------------------------------------------

(define (E? fname ...args)
  (print (native-var (native-name TI++)) "--> (" (trace-unprefix fname) " "
         (foreach ([a] args) (format a)) ")"))


(define (R? fname value)
  (print (native-var (native-name --TI)) "<-- " (trace-unprefix fname) ": " (format value))
  value)


;;------------------------------------------------------------------------
;; tally-XXX: efficient invocation counting mechanism
;;------------------------------------------------------------------------

(define `tally-zero "////////")

(define `(tally-1+ k)
  (subst "/1111111111" "1/" (.. k 1)))

(define (tally-decimal k)
  ;; propagate carry if pending...
  (if (findstring "/1111111111" k)
      (tally-decimal (subst "/1111111111" "1/" k))

      (begin
        (define `digits
          (foreach (digit (subst "/" " /" k))
            (words (subst "/" "" "1" "1 " digit))))
        ;; remove up to 7 leading zeros
        (patsubst "0%" "%"
                  (patsubst "00%" "%"
                            (patsubst "0000%" "%"
                                      (subst " " "" digits)))))))


;;------------------------------------------------------------------------
;; trace / tracing
;;------------------------------------------------------------------------
;;
;; Undocumented behavior includes:
;;
;;  - `NAME : matches functions in the system namespace (DANGEROUS)
;;  - `pPREFIX` mode prepends a user-supplied GNU Make prefix (demoted) to
;;    the function body.


;; Dictionary of {VARNAME: ID}.  VARNAME is the native name of a
;; function beind traced. ID is a short identifiers used to construct
;; save- and tally-variables.
;;
(define *trace-ids* nil)
(define `(save-var id) (.. "[S-" id "]"))
(define `(tally-var id) (.. "[K-" id "]"))


;; Get the ID for VARNAME.  If no ID has been assigned and create is non-nil,
;; assign one; otherwise return nil.
;;
(define (trace-id varname ?create)
  (or (dict-get varname *trace-ids*)
      (if create
          (foreach (id (words *trace-ids*))
            (set *trace-ids* (append *trace-ids* {=varname: id}))
            id))))


(define (tf-args-initial str)
  (if str (.. " " str) str))
(define (tf-args ...args)
  (tf-args-initial (foreach ([a] args) (format a))))


(define (tf-pv value pre)
  (print pre " " (format value))
  value)


;; ENAME = function name *encoded* for RHS of asssignment
;;
(define (trace-body mode ename id defn)
  (define `template
    (cond
     ;; trace invocations and arguments
     ((filter "t" mode)
      ;; function, args, and return value
      "$(info :+--> (:N:A))$(call :V,:C,:-<-- :N:)")

     ;; fast, function name only
     ((filter "f" mode)
      "$(info :+--> :N):C$(info :-<-- :N)")

     ;; count invocations
     ((filter "c" mode)
      (define `cv (tally-var id))
      (or (native-value cv)
          (set-native cv (or (native-value cv) tally-zero)))
      (.. "$(eval " cv ":=" (lambda () (tally-1+ (native-var cv))) "):D"))

     ;; multiply invocations
     ((filter "x%" mode)
      (define `reps (or (patsubst "x%" "%" mode) 11))
      (define `rep-words (patsubst "%" 1 (urange 2 reps)))
      (.. "$(foreach ^X,1,:C)"
          "$(if $(^X),,$(if $(foreach ^X," rep-words ",$(if :C,)),))"))

     ;; prefix
     ((filter "p%" mode)
      ;; prevent unintential processing of template code
      (.. (subst ":" ":$ " (promote (patsubst "p%" "%" mode))) ":D"))

     (else
      (error (.. "TRACE: Unknown mode: '" mode "'")))))

  ;; Expand an instrumentation template.
  (subst
   ":N" (trace-unprefix ename)
   ":+" (.. "$(" (native-name TI++) ")")
   ":-" (.. "$(" (native-name --TI) ")")
   ":V" (native-name tf-pv)
   ":A" (.. "$(" (native-name tf-args) ")")
   ":C" (.. "$(call " (save-var id) ",$1,$2,$3,$4,$5,$6,$7,$8,$9)")
   ":D" defn ;; do this last, because we don't know what it contains
   template))


;; Return function matching pattern or name NAME.  Warn if none are found.
;;
(define (trace-match name)
  (declare .VARIABLES &native)

  ;; search for matches among recursive file-origin variables
  (foreach (var (or (trace-prefix name) "!"))
    (cond
     ((filter "! '" var)
      (print "scam: trace: empty name in trace spec!"))

     ((findstring "%" name)
      (foreach (v (filter var .VARIABLES))
        (trace-is-func v)))

     (else (trace-is-func var)))))


;; SPEC parsing

(define `(spec-name spec)
  (filter-out ":%" (subst ":" " :" spec)))
(define `(spec-mode spec)
  (or (subst " " "" (wordlist 2 999 (subst ":" ": " spec)))
      "t"))

;; {ID: MODE} of reported instrumentation.  Display tracing aanouncement
;; only once per function/mode.
(define *trace-reported* nil)


;; Instrument functions as described by SPECS, a list of [trace
;; specifications](#trace-specifications).
;;
;; Return a list of native names of the instrumented functions.
;;
;; When `trace` is called, it will instrument specified functions *if* they
;; have already been defined, and it will cause tracing to be updated
;; immediately after any new module is loaded.  Note that during the
;; execution of a module, functions defined early in the module will remain
;; un-instrumented until the module completes loading (or until tracing is
;; explicitly added with `trace` or `tracing`).
;;
;; `trace` can be performed repeatedly on the same function; only the most
;; recently-named tracing mode will remain in effect.  Invocation counts
;; will not be reset by new calls to trace.
;;
(define (trace specs)
  &public

  ;; Find variables matching the spec pattern
  (define `(match-vars spec)
    (filter-out (foreach (p (filtersub "%:-" "%" specs))
                  (trace-prefix p))
                (trace-match (spec-name spec))))

  ;; Apply instrumentation to a function whose native name is VAR
  (define `(instrument var mode)
    ;; SCAM variables can contain `#` and `=`. `=` is ok on the RHS.
    (define `evar
      (subst "#" "$\"" var))

    (foreach (id (trace-id var 1))
      ;; don't overwrite original if it has already been saved
      (when (filter-out (dict-get id *trace-reported*) mode)
        (print "scam: tracing " (trace-unprefix var) " [mode=" mode "] ...")
        (set *trace-reported* (._. *trace-reported* {=id: mode})))
      (if (not (defined? (save-var id)))
          (set-native-fn (save-var id) (native-value var)))
      (define `body
        (trace-body (or mode "t") evar id (native-value (save-var id))))
      (set-native-fn var body)))

  (define `instrumented-vars
    (foreach (spec (filter-out "%:-" specs))
      (foreach (var (match-vars spec))
        (instrument var (spec-mode spec))
        var)))

  (filter "%" instrumented-vars))


;; Remove instrumentation from functions listed in VARS.  For any functions
;; currently instrumented for counting, report those counts and clear them.
;;
(define (untrace vars retval)
  (set *trace-reported* nil)
  (define `count-map
    (foreach (var vars)
      (foreach (id (trace-id var))
        (when (defined? (save-var id))
          ;; restore original definition
          (set-native-fn var (native-value (save-var id)))
          ;; return {var: count} if present (nil => unset)
          (foreach (k (native-var (tally-var id)))
            (set-native (tally-var id) nil)
            { (trace-unprefix var): (tally-decimal k) })))))

  (let ((map count-map))
    (if map
        (for ({=key: value} map)
          (print "scam: counts: " key " = " value))))

  retval)


;; Display pending tracing metrics for any remaining traced functions.
;;
(define (trace-exit)
  (untrace (dict-keys *trace-ids*) nil))


;; Evaluate EXPR while functions are instrumented according to
;; [SPECS](#trace-specifications).  On return, instrumentation is removed
;; and invocation counts will be reported, and then reset, for any functions
;; instrumented with mode `c`.
;;
;; See the [reference manual](reference.md#tracing-examples) for examples.
;;
(define `(tracing specs expr)
  &public
  (untrace (trace specs) expr))


;; Like [`expect`](#expect-a-b), but evaluation of A and B is done with
;; tracing enabled.
;;
(define `(trace-expect a b)
  &public
  (let ((ab (tracing "%" [a b])))
    (expect (nth 1 ab) (nth 2 ab))))


;;------------------------------------------------------------------------
;; On load...
;;------------------------------------------------------------------------

(declare SCAM_TRACE &native)

;; should be called immediately after this module exist
(define (trace-after-load)
  (if SCAM_TRACE
      (trace SCAM_TRACE)))

(at-exit (lambda () (trace-exit)))
