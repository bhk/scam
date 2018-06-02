;;----------------------------------------------------------------
;; trace.scm: tracing and debugging facilities
;;----------------------------------------------------------------

;; See reference.md for documentation.  Behavior not documented
;; there includes:
;;
;; - `pPREFIX` action prepends a user-supplied string to the function body.
;;
;; - `v...` prefix for actions details which functions were instrumented.
;;
;; - Function wildcards will not match names beginning with '~' and '^'
;;   UNLESS the pattern explicitly includes those characters.
;;
;; - If a function is instrumented a second time, the previous
;;   instrumentation will be replaced.

(define (trace-info a ?b ?c ?d)
  (info (concat "TRACE: " a b c d)))


;; Initialize count variables to this representation of 0.
(define `zero "///////")

;; Convert tally marks to decimal digits, padded with ":" on left.  The
;; number of digits in the result is one more than the number of digit
;; delimiters (/) in K.  A maximum of 8 digits is supported.
;;
(define (trace-digits k)
  ;; normalize
  (if (findstring "/1111111111" k)
      (trace-digits (subst "/1111111111" "1/" k))

      ;; convert to ASCII
      (let& ((digits (foreach d (concat "/" (subst "/" " /" k))
                              (words (subst "1" " 1" "/" "" d)))))
        ;; Convert leading 0's to :'s, but leave 0 if in 1's place.
        (subst " " "" ":0000" ":::::" ":00" ":::" ":0" "::" ":!:" "0" "!:" ""
               (concat "!:" digits "!:")))))


;; Construct a string with N words.
;;
(define (trace-words n ?str)
  (if (word n str)
      str
      (trace-words n (concat "1 " str))))


(define `(save-var id)
  (concat "[S-" id "]"))


(define `(count-var id)
  (concat "[K-" id "]"))


;; Call the original definition
;;
(define (trace-defer id)
  (concat "$(call " (save-var id) ",$1,$2,$3,$4,$5,$6,$7,$8,$9)"))


;; ENAME = function name *encoded* for RHS of asssignment
;;
(define (trace-body action ename id defn)
  (cond
   ;; count invocations
   ((filter "c" action)
    (define `cv (count-var id))
    (set-global cv (or (value cv) zero))
    (concat "$(eval " cv ":=$(subst /1111111111,1/,$(" cv ")1))"
            defn))

   ;; prefix
   ((filter "p%" action)
    (concat (promote (patsubst "p%" "%" action)) defn))

   ;; trace invocations and arguments
   ((filter "t%" action)
    (if (filter "^ta ^f ^tc ^tp ^n ^Y" ename)
        (trace-info "skipping [t] " ename)
        (concat "$(info $(^TI)--> (" ename "$(^ta)))"
                "$(call ^tp,$(^TI)<-- " ename ":,"
                "$(eval ^TI:=$$(^TI) )"                ;; increase indentation
                (trace-defer id)
                "$(eval ^TI:=$$(subst x ,,x$$(^TI)))"  ;; decrease indentation
                ")")))

   ;; multiply invocations
   ((filter "x%" action)
    (define `reps (or (patsubst "x%" "%" action) 11))
    (define `ws (rest (trace-words reps "1")))
    (subst "DO" (trace-defer id)
           (concat "$(foreach ^X,1,DO)"
                   "$(if $(^X),,$(if $(foreach ^X," ws ",$(if DO,)),))")))

   (else
    (error (concat "TRACE: Unknown action: '" action "'")))))


;; Get function names that match a name or pattern.  We avoid certain
;; subsets unless the specified pattern explicitly requests those names.
;;
(define (trace-match pat variables)
  (define `avoid-pats
    (foreach p "^% ~% ~trace%"
             (if (filter-out p pat)
                 p)))
  (filter-out avoid-pats (filter pat variables)))


;; List of NAME:ID pairs.
;;
(define *trace-ids* nil)


;; Get the ID for NAME.  If no ID has been assigned and create is non-nil,
;; assign one; otherwise return nil.
;;
(define (trace-id name ?create)
  (define `(filtersub pat-match pat-repl list)
    (patsubst pat-match pat-repl (filter pat-match list)))

  (or (filtersub (concat name ":%") "%" *trace-ids*)
      (if create
          (set *trace-ids*
               (concat *trace-ids* " " (concat name ":" (words *trace-ids*)))
               (words *trace-ids*)))))


;; Get list of names with assigned IDs.
;;
(define `known-names
  (filter-out ":%" (subst ":" " :" *trace-ids*)))


;; Provide a default and log if requested.  Return the action to be performed.
;;
(define (trace-action action name)
  (if (filter "v%" action)
      (foreach a (trace-action (patsubst "v%" "%" action) name)
               (trace-info "[" a "] " name)
               a)
      ;; default to "t" if not given
      (or action "t")))


;; Instrument functions as described in SPECS.  Return list of instrumented
;; function names.
;;
(define (trace-ext specs ignore-vars)

  ;; Break SPEC into name and action...
  (define `(spec-name spec)
    (filter-out ":%" (subst ":" " :" spec)))
  (define `(spec-action spec)
    (subst " " "" (wordlist 2 999 (subst ":" ": " spec))))

  ;; Filter out non-function variables
  (define `(only-funcs vars)
    (foreach v vars
             (if (filter "filerec%" (concat (origin v) (flavor v)))
                 v)))

  ;; Avoid modifying any function while it is currently being expanded,
  ;; which could trigger a GNU Make user-after-free bug.  In general we
  ;; do not know what's being expanded, and users have to take care, but
  ;; we can exclude what we know is on stack during REPL and in programs.
  ;;
  ;; The runtime contributes ignore-vars, which covers some file-recursive
  ;; variables that are not functions.
  ;;
  ;; Also avoid corruption save-var copies, and ^Y since it requires $(10).
  ;;
  ;; Also avoid most of the functions along the way out of `trace` and into
  ;; `untrace`.
  ;;
  (define `dangerous-vars
    "[% ~trace ~untrace ~trace-ext ~untrace-ext ^start ^Y "
    "~repl ~eval-and-print ~while ~while-0 ~while-N")

  (define `all-vars
    (declare .VARIABLES &global)
    (filter-out (concat ignore-vars " " dangerous-vars) .VARIABLES))

  ;; Apply instrumentation to a function
  ;;
  (define `(instrument action name id)
    ;; SCAM variables can contain `#` and `=`. `=` is ok on the RHS.
    (define `ename
      (subst "#" "$\"" name))

    ;; don't overwrite original if it has already been saved
    (if (filter "u%" (origin (save-var id)))
        (set-rglobal (save-var id) (value name)))

    (define `body
      (trace-body (trace-action action name) ename id (value (save-var id))))
    (set-rglobal name body))


  ;; Choose automatic vars extremely unlikely to shadow `(value NAME)`
  (define `instrumented-names
    (foreach
        _-spec specs
        (foreach
            _-name (only-funcs (trace-match (spec-name _-spec) all-vars))
            (foreach id (trace-id _-name 1)
                     (instrument (spec-action _-spec) _-name id)
                     _-name))))

  (filter "%" instrumented-names))


(define (trace-rev lst)
  (if lst
      (concat (trace-rev (wordlist 2 99999 lst)) " " (word 1 lst))))


;; Print function counts and reset them.
;;
(define (trace-dump names)
  (define `lines
    (foreach name names
             (foreach k (value (count-var (trace-id name)))
                      (if (findstring 1 k)
                          (begin
                            (set-global (count-var (trace-id name)) zero)
                            [(concat (subst ":" " " (trace-digits k)) " " name)])))))

  (for line (sort lines)
           (trace-info line)))


;; Remove instrumentation from functions listed in NAMES, or functions
;; matched by patterns in NAMES.
;;
(define (untrace-ext names)
  (define `matched-names
    (foreach name names
             (trace-match name known-names)))

  (define `untraced-names
    (foreach name matched-names
             (foreach id (trace-id name)
                      ;; restore original definition
                      (set-rglobal name (value (save-var id)))
                      name)))

  (trace-dump untraced-names))


(at-exit (lambda () (trace-dump known-names)))
