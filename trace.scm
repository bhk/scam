;;----------------------------------------------------------------
;; trace.scm: tracing and debugging facilities
;;----------------------------------------------------------------

;; See reference.md for documentation.  Behavior not documented
;; there includes:
;;
;; - `pPREFIX` mode prepends a user-supplied string to the function body.
;;
;; - An entry with `:v` suffix enables verbose mode.
;;
;; - Function wildcards will not match names beginning with '~' and '^'
;;   UNLESS the pattern explicitly includes those characters.
;;
;; - If a function is instrumented a second time, the previous
;;   instrumentation will be replaced.


(define (trace-info a ?b ?c ?d)
  (info (concat "TRACE: " a b c d)))


(define `(filtersub pat-match pat-repl list)
  (patsubst pat-match pat-repl (filter pat-match list)))


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
        (subst " " "" ":0000" ":::::" ":00" ":::" ":0" "::" ":!" "0!" "!:" ""
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


;; ENAME = function name *encoded* for RHS of asssignment
;;
(define (trace-body mode ename id defn)
  (define `template
    (cond
     ;; count invocations
     ((filter "c" mode)
      (define `cv (count-var id))
      (set-global cv (or (value cv) zero))
      (concat "$(eval " cv ":=$(subst /1111111111,1/,$(" cv ")1)):D"))

     ;; prefix
     ((filter "p%" mode)
      ;; prevent unintential processing of template code
      (concat (subst ":" ":$ " (promote (patsubst "p%" "%" mode))) ":D"))

     ;; trace invocations and arguments
     ((filter "t f" mode)
      (if (or (filter "f" mode)
              ;; avoid infinite recursion
              (filter "^ta ^f ^tc ^tp ^n" ename))
          ;; fast, function name only
          "$(:I--> :N):E$(:I<-- :N)"
          ;; function, args, and return value
          "$(:I--> (:N$(^ta)))$(call ^tp,$(^TI)<-- :N:,:E)"))

     ;; multiply invocations
     ((filter "x%" mode)
      (define `reps (or (patsubst "x%" "%" mode) 11))
      (define `ws (rest (trace-words reps "1")))
      (concat "$(foreach ^X,1,:C)"
              "$(if $(^X),,$(if $(foreach ^X," ws ",$(if :C,)),))"))

     (else
      (error (concat "TRACE: Unknown mode: '" mode "'")))))

  ;; Expand an instrumentation template.
  (subst
   ":I" "info $(^TI)"
   ":E" "$(eval ^TI:=$$(^TI) ):C$(eval ^TI:=$$(subst x ,,x$$(^TI)))"
   ":C" (concat "$(call " (save-var id) ",$1,$2,$3,$4,$5,$6,$7,$8,$9)")
   ":N" ename
   ":D" defn ;; do this last, because we don't know what it contains
   template))


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
  (or (filtersub (concat name ":%") "%" *trace-ids*)
      (if create
          (set *trace-ids*
               (concat *trace-ids* " " (concat name ":" (words *trace-ids*)))
               (words *trace-ids*)))))


;; Get list of names with assigned IDs.
;;
(define `known-names
  (filter-out ":%" (subst ":" " :" *trace-ids*)))


;; Instrument functions as described in SPECS.  Return list of instrumented
;; function names.
;;
(define (trace-ext specs ignore-vars)

  ;; Break SPEC into name and mode...
  (define `(spec-name spec)
    (filter-out ":%" (subst ":" " :" spec)))
  (define `(spec-mode spec)
    (subst " " "" (wordlist 2 999 (subst ":" ": " spec))))

  ;; Avoid modifying any function while it is currently being expanded,
  ;; which could trigger a GNU Make user-after-free bug.  In general we do
  ;; not know what's being expanded, and users have to take care, but we can
  ;; exclude what we know is problematic.  The runtime contributes
  ;; IGNORE-VARS, which covers some file-recursive variables that are not
  ;; functions.
  ;;
  ;; Also avoid corruption save-var copies, and ^Y since it requires $(10).
  ;;
  (define `dangerous-vars
    "[% ~trace ~untrace ~trace-ext ~untrace-ext ^Y ")

  ;; Find names matching pattern PAT, remove those that match "PAT:-" specs,
  ;; and remove non-function names.
  (define `(match-funcs pat)
    (declare .VARIABLES &global)
    (define `eligible-vars
      (filter-out (concat ignore-vars " " dangerous-vars " "
                          (filtersub "%:-" "%" specs))
                  .VARIABLES))

    (foreach v (trace-match pat eligible-vars)
             (if (filter "filerec%" (concat (origin v) (flavor v)))
                 v)))

  ;; Apply instrumentation to a function
  ;;
  (define `(instrument mode name id)
    ;; SCAM variables can contain `#` and `=`. `=` is ok on the RHS.
    (define `ename
      (subst "#" "$\"" name))

    ;; don't overwrite original if it has already been saved
    (if (filter "u%" (origin (save-var id)))
        (set-rglobal (save-var id) (value name)))

    (define `body
      (trace-body (or mode "t") ename id (value (save-var id))))
    (if (filter "%:v" specs)
        (trace-info "[" mode "] " name))
    (set-rglobal name body))

  ;; Choose automatic vars extremely unlikely to shadow `(value NAME)`
  (define `instrumented-names
    (foreach
        _-spec (filter-out "%:v %:-" specs)
        (foreach
            _-name (match-funcs (spec-name _-spec))
            (foreach id (trace-id _-name 1)
                     (instrument (spec-mode _-spec) _-name id)
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
    (filter names known-names))

  (define `untraced-names
    (foreach name matched-names
             (foreach id (trace-id name)
                      ;; restore original definition
                      (set-rglobal name (value (save-var id)))
                      name)))

  (trace-dump untraced-names))


(at-exit (lambda () (trace-dump known-names)))
