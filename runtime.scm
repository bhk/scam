;;----------------------------------------------------------------
;; runtime: runtime functions
;;----------------------------------------------------------------

;; When a SCAM source file is compiled, the generated code will contain
;; embedded references to the runtime.  The runtime must therefore be loaded
;; before any SCAM module can execute; in fact, the runtime itself initiates
;; execution of the program (see Program Execution, below).
;;
;; *This* file is itself compiled from SCAM source, so we have to take care
;; not to use SCAM constructs that depend upon runtime functions before
;; those functions are defined.

;; Variables not to be instrumented.
(define *do-not-trace*
  (value ".VARIABLES"))

(eval "define '


endef
 [ := (
 ] := )
\" := \\#
' := $'
` := $$
& := ,
")

;; Most runtime exports are declared as "&native" so that the code generation
;; phase does not have to take namespacing into account.

;; (^d string) => "down" = encode as word
;;
(define (^d str)
  &native
  (or (subst "!" "!1" "\t" "!+" " " "!0" str) "!."))

;; (^u string) => "up" = recover string from word
;;
(define (^u str)
  &native
  (subst "!." "" "!0" " " "!+" "\t" "!1" "!" str))

;; (^n n vec) => Nth member of vector VEC
;;
(define (^n n vec)
  &native
  (^u (word n vec)))

;; Encode dictionary key
;;
(define (^k str)
  &native
  (declare ^d &native)
  (subst "%" "!8" ^d))

;; ^Y : invokes lambda expression
;;
;;  $(call ^Y,a,b,c,d,e,f,g,h,i,lambda) invokes LAMBDA.  A through H
;;  hold the first 8 arguments; I is a vector of remaining arguments.
;;
(declare (^Y ...args)
         &native)
(set ^Y "$(call if,,,$(10))")


;; ^v: return a vector of all arguments starting at argument N, where N is
;; 1..8.  The last element in the vector is the last non-nil argument.
;;
;; ^av: return a vector of all arguments.
;;
;; These are declared as functions, but referenced elsewhere as variables so
;; that the reference will compile to "$(VAR)" instead of "$(call VAR)", in
;; order to retain $1, $2, etc..

(declare (^v)
         &native)

(set ^v (concat "$(subst !.,!. ,$(filter-out %!,$(subst !. ,!.,"
                "$(foreach n,$(wordlist $N,9,1 2 3 4 5 6 7 8),"
                "$(call ^d,$($n)))$(if $9, $9) !)))"))

(declare (^av)
         &native)

(set ^av "$(foreach N,1,$(^v))")

;; Call FN with elements of vector ARGV as arguments.

(declare (^apply fn argv))

(set ^apply (concat "$(call ^Y,$(call ^n,1,$2),$(call ^n,2,$2),$(call ^n,3,$2),"
                    "$(call ^n,4,$2),$(call ^n,5,$2),$(call ^n,6,$2),"
                    "$(call ^n,7,$2),$(call ^n,8,$2),$(wordlist 9,99999999,$2),$1)"))

;; Call function named NAME with elements of vector ARGV as arguments.
;;
(define (name-apply name argv)
  &public
  (define `call-expr
    (concat "$(call " name
            (subst " ," ","
                   (foreach n (wordlist 1 (words argv) "1 2 3 4 5 6 7 8")
                            (concat ",$(call ^n," n ",$2)")))
            (if (word 9 argv)
                (concat ",$(wordlist 9,99999999,$2)"))
            ")"))
  (call "if" "" "" call-expr))


;;--------------------------------------------------------------
;; debugging and diagnostics
;;--------------------------------------------------------------


;; Format a value as a quoted string.
;;
(define (^f a)
  &native
  (concat "\""
          (subst "\\" "\\\\" "\"" "\\\"" "\n" "\\n" a)
          "\""))

;; Display a value to stdout and return it.
;;
(define (^tp name value)
  &native
  (concat
   (info (concat name " " (^f value)))
   value))

;; ^tc : call function named by $1, and shift all other args left
;;
(declare (^tc fn ...args) &native)
(set ^tc (concat "$(call $1,$2,$3,$4,$5,$6,$7,$8,$(call ^n,1,$9),$(wordlist 2,9999,$9))"))


;; ^ta : format arguments for display
;;
(declare (^ta ...args) &native)

(set ^ta (concat "$(if $(or $1,$2,$3,$4,$5,$6,$7,$8,$9), $(^f)$(call ^tc,^ta,$2,$3,$4,$5,$6,$7,$8,$9))"))

;; ^t : trace function call with arguments and results.  Generated code will
;;      evaluate this as a variable -- `$(^t)` -- rather than via `call`.
;;
(declare (^t) &native)
(set ^t (concat "$(info --> ($1$(call ^tc,^ta,$2,$3,$4,$5,$6,$7,$8,$9)))$(call ^tp,<-- $1:,$(call ^tc,$1,$2,$3,$4,$5,$6,$7,$8,$9))"))


;;--------------------------------------------------------------
;; ^set and ^fset

(define `(esc-RHS str)
  (subst "$" "$$"
         "#" "$\""
         "\n" "$'" str))

(define (esc-LHS str)
  ;; $(if ,,...) protects ":", "=", *keywords*, and leading/trailing spaces
  (concat "$(if ,,"
          (subst "(" "$["
                 ")" "$]" (esc-RHS str))
          ")"))


;; Assign a new value to a simple variable, and return RETVAL.
;;
(define (^set name value ?retval)
  &native
  (concat (eval (concat (esc-LHS name)
                        " :=$ "
                        (esc-RHS value)))
          retval))

;; Assign a new value to a recursive variable, and return RETVAL.
;;
(define (^fset name value retval)
  &native
  (define `qname (esc-LHS name))
  (define `qbody (subst "endef" "$ endef"
                         "define" "$ define"
                         "\\\n" "\\$ \n"
                         (concat value "\n")))

  (eval (concat "define " qname "\n" qbody "endef\n"))
  retval)

(^set " " "")  ;; "$ " --> empty string


;; Escape a value for inclusion in a lambda expression.  Return a value
;; that, after N expansions (one or more), will yield STR, where N is
;; described by PRE: "" => one, "`" => two, "``" => three, and so on.
;;
;; Also, the escaped value and all expansions thereof (except for the very
;; last) must be safe for all argument contexts, so it must not contain
;; unbalanced parens, newlines, or commas (unless within balanced parens).
;;
;; Unlike protect-arg, which runs at compile time and is optimized for
;; small, simple output, ^E also tries to minimize encoding time.
;;
(define (^E str ?pre)
  &native
  (subst "$" (concat "$" pre)
         (concat
          "$(if ,,"
          (subst "$" "$`"
                 ")" "$]"
                 "(" "$["
                 "\n" "$'" str)
          ")")))

;;--------------------------------------------------------------
;; Support for fundamental data types

(define (apply a b) &public (^apply a b))
(define (promote a) &public (^u a))
(define (demote a)  &public (^d a))
(define (nth a b)   &public (^n a b))
(define (set-native a b ?c) &public (^set a b c))
(define (set-native-fn a b ?c) &public (^fset a b c))

(define `nil &public "")

(define `(not v)
  &public
  (if v nil "1"))

;; (nth-rest n vec) == vector starting at Nth item in VEC.
(define `(nth-rest n vec)
  &public
  (wordlist n 99999999 vec))

(define `(first vec)
  &public
  (^u (word 1 vec)))

(define `(rest vec)
  &public
  (nth-rest 2 vec))

(define `(rrest vec)
  &public
  (nth-rest 3 vec))


;; (bound? VAR) -> 1 if variable VAR is defined
;;
;; Note that VAR must be a string that names the variable, not
;; a quoted symbol: (bound? "map"), not (bound? 'map).
;;
(define `(bound? var)
  &public
  (if (filter-out "u%" (flavor var)) 1))


;;--------------------------------------------------------------
;; tags track record types for dynamic typing purposes

(define ^tags
  &native
  &public
  "")

(define (^add-tags str)
  &native
  (set ^tags (concat ^tags " " (filter-out ^tags str))))


;;--------------------------------------------------------------
;; ^require

(define *required* nil)

(define `(mod-var id)
  (concat "[mod-" id "]"))


;; This will be overridden by compiler modules.
;;
(declare (load-ext mod-id))


;; Load the module identified by ID.
;;
(define (^load id)
  &native
  (if (bound? (mod-var id))
      (eval (value (mod-var id)))
      (or (load-ext id)
          (error (concat "module " id " not found!"))))
  ;; return value is useful when viewing trace of load sequence
  id)


;; Execute a module if it hasn't been executed yet.
;;
(define (^require id)
  &native
  (or (filter id *required*)
      (begin
        (set *required* (concat *required* " " id))
        (^load id)))
  nil)


;;----------------------------------------------------------------
;; Tracing: tracing and debugging facilities
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
      (set-native cv (or (value cv) zero))
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
    (foreach p "^% ~% ~trace% ~esc-% ~set-native-fn"
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
(define (trace specs)
  &public

  ;; Break SPEC into name and mode...
  (define `(spec-name spec)
    (filter-out ":%" (subst ":" " :" spec)))
  (define `(spec-mode spec)
    (subst " " "" (wordlist 2 999 (subst ":" ": " spec))))

  ;; Avoid modifying any function while it is currently being expanded,
  ;; which could trigger a GNU Make user-after-free bug.  In general we do
  ;; not know what's being expanded, and users have to take care, but we can
  ;; exclude what we know is problematic.  The *do-not-trace* variable names
  ;; some file-recursive variables that are not functions.
  ;;
  ;; Also avoid corruption save-var copies, and ^Y since it requires $(10).
  ;;
  (define `dangerous-vars
    "[% ~trace ~untrace ~trace-ext ~untrace-ext ^Y ")

  ;; Find names matching pattern PAT, remove those that match "PAT:-" specs,
  ;; and remove non-function names.
  (define `(match-funcs pat)
    (declare .VARIABLES &native)
    (define `eligible-vars
      (filter-out (concat *do-not-trace* " " dangerous-vars " "
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
        (set-native-fn (save-var id) (value name)))

    (define `body
      (trace-body (or mode "t") ename id (value (save-var id))))
    (if (filter "%:v" specs)
        (trace-info "[" mode "] " name))
    (set-native-fn name body))

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
                            (set-native (count-var (trace-id name)) zero)
                            [(concat (subst ":" " " (trace-digits k)) " " name)])))))

  (for line (sort lines)
           (trace-info line)))


;; Remove instrumentation from functions listed in NAMES, or functions
;; matched by patterns in NAMES.
;;
(define (untrace names ?retval)
  &public
  (define `matched-names
    (filter names known-names))

  (define `untraced-names
    (foreach name matched-names
             (foreach id (trace-id name)
                      ;; restore original definition
                      (set-native-fn name (value (save-var id)))
                      name)))

  (trace-dump untraced-names)
  retval)



;; Add vars to the list of variables not to trace.
(define (do-not-trace vars)
  &public
  (set *do-not-trace* (concat *do-not-trace* " " vars)))

;; Activate tracing *only* during evaluation of EXPR.
;;
(define `(tracing spec expr)
  &public
  (untrace (trace spec) expr))


(define (start-trace main-mod)
  ;; Activate tracing if [_]SCAM_TRACE is set
  (define `env-prefix (if (filter "scam" main-mod) "_"))
  (trace (value (concat env-prefix "SCAM_TRACE"))))


;;----------------------------------------------------------------
;; Program execution


(define *atexits* nil)


;; prepend new function so they are run in reverse order
(define (at-exit fn)
  &public
  (set *atexits* (concat [fn] " " *atexits*)))


(define (run-at-exits)
  (for fn *atexits*
       (fn))
  nil)


;; Validate what was returned from main before it is passed to the bash
;; `exit` builtin in the [exit] command.  Return nil if it isn't valid.
;;
(define (check-exit code)
  (define `(non-integer? n)
    (subst "1" "" "2" "" "3" "" "4" "" "5" "" "6" "" "7" "" "8" "" "9" "" "0" ""
           (patsubst "-%" "%" (subst " " "x" "\t" "x" code))))

  (if (non-integer? code)
      (error (concat "scam: main returned '" code "'"))
      (or code 0)))


(define (^start main-mod main-func args)
  &native

  ;; Allow module loading to be traced.
  (start-trace main-mod)
  ;; Now it's dangerous...
  (do-not-trace (concat "^require ^load " (native-name load-ext)))

  (^require main-mod)
  (start-trace main-mod)

  ;; Run main, *then* read .DEFAULT_GOAL, *then* re-assign it.  We call
  ;; main's value as an anonymous function so tracing it will not cause
  ;; problems (redefining a function while it's being expanded).
  (define `rules
    (let ((exit-arg (check-exit ((value main-func) args))))
      ;; Read .DEFAULT_GOAL *after* running main.  Ensure [exit] will run
      ;; last.  There we run exit hooks and deliver main's exit code.
      (concat ".DEFAULT_GOAL :=\n"
              ".PHONY: [exit]\n"
              "[exit]: " .DEFAULT_GOAL ";"
              "@exit " exit-arg (lambda () (run-at-exits)))))

  (eval rules))

;; these will be on the stack
(do-not-trace (concat (native-name ^start) " " (native-name start-trace)))
(at-exit (lambda () (trace-dump known-names)))
