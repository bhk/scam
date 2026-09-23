;;----------------------------------------------------------------
;; runtime: runtime functions
;;----------------------------------------------------------------

;; When a SCAM source file is compiled, the generated code will contain
;; embedded references to functions and variables defined in the runtime
;; (this module).  The runtime must therefore be loaded before any SCAM
;; module can execute, and since it is implemented as SCAM source, we
;; must take care to avoid SCAM constructs that depend upon runtime
;; functions before those functions are defined.

;; passed from the BASH preamble...
(declare SCAM_MAIN &native)
(declare SCAM_ARGS &native)

(native-eval "define '


endef
 [ := (
 ] := )
\" := \\#
' := $'
` := $$
& := ,
$(if ,, ) :=
")


;; (^d string) => "down" = encode as word
;;
(define (^d str)
  &native
  (or (subst "!" "!1" "\t" "!+" " " "!0" str) "!."))

;; (up string) => recover string from word
;;
(define `(up str)
  (subst "!." "" "!0" " " "!+" "\t" "!1" "!" str))

(define (^u str)
  &native
  (up str))

;; (^n n vec) => Nth member of vector VEC
;;
(define (^n n vec)
  &native
  (up (word n vec)))

;; Encode dictionary key
;;
(define (^k str)
  &native
  (declare ^d &native)
  (subst "%" "!8" ^d))

;; Get KEY portion of a dictionary pair.
;;
(define (^dk pair)
  &native
  (up (subst "!8" "%" (word 1 (subst "!=" " " pair)))))

;; Get VALUE portion of a dictionary pair.
;;
(define (^dv pair)
  &native
  (up (word 2 (subst "!=" " " pair))))

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

(set ^v (.. "$(subst !.,!. ,$(filter-out %!,$(subst !. ,!.,"
            "$(foreach n,$(wordlist $N,9,1 2 3 4 5 6 7 8),"
            "$(call ^d,$($n)))$(if $9, $9) !)))"))

(declare (^av)
         &native)

(set ^av "$(foreach N,1,$(^v))")

;; Call FN with elements of vector ARGV as arguments.

(declare (^apply fn argv) &native)

(set ^apply (.. "$(call ^Y,$(call ^n,1,$2),$(call ^n,2,$2),$(call ^n,3,$2),"
                "$(call ^n,4,$2),$(call ^n,5,$2),$(call ^n,6,$2),"
                "$(call ^n,7,$2),$(call ^n,8,$2),$(wordlist 9,99999999,$2),$1)"))

;; Call function named NAME with elements of vector ARGV as arguments.
;;
(define (^na name argv)
  &native
  (define `call-expr
    (.. "$(call " name
        (subst " ," ","
               (foreach (n (wordlist 1 (words argv) "1 2 3 4 5 6 7 8"))
                 (.. ",$(call ^n," n ",$2)")))
        (if (word 9 argv)
            (.. ",$(wordlist 9,99999999,$2)"))
        ")"))
  (native-call "if" "" "" call-expr))


;;--------------------------------------------------------------
;; ^set and ^fset
;;--------------------------------------------------------------

(define `(esc-RHS str)
  (subst "$" "$$"
         "#" "$\""
         "\n" "$'" str))

(define (esc-LHS str)
  ;; $(if ,,...) protects ":", "=", *keywords*, and leading/trailing spaces
  (.. "$(if ,,"
      (subst "(" "$["
             ")" "$]" (esc-RHS str))
      ")"))


;; Assign a new value to a simple variable, and return RETVAL.
;;
(define (^set name value ?retval)
  &native
  (.. (native-eval (.. (esc-LHS name) " :=$ " (esc-RHS value)))
      retval))

;; Assign a new value to a recursive variable, and return RETVAL.
;;
(define (^fset name value retval)
  &native
  (define `qname (esc-LHS name))
  (define `qbody (subst "endef" "$ endef"
                         "define" "$ define"
                         "\\\n" "\\$ \n"
                         (.. value "\n")))

  (native-eval (.. "define " qname "\n" qbody "endef\n"))
  retval)


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
  (define `(E exp) (.. "$" pre exp))

  (if (or (findstring "," str)
          (findstring " $ " (.. " $" str "$ ")))
      ;; protect commas and/or whitespace
      (.. (E "(if ,,")
          (subst "$" (E "`")
                 ")" (E "]")
                 "(" (E "[")
                 str)
          ")")
      ;; no commas and no leading/trailing whitespace
      (subst "$" (E "`")
             ")" (E "]")
             "(" (E "[")
             str)))


;;--------------------------------------------------------------
;; Support for fundamental data types, and utility functions
;;--------------------------------------------------------------

(define `(name-apply a b) &public (^na a b))
(define `(apply a b) &public (^apply a b))
(define `(promote a) &public (^u a))
(define `(demote a)  &public (^d a))
(define `(nth a b)   &public (^n a b))
(define `(set-native a b ?c) &public (^set a b c))
(define `(set-native-fn a b ?c) &public (^fset a b c))

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
  (^n 1 vec))

(define `(rest vec)
  &public
  (nth-rest 2 vec))

(define `(rrest vec)
  &public
  (nth-rest 3 vec))


;; (native-bound? VAR-NAME) -> 1 if variable VAR-NAME is defined
;;
(define `(native-bound? var-name)
  &public
  (if (filter-out "u%" (native-flavor var-name)) 1))


;; Replace PAT with REPL if STR matches PAT; return nil otherwise.
;;
(define (filtersub pat repl str)
  &public
  (patsubst pat repl (filter pat str)))


;;--------------------------------------------------------------
;; tags track record types for dynamic typing purposes
;;--------------------------------------------------------------

(define ^tags
  &native
  &public
  "")

;; Add items to ^tags.
;;
(define (^at str)
  &native
  (set ^tags (._. ^tags (filter-out ^tags str))))


;;--------------------------------------------------------------
;; ^R : runtime require
;;--------------------------------------------------------------

;; A list of modules that have been loaded
(define *required* nil)

;; overridden by trace.scm
(define (trace-after-load id) nil)

;; Load the module identified by ID.
;;
(define (load id ?bound-only)
  &native
  (define `(mod-var id)
    (.. "[mod-" id "]"))

  (define `mod-file
    ;; Encode for "include ..."
    (subst " " "\\ " "\t" "\\\t"
           (.. (native-value "SCAM_DIR") id ".o")))

  (if (native-bound? (mod-var id))
      (native-eval (native-value (mod-var id)))
     (if bound-only
          nil
          (native-eval (.. "include " mod-file))))
  (trace-after-load id))


;; Execute a module if it hasn't been executed yet.
;;
(define (^R id)
  &native
  (or (filter [id] *required*)
      (begin
        (set *required* (._. *required* [id]))
        (load id)))
  nil)


;; Display function/args on entry to traced call
(define (E? fname ...)
  (print "--> (" fname "...)"))


;; Display function and return value on exit from traced call
(define (R? fname value)
  (.. (print "<-- " fname ": " value)
      value))


;; Call site tracing (minimal version independent of trace module)
(declare (^t fn ...args) &native)
(set ^t (.. "$(" (native-name E?) ")"
            "$(call " (native-name R?) ",$1,$(call $1,$2,$3,$4,$5,$6,$7,$8,"
            "$(call ^n,1,$9),$(wordlist 2,9999,$9)))"))


;; Some make distros (Ubuntu) ignore the environment's SHELL and set it to
;; /bin/sh.  We set it to bash rather than bothering to test the `io` module
;; with others.
;;
(define SHELL
  &native
  "/bin/bash")


(define *at-exits* nil)


;; Call FN immediately prior to program exit.  Note that FN is a function
;; value, not a function name.  When UNIQUE is set, FN will be added only
;; if it is note in the list of at-exit functions.
;;
(define (at-exit fn ?unique)
  &public
  (if (and unique (findstring (.. " " [fn] " ") (.. " " *at-exits* " ")))
      nil
      (set *at-exits* (._. [fn] *at-exits*))))


(define (on-exit)
  (for (fn *at-exits*)
    (fn))
  nil)


;; Validate what was returned from main before it is passed to the bash
;; `exit` builtin in the <exit> rule.
;;
(define (check-exit code)
  (define `(non-integer? n)
    (subst "1" "" "2" "" "3" "" "4" "" "5" "" "6" "" "7" "" "8" "" "9" "" "0" ""
           (patsubst "-%" "%" (subst " " "x" "\t" "x" code))))

  (if (non-integer? code)
      (error (.. "scam: main returned '" code "'"))
      (or code 0)))


;;------------------------------------------------------------------------
;; Run program
;;------------------------------------------------------------------------

;; Loads the "main" module and call the "main" function.
(define `[main-mod main-func] SCAM_MAIN)

;; Load the trace module only if it is embedded in the current file, which
;; would be the case when we are running in interactive or immediate mode,
;; or when in a compiled program that has explicitly required "trace".  This
;; allows tracing of -q tests, except with `--boot`.
(load "trace" 1)

(^R main-mod)

(define `exit-code
  (check-exit (native-call main-func SCAM_ARGS)))

;; <exit> is defined last; rules defined in MAIN will supercede.
;; Run onExit if and when <exit> is processed.
(native-eval
 (.. ".PHONY: <exit>\n"
     "<exit>: ; @exit " exit-code "$(" (native-name on-exit) ")"))
