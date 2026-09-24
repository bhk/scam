;;----------------------------------------------------------------
;; runtime: runtime functions
;;----------------------------------------------------------------

;; When a SCAM source file is compiled, the generated code will contain
;; embedded references to functions and variables defined in the runtime
;; (this module).  The runtime must therefore be loaded before any SCAM
;; module can execute, and since it is implemented as SCAM source, we
;; must take care to avoid SCAM constructs that depend upon runtime
;; functions before those functions are defined.

;; Passed from the BASH preamble. See compile.scm.
(declare SCAM_MAIN &native)
(declare SCAM_ARGS &native)
(declare SCAM_DIR &native)


;; Some make distros (Ubuntu) ignore the environment's SHELL and set it to
;; /bin/sh.  We set it to bash rather than bothering to test the `io` module
;; with others.
;;
(define SHELL
  &native
  "/bin/bash")


(native-eval "define '


endef
 [ := (
 ] := )
\" := \\#
' := $'
` := $$
& := ,
$(if ,, ) :=")


;;--------------------------------------------------------------
;; Support for fundamental data types, and utility functions
;;--------------------------------------------------------------

(define `nil
  &public
  "")


(define `(not v)
  &public
  (if v nil "1"))


;; (^d string) => "down" = encode as word
;;
(define (^d str)
  &native
  (or (subst "!" "!1" "\t" "!+" " " "!0" str) "!."))

(define `(demote a)
  &public
  (^d a))


;; (up string) => recover string from word
;;
(define `(up str)
  (subst "!." "" "!0" " " "!+" "\t" "!1" "!" str))

(define (^u str)
  &native
  (up str))

(define `(promote a)
  &public
  (^u a))


;; (^n n vec) => Nth member of vector VEC
;;
(define (^n n vec)
  &native
  (up (word n vec)))

;; Vector operation exports

(define `(nth a b)
  &public
  (^n a b))

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
  (^u (subst "!8" "%" (word 1 (subst "!=" " " pair)))))

;; Get VALUE portion of a dictionary pair.
;;
(define (^dv pair)
  &native
  (^u (word 2 (subst "!=" " " pair))))

;; ^Y : invokes lambda expression
;;
;;  $(call ^Y,a,b,c,d,e,f,g,h,i,lambda) invokes LAMBDA.  A through H
;;  hold the first 8 arguments; I is a vector of remaining arguments.
;;
(declare (^Y ...args)
         &native)
(set ^Y "$(call if,,,$(10))")


;; ^v: return a vector of all arguments starting at argument $N (bound by an
;; enclosing `foreach`!), where N is 1..8.  The last element in the vector
;; is the last non-nil argument.
;;
;; This is defined as a function, but in order to work it must be referenced
;; as a variable so that the reference will compile to "$(VAR)" instead of
;; "$(call VAR)", which would clobber all the arguments.
;;
(define (^v)
  &native
  (define `maxarg
    (word 1 (._. (foreach (n "9 8 7 6 5 4 3 2 1")
                   (if (native-var n) n))
                 0)))

  (.. (foreach (n (wordlist (native-var "N") maxarg "1 2 3 4 5 6 7 8"))
        [(native-var n)])
      (if (native-var 9)
          (.. " " (native-var 9)))))


(define (^NA fname args ?a10)
  &native
  (native-call fname (nth 1 args) (nth 2 args) (nth 3 args) (nth 4 args)
               (nth 5 args) (nth 6 args) (nth 7 args) (nth 8 args)
               (nth-rest 9 args) a10))

(define `(name-apply n a)
  &public
  (^NA n a))

(define `(apply f a)
  &public
  (^NA "^Y" a f))


;;--------------------------------------------------------------
;; set-native, set-native-fn (^S, ^SF)
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
(define (^S name value ?retval)
  &native
  (.. (native-eval (.. (esc-LHS name) " :=$ " (esc-RHS value)))
      retval))

(define `(set-native a b ?c)
  &public
  (^S a b c))


;; Assign a new value to a recursive variable, and return RETVAL.
;; Note: ^F conflicts with a Make automatic variable
;;
(define (^SF name value retval)
  &native
  (define `qname (esc-LHS name))
  (define `qbody (subst "endef" "$ endef"
                         "define" "$ define"
                         "\\\n" "\\$ \n"
                         (.. value "\n")))

  (native-eval (.. "define " qname "\n" qbody "endef\n"))
  retval)

(define `(set-native-fn a b ?c)
  &public
  (^SF a b c))


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
  (define `(E exp)
    (.. "$" pre exp))

  (define `quoted
    (subst "$" (E "`")
           ")" (E "]")
           "(" (E "[")
           str))

  (subst "Q" quoted
         (if (or (findstring "," str)
                 (findstring " $ " (.. " $" str "$ ")))
             ;; preserve whitespace and/or contain commas
             (E "(if ,,Q)")
             "Q")))


;; (native-bound? VAR-NAME) -> 1 if variable VAR-NAME is defined
;;
(define `(native-bound? var-name)
  &public
  (if (filter-out "u%" (native-flavor var-name)) 1))


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
(declare *RM*)

;; overridden by trace.scm
(declare (trace-after-load id))

;; Load the module identified by ID.
;;
(define `(load id bound-only)
  (define `(mod-var id)
    (.. "[mod-" id "]"))

  ;; Encode file name for "include ..."
  (define `mod-file
    (subst " " "\\ " "\t" "\\\t"
           (.. SCAM_DIR id ".o")))

  (define `skipped
    (if (filter "r%" (native-flavor (mod-var id)))
        (native-eval (native-value (mod-var id)))
        (or bound-only
            (native-eval (.. "include " mod-file)))))

  (if skipped
      nil
      (begin
        (trace-after-load id)
        1)))


;; Execute a module if it hasn't been executed yet.
;;
(define (^R id ?bound-only)
  &native
  (or (filter [id] *RM*)
      (if (load id bound-only)
          (set *RM* (._. *RM* [id]))))
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


(declare *AE*)

;; Call FN immediately prior to program exit.  Note that FN is a function
;; value, not a function name.  FN will be added only if it is not in the
;; list of at-exit functions.
;;
(define (^AE fn)
  &native
  (set *AE* (._. (filter-out *AE* (native-var "^k")) *AE*)))


(define `(at-exit fn)
  &public
  (^AE fn))


;; run all at-exit functions
(define (^OE)
  &native
  (foreach (kfn *AE*)
    (define `fn (native-call "^dk" kfn))
    (native-call "if" nil nil fn))
  nil)

;;------------------------------------------------------------------------
;; Run program
;;------------------------------------------------------------------------

;; Loads the "main" module and call the "main" function.
(define `[main-mod main-func] SCAM_MAIN)

;; Load the trace module only if it is embedded in the current file, which
;; would be the case when we are running in interactive or immediate mode,
;; or when in a compiled program that has explicitly required "trace".  This
;; allows tracing of -q tests, except with `--boot`.
(^R "trace" 1)

(^R main-mod)

(define `exit-code
  (or (native-call main-func SCAM_ARGS) 0))

;; <exit> is defined last; rules defined in MAIN will supercede.
;; Run onExit if and when <exit> is processed.
(native-eval
 (.. ".PHONY: <exit>\n"
     "<exit>: ; @exit '" exit-code "'$(" (native-name ^OE) ")"))
