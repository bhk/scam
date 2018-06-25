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

;; Most runtime exports are declared as "global" so that the code generation
;; phase does not have to take namespacing into account.

;; (^d string) => "down" = encode as word
;;
(define (^d str)
  &global
  (or (subst "!" "!1" "\t" "!+" " " "!0" str) "!."))

;; (^u string) => "up" = recover string from word
;;
(define (^u str)
  &global
  (subst "!." "" "!0" " " "!+" "\t" "!1" "!" str))

;; (^n n vec) => Nth member of vector VEC
;;
(define (^n n vec)
  &global
  (^u (word n vec)))

;; Encode dictionary key
;;
(define (^k str)
  &global
  (declare ^d &global)
  (subst "%" "!8" ^d))

;; ^Y : invokes lambda expression
;;
;;  $(call ^Y,a,b,c,d,e,f,g,h,i,lambda) invokes LAMBDA.  A through H
;;  hold the first 8 arguments; I is a vector of remaining arguments.
;;
(declare (^Y ...args)
         &global)
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
         &global)

(set ^v (concat "$(subst !.,!. ,$(filter-out %!,$(subst !. ,!.,"
                "$(foreach n,$(wordlist $N,9,1 2 3 4 5 6 7 8),"
                "$(call ^d,$($n)))$(if $9, $9) !)))"))

(declare (^av)
         &global)

(set ^av "$(foreach N,1,$(^v))")

;; ^apply

(declare (^apply fn ...args))

(set ^apply (concat "$(call ^Y,$(call ^n,1,$2),$(call ^n,2,$2),$(call ^n,3,$2),"
                    "$(call ^n,4,$2),$(call ^n,5,$2),$(call ^n,6,$2),"
                    "$(call ^n,7,$2),$(call ^n,8,$2),$(wordlist 9,9999,$2),$1)"))


;;--------------------------------------------------------------
;; debugging and diagnostics
;;--------------------------------------------------------------


;; Format a value as a quoted string.
;;
(define (^f a)
  &global
  (concat "\""
          (subst "\\" "\\\\" "\"" "\\\"" "\n" "\\n" a)
          "\""))

;; Display a value to stdout and return it.  [also used by trace.scm]
;;
(define (^tp name value)
  &global
  (concat
   (info (concat name " " (^f value)))
   value))

;; ^tc : call function named by $1, and shift all other args left
;;
(declare (^tc fn ...args) &global)
(set ^tc (concat "$(call $1,$2,$3,$4,$5,$6,$7,$8,$(call ^n,1,$9),$(wordlist 2,9999,$9))"))


;; ^ta : format arguments for display  [also used by trace.scm]
;;
(declare (^ta ...args) &global)

(set ^ta (concat "$(if $(or $1,$2,$3,$4,$5,$6,$7,$8,$9), $(^f)$(call ^tc,^ta,$2,$3,$4,$5,$6,$7,$8,$9))"))

;; ^t : trace function call with arguments and results.  Generated code will
;;      evaluate this as a variable -- `$(^t)` -- rather than via `call`.
;;
(declare (^t) &global)
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
  &global
  (concat (eval (concat (esc-LHS name)
                        " :=$ "
                        (esc-RHS value)))
          retval))

;; Assign a new value to a recursive variable, and return RETVAL.
;;
(define (^fset name value retval)
  &global
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
  &global
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
(define (set-global a b ?c) &public (^set a b c))
(define (set-rglobal a b ?c) &public (^fset a b c))

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
  &global
  &public
  "")

(define (^add-tags str)
  &global
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
  &global
  (or (load-ext id)
      (if (bound? (mod-var id))
          (eval (value (mod-var id)))
          (error (concat "module " id " not found!"))))
  ;; return value is useful when viewing trace of load sequence
  id)


;; Execute a module if it hasn't been executed yet.
;;
(define (^require id)
  &global
  (or (filter id *required*)
      (begin
        (set *required* (concat *required* " " id))
        (^load id)))
  nil)


;;----------------------------------------------------------------
;; Tracing
;;
;; The trace module is effectively part of the runtime, but it is included
;; in a separate file in order to allow programs to omit it (to save about
;; 2.5KB) (Sometimes I don't know why I do the things I do).


;; Add vars to the list of variables not to trace.
(define (do-not-trace vars)
  &public
  (set *do-not-trace* (concat *do-not-trace* " " vars)))

;; externalized depenencies
(declare (trace-ext specs *do-not-trace*))
(declare (untrace-ext names))

(define (trace specs)
  &public
  (if specs
      (begin
        ;; Use ^require, not require, to avoid treating it as a dependency.
        (^require "'trace")
        (trace-ext specs *do-not-trace*))))

(define (untrace names ?retval)
  &public
  (if names
      (begin
        (^require "'trace")
        (untrace-ext names)))
  retval)

;; Activate tracing *only* during evaluation of EXPR.
;;
(define `(tracing spec expr)
  &public
  (untrace (trace spec) expr))


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


(define (start-trace main-mod)
  ;; Activate tracing if [_]SCAM_TRACE is set
  (define `env-prefix (if (filter "'%" main-mod) "_"))
  (trace (value (concat env-prefix "SCAM_TRACE"))))


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
  &global

  ;; Allow module loading to be traced.
  (start-trace main-mod)
  ;; Now it's dangerous...
  (do-not-trace (concat "^require ^load " (global-name load-ext)))

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
(do-not-trace (concat (global-name ^start) " " (global-name start-trace)))
