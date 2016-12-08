;; runtime: runtime functions
;;
;; When a SCAM source file is compiled, the generated code will contain
;; embedded references to the runtime.  The runtime must therefore be loaded
;; before any compile SCAM module can execute.  It is the runtime itself that
;; intiates execution of the program (see Program Execution, below).
;;
;; *This* file is itself compiled from SCAM source, so we have to take care
;; not to use SCAM constructs that depend upon runtime functions before
;; those functions are defined.


(declare SCAM_MODS &global)
(declare SCAM_DEBUG &global)
(eval "SCAM_DEBUG ?=")


(if (findstring "R" SCAM_DEBUG)
    (print "runtime: " (lastword MAKEFILE_LIST)))

(eval "define \\n


endef
 [ := (
 ] := )
\" := \\#
' := $(\\n)
` := $$
& := ,
")

;; Most runtime exports are declared as "global" so that the code generation
;; phase does not have to take namespacing into account.  This should not
;; cause problems with self-hosting unless we want to change the contracts
;; or names of these very basic operations.

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

;; (^n n vec) => nth member of vector `vec`
;;
(define (^n n vec)
  &global
  (^u (word n vec)))


;; ^Y : invokes lambda expression
;;
;;  $(call ^Y,a,b,c,d,e,f,g,h,i,lambda) invokes `lambda`. `a` through `h`
;;  hold the first 8 arguments; `i` is a vector of remaining args.
;;
(declare (^Y ...)
         &global)
(set ^Y "$(call if,,,$(10))")


;; ^av: return a vector of all arguments.  The vector length is the index of
;; the last non-nil argument
;;
;; We declare *args* as a SCAM variable (not function), but we assign it as a
;; recursive variable (not simply-expanded).  References to it will
;; therefore compile to $(*args*), so it will be expanded (executed) without
;; overriding $1, $2, etc.
;;
(declare (^av)
         &global)

(set ^av (concat "$(subst !.,!. ,$(filter-out %!,$(subst !. ,!.,"
                 "$(foreach n,1 2 3 4 5 6 7 8,$(call ^d,$($n)))$(if $9, $9) !)))"))

;; ^apply

(declare (^apply fn ...))

(set ^apply (concat "$(call ^Y,$(call ^n,1,$2),$(call ^n,2,$2),$(call ^n,3,$2),"
                    "$(call ^n,4,$2),$(call ^n,5,$2),$(call ^n,6,$2),"
                    "$(call ^n,7,$2),$(call ^n,8,$2),$(wordlist 9,9999,$2),$1)"))


;;--------------------------------------------------------------
;; debugging and diagnostics
;;--------------------------------------------------------------


;; Format a value as a quoted string.
;;
(define (^f a)
  (concat "\""
          (subst "\\" "\\\\" "\"" "\\\"" a)
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
(declare (^tc fn ...))
(set ^tc (concat "$(call $1,$2,$3,$4,$5,$6,$7,$8,$(call ^n,1,$9),$(wordlist 2,9999,$9))"))


;; ^ta : format arguments for display  [also used by trace.scm]
;;
(declare (^ta ...) &global)

(define `TC (global-name ^tc))
(define `F (global-name ^f))
(set ^ta (concat "$(if $(or $1,$2,$3,$4,$5,$6,$7,$8,$9), $(" F ")$(call " TC ",^ta,$2,$3,$4,$5,$6,$7,$8,$9))"))

;; ^t : trace function call with arguments and results.  Generated code will
;;      evaluate this as a variable -- `$(^t)` -- rather than via `call`.
;;
(declare (^t) &global)
(set ^t (concat "$(info --> ($1$(call " TC ",^ta,$2,$3,$4,$5,$6,$7,$8,$9)))$(call ^tp,<-- $1:,$(call " TC ",$1,$2,$3,$4,$5,$6,$7,$8,$9))"))


;;--------------------------------------------------------------
;; ^set and ^fset

(define `(esc-RHS str)
  &private
  (subst "$" "$$"
         "#" "$\""
         "\n" "$'" str))

(define (esc-LHS str)
  ;; $(if ,,...) protects ":", "=", *keywords*, and leading/trailing spaces
  (concat "$(if ,,"
          (subst "(" "$["
                 ")" "$]" (esc-RHS str))
          ")"))


;; Assign a new value to a simple variable, and return `retval`.
;;
(define (^set name value retval)
  &global
  (concat (eval (concat (esc-LHS name)
                        " :=$ "
                        (esc-RHS value)))
          retval))

;; Assign a new value to a recursive variable, and return `retval`.
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
(define (^E str pre)
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

(define (promote a) (^u a))
(define (demote a)  (^d a))
(define (nth a b)   (^n a b))
(define (set-global a b c) (^set a b c))
(define (set-rglobal a b c) (^fset a b c))
(define (apply a b) (^apply a b))

(define `nil "")

(define `(not v)
  (if v nil "1"))

;; (nth-rest n vec) == vector starting at `n`th item in `vec`
(define `(nth-rest n vec)
  (wordlist n 99999999 vec))

(define `(first vec)
  (^u (word 1 vec)))

(define `(rest vec)
  (nth-rest 2 vec))

(define `(rrest vec)
  (nth-rest 3 vec))


;; (bound? VAR) -> 1 if variable VAR is defined
;;
;; Note that VAR must be a string that names the variable, not
;; a quoted symbol: (bound? "map"), not (bound? 'map).
;;
(define `(bound? var)
  (if (filter-out "u%" (flavor var)) 1))


;; word list of "event=func_name" entries
(define *hooks* nil)

(define (add-hook event funcname)
  (set *hooks* (concat *hooks* " " event "=" funcname)))

(define (run-hooks event)
  (define `pat (concat event "=%"))
  (foreach funcname (patsubst pat "%" (filter pat *hooks*))
           (call funcname)))

(define ^tags
  &global
  "")

(define (^add-tags str)
  &global
  (set ^tags (concat ^tags " " (filter-out ^tags str))))


;;--------------------------------------------------------------
;; ^require


(define ^required-files "///runtime.min")


;; Include a module if it hasn't been included yet.
;;
;; Note: `let-global` introduces a hidden dependency on `^set`.
;;
(define (^require name)
  &global

  (define `mod (notdir name))
  (define `bundle (concat "///" mod ".min"))

  ;; Load and execute `path`
  (define `(load path)
    (if (filter "///%" path)
        (eval (value path))
        (eval (concat "include " path))))

  ;; If a matching file is listed in SCAM_MODS, use that file
  (define `named-mod
    (word 1 (foreach f SCAM_MODS
                     (if (filter mod (notdir (basename f)))
                         f))))

  (define `new-file
    (filter-out ^required-files
                (or named-mod
                   (if (bound? bundle)
                       bundle
                       (concat name ".min")))))

  ;; Using `foreach` as a cheap binding mechanism...
  (foreach
      ^file new-file
      (begin
        (set ^required-files (concat ^required-files " " ^file))

        (if (findstring "R" SCAM_DEBUG)
            (info (concat "require: " ^file)))
        (load ^file)
        (run-hooks "load")
        (if (findstring "Rx" SCAM_DEBUG)
            (info (concat "exited: " ^file))))))


;;----------------------------------------------------------------
;; Program execution

(declare *started* &global)


(define (start main-mod main-func args)
  (if (not *started*)
      ;; Avoid recursive of repeated calls to start, as when:
      ;;   - `runtime` is required explicitly
      ;;   - SCAM_MAIN runs a test and then ^start is called
      (begin
        (set *started* 1)

        (if (bound? "///trace.min")
            (^require "trace"))

        (^require (notdir main-mod))

        (let ((exit-code (call main-func args)))
          ;; If `main` returns a bogus value then the "exit" command will display
          ;; an acceptable warning.
          (define `exit-arg
            (concat "'" (or (subst "'" "" (strip exit-code)) 0) "'"))

          ;; Run exit hooks and return exit code after rule processing phase.
          ;; If `main` has generated a rule, build it before exiting.
          (eval (concat ".DEFAULT_GOAL :=\n"
                        ".PHONY: .scam/-exit\n"
                        ".scam/-exit: " .DEFAULT_GOAL "; @exit " exit-arg
                        (lambda () (run-hooks "exit")) ))))))


;; &global functions can cause problems if redefined by a new runtime *while
;; running*.

(declare (^start main-mod main-func args)
  &global)

(if (not (bound? "^start"))
    (set ^start start))


;; If SCAM_MAIN is set, load that module and short-circuit ordinary program
;; startup.  This allows executing modules in the following manner:
;;
;;    make -f runtime.min SCAM_MAIN=<module>
;;    make -f bin/scam SCAM_MAIN=<module>
;;
(declare SCAM_MAIN &global)
(if SCAM_MAIN
    (start SCAM_MAIN nil nil))
