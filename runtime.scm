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

(declare SCAM_ARGS)
(declare SCAM_DEBUG)
(declare SCAM_MAIN)
(declare SCAM_MODS)
(eval "SCAM_DEBUG ?=")

(define ^required-files "")

(if (findstring "R" SCAM_DEBUG)
    (print "runtime: " (lastword MAKEFILE_LIST)))

(eval "define \\n


endef")

(define \H "#")
(define \L "(")
(define \R ")")


;; (^d string) => "down" = encode as word
;;
(define (^d str)
  (or (subst "!" "!1" "\t" "!+" " " "!0" str) "!."))

;; (^u string) => "up" = recover string from word
;;
(define (^u str)
  (subst "!." "" "!0" " " "!+" "\t" "!1" "!" str))

;; (^n n vec) => nth member of vector `vec`
;;
(define (^n n vec)
  (^u (word n vec)))


;; ^Y : invokes lambda expression
;;
;;  $(call ^Y,a,b,c,d,e,f,g,h,i,lambda) invokes `lambda`. `a` through `h`
;;  hold the first 8 arguments; `i` is a vector of remaining args.
;;
(eval "^Y = $(call if,,,$(10))")


;; ^av: return a vector of all arguments.  The vector length is the index of
;; the last non-nil argument
;;
;; We declare *args* as a SCAM variable (not function), but we assign it as a
;; recursive variable (not simply-expanded).  References to it will
;; therefore compile to $(*args*), so it will be expanded (executed) without
;; overriding $1, $2, etc.
;;
(eval
 (concat "^av = $(subst !.,!. ,$(filter-out %!,$(subst !. ,!.,$(foreach n,1 2 3 4 5 6 7 8,$(call ^d,$($n)))$(if $9, $9) !)))"))

;; ^apply

(eval "^apply = $(call ^Y,$(call ^n,1,$2),$(call ^n,2,$2),$(call ^n,3,$2),$(call ^n,4,$2),$(call ^n,5,$2),$(call ^n,6,$2),$(call ^n,7,$2),$(call ^n,8,$2),$(wordlist 9,9999,$2),$1)")


;;--------------------------------------------------------------
;; debugging and diagnostics
;;--------------------------------------------------------------


;; Format a value as a quoted string.
;;
(define (^f a)
  (concat "\""
          (subst "\\" "\\\\" "\"" "\\\"" a)
          "\""))

;; Display a value to stdout and return it.
;;
(define (^tp name value)
  (concat
   (info (concat name " " (^f value)))
   value))

;; ^t : trace function call with arguments and results
;; ^tc : call function named by $1, and shift all other args left
;; ^ta : format arguments
;;
(eval
"^tc = $(call $1,$2,$3,$4,$5,$6,$7,$8,$(call ^n,1,$9),$(wordlist 2,9999,$9))
^ta = $(if $(or $1,$2,$3,$4,$5,$6,$7,$8,$9), $(^f)$(call ^tc,^ta,$2,$3,$4,$5,$6,$7,$8,$9))
^t = $(info --> ($1$(call ^tc,^ta,$2,$3,$4,$5,$6,$7,$8,$9)))$(call ^tp,<-- $1:,$(call ^tc,$1,$2,$3,$4,$5,$6,$7,$8,$9))")


;;--------------------------------------------------------------
;; ^set and ^fset

(define `(^set-RHS str)
  (subst "$" "$$" "#" "$(\\H)" "\n" "$(\\n)" str))

(define `(^set-LHS-x str)
  (subst "$" "$."")" "$R""(" "$(\\L)""$R" "$(\\R)""$." "$$""#" "$(\\H)"
         "\n" "$(\\n)"
         str))

(define (^set-LHS str)
    ;; $(if ,,...) protects ":", "=", *keywords*, and leading/trailing spaces
    (concat
     "$(if ,,"
     ;; avoid escaping when possible (most likely)
     (if (concat (findstring "\n" str)
                 (findstring "#" str)
                 (findstring "$" str)
                 (findstring "(" str)
                 (findstring ")" str))
         (^set-LHS-x str)
         str)
    ")"))


;; Assign a new value to a simple variable, and return `retval`.
;;
(define (^set name value retval)
  (concat (eval (concat (^set-LHS name)
                        " :=$ "
                        (^set-RHS value)))
          retval))

;; Assign a new value to a recursive variable, and return `retval`.
;;
(define (^fset name value retval)
  (define `qname (^set-LHS name))
  (define `qbody (subst "endef" "$ endef"
                         "define" "$ define"
                         "\\\n" "\\$ \n"
                         (concat value "\n")))

  (eval (concat "define " qname "\n" qbody "endef\n"))
  retval)

(^set " " "")  ;; "$ " --> empty string


;;--------------------------------------------------------------
;; lightweight, run-time escape & protect-arg (see escape.scm)


;; wrap s if it contains a ","
;;
(define (^es s)
  (if (findstring "," s)
    (concat "$(if ,," s ")")
    s))


;; Return a string of `count` dollar-signs
;;
(define (^ed n w)
  (if (filter n (words w))
      (subst " " "" w)
    (^ed n (concat w " $"))))


;; Escape a value for inclusion in a lambda expression.  `s` is the string
;; to escape, `n` describes the number of levels of escaping needed (the
;; number of rounds of evaluation it will undergo).  nil => 1.
;;
(define (^e s n)
  (subst "$" (if n (^ed n) "$")
    (^es (subst "$" "$1" ")" "$2\\R)" "(" "$(\\L)" "\n" "$(\\n)" "$2" "$("
                "$1" "$$" s))))


;;--------------------------------------------------------------
;; ^require


;; When a required file is executing, *file* holds the name of that file.
(define *file* "")


;; Include a module if it hasn't been included yet.
;;
;; Note: `let-global` introduces a hidden dependency on `^set`.
;;
(define (^require name)
  (define `bundle (concat "///" (notdir name) ".min"))
  (define `file (concat (dir SCAM_MAIN) (notdir name) ".min"))

  ;; True if `var` is bound
  (define `(bound? var)
    (filter-out "u%" (flavor var)))

  ;; Load and execute `path`
  (define `(load path)
    (if (filter "///%" path)
        (eval (value path))
        (eval (concat "include " path))))

  ;; If a matching file is listed in SCAM_MODS, use that file
  (define `named-mod
    (word 1 (foreach f SCAM_MODS
                     (if (filter (notdir name) (notdir (basename f)))
                         f))))

  (let-global
   ((*file* (or named-mod
                (if (bound? bundle)
                    bundle
                    file))))

   (if (filter *file* ^required-files)
       ""
       (begin
         (set ^required-files (concat ^required-files " " *file*))
         (if (findstring "R" SCAM_DEBUG)
             (info (concat "require: " *file*)))
         (load *file*)
         (if (findstring "Rx" SCAM_DEBUG)
             (info (concat "exited: " *file*)))))))

;;----------------------------------------------------------------
;; Program execution
;;
;; After defining variables that may be used by SCAM modules, the runtime
;; proceeds to execute the "main" module of the program.  Executing a
;; SCAM module is done by:
;;
;;    make -f runtime.min SCAM_MAIN=<module>
;;
;; After the SCAM_MAIN module executes, and if no targets have been defined,
;; the runtime defines a default target to avoid Make's "no targets" error.

(declare (main))
(declare SCAM_TRACE)
(declare (trace spec warn))
(declare (trace-dump))

(if SCAM_MAIN
    (begin
      ;; Load `trace` after runtime has initialized and before SCAM_MAIN is loaded.
      (if SCAM_TRACE
        (^require "trace"))
      (^require (notdir SCAM_MAIN))
      (if SCAM_TRACE
          (trace SCAM_TRACE))

      ;; `main` may return a closure.  This allows it to include other
      ;; modules that redefine main (a GNU Make can manifest if you override
      ;; a variable name while it is being expanded).
      (define *exit-code* (or ((main SCAM_ARGS)) 0))
      (if SCAM_TRACE
          (trace-dump))
      (if .DEFAULT_GOAL
          ""
          (eval ".PHONY: exit\nexit: ; @exit $(*exit-code*)"))))
