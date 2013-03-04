;; runtime: runtime functions
;;
;; When the compiler compiles SCAM sources it emits Make code that refers to
;; these runtime functions, but it also compiles these functions, which
;; introduces some potentially tricky dependency issues.

(declare ARGS)

(declare SCAM_DEBUG) ; See SCAM.txt
(declare SCAM_MAIN)
(native "SCAM_DEBUG ?=")

(define *included* "")

(if (findstring "R" SCAM_DEBUG)
    (print "runtime: " (lastword MAKEFILE_LIST)))

(native "define \\n


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
(native "^Y = $(call if,,,$(10))")

;; We declare *args* as a SCAM variable (not function), but we assign it as a
;; recursive variable (not simply-expanded).  References to it will
;; therefore compile to $(*args*), so it will be expanded (executed) without
;; overriding $1, $2, etc.


;; *args*: return a vector of all arguments (up to the 8th argument).

(declare *args*)

(declare (^apply func vec))

(native
"*args* = $(foreach n,1 2 3 4 5 6 7 8,$(if $(findstring auto,$(call origin,$n)),$(call ^d,$($n))))$(if $9, $9)
^apply = $(call ^Y,$(call ^n,1,$2),$(call ^n,2,$2),$(call ^n,3,$2),$(call ^n,4,$2),$(call ^n,5,$2),$(call ^n,6,$2),$(call ^n,7,$2),$(call ^n,8,$2),$(wordlist 9,9999,$2),$1)")


;;--------------------------------------------------------------
;; debugging and diagnostics
;;--------------------------------------------------------------


;; format a value as a quoted string
(define (^fmt a)
  (concat "\""
          (subst "\\" "\\\\" "\"" "\\\"" a)
          "\""))


(define (^value name value)
  (concat
   (info (concat name " " (^fmt value)))
   value))

;; ^v : runtime detection of undefined references

(define (^v name is-func)
  (and (filter "undefined" (flavor name))
       (info (concat "warning: " (if is-func "call to" "read of")
                     " undefined variable: " name))
       name))

;; ^trace : trace function call with arguments and results

(native
"^shift = $(call $1,$2,$3,$4,$5,$6,$7,$8,$(call ^n,1,$9),$(wordlist 2,9999,$9))
^fmt-args = $(if $(or $1,$2,$3,$4,$5,$6,$7,$8,$9), $(^fmt)$(call ^shift,^fmt-args,$2,$3,$4,$5,$6,$7,$8,$9))
^trace = $(info --> ($1$(call ^shift,^fmt-args,$2,$3,$4,$5,$6,$7,$8,$9)))$(call ^value,<-- $1:,$(call ^shift,$1,$2,$3,$4,$5,$6,$7,$8,$9))")

;; When hacking .min files, prepending "$?" is a quick way to trace calls
(native "? = $(info ?: ($0$(^fmt-args)))")


;;--------------------------------------------------------------


;; *file* indicates the currently executing file.  We can only
;; maintain this for SCAM-based make files that use the runtime
;; and that load other SCAM-based files via require.
;;
;; When the runtime is first included, we set it to the next-to-last file in
;; MAKEFILE_LIST.  When require includes a new file, we set it to the
;; required file (and restore the previous file name on completion).
;;
(define *file* (lastword (filter-out "%/runtime.min" MAKEFILE_LIST)))

(define (^esc-RHS str)
  (subst "$" "$$" "#" "$(\\H)" "\n" "$(\\n)" str))

(define (^esc-LHS-x str)
  (subst "$" "$."")" "$R""(" "$(\\L)""$R" "$(\\R)""$." "$$""#" "$(\\H)"
         "\n" "$(\\n)"
         str))

(define (^esc-LHS str)
    ;; $(if ,,...) protects ":", "=", *keywords*, and leading/trailing spaces
    (concat
     "$(if ,,"
     ;; avoid escaping when possible (most likely)
     (if (concat (findstring "\n" str)
                 (findstring "#" str)
                 (findstring "$" str)
                 (findstring "(" str)
                 (findstring ")" str))
         (^esc-LHS-x str)
         str)
    ")"))


;; `eval` with tracing facilities
(define (^eval str)
  (if (findstring "E" SCAM_DEBUG)
      (info (concat "eval: " str)))
  (eval str))


;; ^set = $(call ^eval,$1 := $ $(call ^escRHS,$(subst $$,$$$$,$2)))
(define (^set name value retval)
  (concat (eval (concat (^esc-LHS name)
                        " :=$ "
                        (^esc-RHS value)))
          retval))

;; ^fset = $(call ^eval,$1 = $(or )$(call ^escRHS,$2))
(define (^fset name value retval)
  (define `qname (^esc-LHS name))
  (define `qvalue (subst "endef" "$ endef" "define" "$ define" value))

  (eval (concat "define " qname "\n" qvalue "\nendef\n"))
  retval)


;; Module paths: Paths of the form "///..." name "are bundled" MIN files
;; (stored in variables).  Instead of reading from the file system, we
;; read the value of the variable by that name.


;; Return `modpath` if modpath exists.  
;;
(define (^mod-exists? modpath)
  (if (filter "///%" modpath)
      (if (filter "undef%" (flavor modpath))
          ""
          modpath)
      (or (wildcard modpath)
          (shell (concat "ls " (concat "'" modpath "'") " 2> /dev/null")))))


;; Include (eval) a module.
;;
(define (^mod-load modpath)
  (if (filter "///%" modpath)
      (eval (value modpath))
      (eval (concat "include " modpath))))


;; Find a module file.
;;
;; modname = module name (e.g. "core")
;; scamdir = dir in which to find MIN files.  When compiling (require ...)
;;     directive, this is the output file's directory.  At run time, this
;;     will be "", which will default to the SCAM_MAIN's directory.
;;
(define (^mod-find modname scamdir)
  (define `DIR (or scamdir (dir SCAM_MAIN)))

  (or (^mod-exists? (concat DIR modname ".min"))
      (if (findstring "/" modname)
          (^mod-exists? (concat modname ".min")))))


;; Include a module if it hasn't been included yet.
;;
;; Note: `let-global` introduces a hidden dependency on `^set`.
;;
(define (^require name)
  (let-global
   ((*file* (or (^mod-find name)
                (error (concat "require: cannot find " name " or "
                               (dir SCAM_MAIN) name ".min")))))
   (if (filter *file* *included*)
       ""
       (begin
         (set *included* (concat *included* " " *file*))
         (if (findstring "R" SCAM_DEBUG)
             (info (concat "require: " *file*)))
         (^mod-load *file*)
         (if (findstring "Rx" SCAM_DEBUG)
             (info (concat "exited: " *file*)))))))


;;
;; ^e: lightweight run-time escape
;;

(^set " " "")  ;; "$ " --> empty string

;; wrap s if it starts or ends with spaces or contains a ","
(define (^se s)
  (if (and (if (findstring "," s) "" 1)
           (findstring s (wordlist 1 999999999 s)))
      s
    (concat "$(if ,," s ")")))


(define (^dollars n w d)
  (if (filter n (words w))
      (subst " " "" w)
    (^dollars n (concat w " $"))))


(define (^e s n)
  (subst "$" (if n (^dollars n) "$")
    (^se (subst "$" "$1" ")" "$2\\R)" "(" "$(\\L)" "\n" "$(\\n)" "$2" "$("
                "$1" "$$" s))))


;; Load SCAM_MAIN if it is defined. Define a target to avoid "no targets"
;; error message, but do so after running SCAM_MAIN so that the 'main'
;; program can define its own default target.

(declare *exit-code*)
(declare (main))
(declare SCAM_TRACE)

(define (trace spec warn) "")

(define (trace-dump) "")


(if SCAM_MAIN
    (begin
      ;; Load `trace` after runtime has initialized and before SCAM_MAIN is loaded.
      (if SCAM_TRACE
        (require "trace"))

      (^require SCAM_MAIN)
      (trace SCAM_TRACE)
      (set *exit-code* (or (main ARGS) 0))
      (trace-dump)

      (if .DEFAULT_GOAL
          ""
          (eval ".PHONY: exit\nexit: ; @exit $(*exit-code*)"))))
