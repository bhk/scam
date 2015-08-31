;; compile.scm
;;
;; The following diagram summarizes the stages of compiling a SCAM
;; expression:
;;
;;               pos                 env
;;                |                   |
;;                v                   v
;;   text    +---------+   form   +------+    IL    +------+   exe
;;  -------->|  parse  |--------->|  c0  |--------->|  c1  |-------->
;;           +---------+          +------+          +------+
;;                |                   |                 |    errors
;;                v                   v                 +----------->
;;               pos                 env
;;
;; Each expression begins at a position "pos" (a numeric index into the
;; sequence of tokens in the subject text).  Parsing emits a "form" (an AST
;; node) and a position at which to look for subsequent expressions.
;;
;; The compiler front end (c0) operates on a form and an environment (a set
;; of symbol bindings), and emits an IL node and a new environment, since
;; expressions (e.g. `declare` and `define`) can alter the environment for
;; subsequent expressions.
;;
;; The compiler back end (c1) emits executable code (Make source) and a
;; (hopefully empty) vector of errors.  The form and IL data structures can
;; convey errors as well as successful results; c1 must output a separate
;; value for error information.
;;
;; File vs. Function Syntax
;;
;; The `c1` phase may generate code for different syntactic contexts in
;; Make.  "File" code can appear as a line in a Makefile and is suitable for
;; passing to Make's `eval` builtin.  "Function" code can appear within a
;; function body, is suitable for invoking directly or binding to a function
;; variable.
;;
;;     SCAM source:     (set-global "x" 1)    (+ 1 2)
;;     Function Code:   $(call ^set,x,1)      $(call +,1,2)
;;     File Code:       x = 1                 $(if ,,$(call +,1,2))

(require "core")
(require "parse")
(require "gen")
(require "gen0")
(require "gen1")
(require "io")

;; This module doesn't use any lexical bindings from "macros" and therefore
;; will compile without the following line, but we require it to load the
;; module at run-time so that the macros will be known to the compiler.
(if 1
    (require "macros"))


;; "Boot" modules: implicit dependencies of other source files.
;;
(define `rt-mod "runtime")  ;; run-time module (implicit "require")
(define `ct-mod "scam-ct")  ;; compile-time module (implicit "use")


;; Compile SCAM source to executable code.
;;
;; Returns:
;;     [ <errors> <exe> <exports> ]    (if outfile is non-nil)
;;     [ <errors> <exe> <newenv> ]     (if outfile == "")
;;
;; If `outfile` is non-nil, it compiles the code for inclusion in a file
;; (is-file == 1) and returns data required to generate an output file (text
;; "exports" in addition to an executable file.
;;
;; If `outfile` is nil, it compiles the code to a form suitable for
;; evaluation (is-file == nil) and returns the final environment.
;;
;; See `compile-forms` for descriptions of other arguments.
;;
(define (compile-text text env infile outfile)
  (let-global ((*compile-subject*  (penc text))
               (*compile-file*     infile)
               (*compile-outfile*  outfile))

    (c0-block-cc (parse-subject *compile-subject*)
                 (or env base-env)
                 (if outfile
                     (lambda (nodes newenv)
                       (append (gen1 nodes 1) [(env-export newenv)]))
                     (lambda (nodes newenv)
                       (append (gen1 nodes) [newenv]))))))


;; Return initial environment (standard prelude)
;;
;; is-boot = one of:
;;    nil   => implicitly (require "runtime") and (use "scam-ct")
;;    true  => no implicit dependencies
;;
;; The implicit modules will be satisfied by bundles unless *compile-mods*
;; contains a matching MIN file name.
;;
(define (compile-prelude is-boot)
  ;; include the runtime if we are building one
  (foreach r *compile-mods*
           (if (filter rt-mod (notdir (basename r)))
               (eval (concat "include " r))))

  (if (not is-boot)
        (append (require-module rt-mod nil)
                (use-module ct-mod))))


(define `(construct-file infile exports exe reqs uses)
  (concat "# compiled from " infile "\n"
          (if reqs (concat "# Requires: " reqs "\n"))
          (if uses (concat "# Requires: " uses "\n"))
          exports
          exe))


;; Compile a SCAM source file and write out a .min file.
;;
;; infile = source file name (to be read)
;; outfile = object file name (to be written)
;; is-boot = true when building "boot" files (runtime, scam-ct, etc.)
;; mod-files = list of files that satisfy `require` and `use` dependencies.
;; reqs = files directly required by this source file
;; uses = files indirectly required by this source file
;;
(define (compile-file infile outfile is-boot mod-files reqs uses)
  (let-global ((*compile-mods* mod-files))
    (let ((text (read-file infile))
          (outfile outfile)
          (imports (compile-prelude is-boot)))
      (let ((o (compile-text text (rest imports) infile outfile))
            (text text)
            (infile infile)
            (outfile outfile))
        (if (first o)
            ;; Error
            (begin
              (for e (first o)
                   (info (describe-error e text infile)))
              (error "error compiling SCAM"))

            ;; Success
            (write-file outfile (construct-file infile (nth 3 o) (nth 2 o) reqs uses)))))))
