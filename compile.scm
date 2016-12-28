;;----------------------------------------------------------------
;; compile.scm
;;----------------------------------------------------------------

(require "core")
(require "parse")
(require "gen")
(require "gen0")
(require "gen1")
(require "io")

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

;; This module doesn't use any lexical bindings from "macros" and therefore
;; will compile without the following line, but we require it to load the
;; module at run-time so that the macros will be known to the compiler.

(begin
  (require "macros"))


;; Implicit modules: these are implicit dependencies of all other
;; source files.
;;
(define `rt-mod &public "runtime")  ;; run-time module (implicit "require")
(define `ct-mod &public "scam-ct")  ;; compile-time module (implicit "use")


;; Compile SCAM source to executable code.
;;
;; Returns:
;;     [ <errors> <exe> <env-out> ]
;;
;; TEXT = SCAM source
;; ENV = Initial environment. This is normally generated by compile-prelude.
;;       It includes exports from implicit modules, unless the file being
;;       compiled is itself an inplicit module.  When called from the REPL,
;;       this will contain additional bindings from the user's session.
;; INFILE = Input file name (or '[command line]').
;; OUTFILE = When nil, code will be compiled for function syntax.  When
;;           non-nil, code will be compiled for file syntax.
;;
(define (compile-text text env infile outfile)
  &public
  (let-global ((*compile-subject*  (penc text))
               (*compile-file*     infile))

    (c0-block-cc env
                 (parse-subject *compile-subject*)
                 (lambda (env-out nodes)
                   (conj (gen1 nodes outfile)
                         env-out)))))


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
  &public
  (if (not is-boot)
      (append (require-module rt-mod nil)
              (use-module ct-mod))))


(define `(construct-file infile env exe reqs uses)
  (concat "# compiled from " infile "\n"
          (if reqs (concat "# Requires: " reqs "\n"))
          (if uses (concat "# Requires: " uses "\n"))
          (env-export env)
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
  &public
  (if (findstring "B" SCAM_DEBUG)
      (printf (concat "compile-file: %s -> %s%s\n"
                      " mod-files = %s\n reqs = %s\n uses = %s\n")
              infile outfile (if is-boot " [--boot]")
              mod-files reqs uses))

  (let-global ((*compile-mods* mod-files))
    (let ((text (read-file infile))
          (outfile outfile)
          (imports (compile-prelude is-boot)))
      (let ((o (compile-text text imports infile outfile))
            (text text)
            (infile infile)
            (outfile outfile))
        (begin
          (define `errors (first o))
          (define `exe (nth 2 o))
          (define `env-out (nth 3 o))

          (if errors
            ;; Error
            (begin
              (for e errors
                   (info (describe-error e text infile)))
              (error (subst "%S" (if (eq? 1 (words (first o))) "" "s")
                            "compilation error%S")))

            ;; Success
            (write-file outfile
                        (construct-file infile env-out exe reqs uses))))))))
