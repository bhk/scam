;; # repl: Interactive mode for SCAM
;;
;; REPL mode reads lines of text from `stdin`, evaluating expressions and
;; displaying results and errors.  Typing `:q<RETURN>` or `Ctrl-D` will
;; exit REPL mode.  Typing `?<RETURN>` will show a command reference.
;;

(require "core.scm")
(require "io.scm")
(require "parse.scm")
(require "compile.scm")
(require "gen.scm")


;; We use this trick to cause bundling of these modules even though this
;; module does not use them directly.  Code *compiled* at run-time by this
;; module *may* use them (when and if REPL functions are called).  This
;; trick would not be of use in ordinary SCAM code; only the compiler with
;; its `--boot` mechanism can register built-in libraries.
(or 1
    (require "math.scm")
    (require "peg.scm")
    (require "utf8.scm"))


;; Override this on the command line to automatically include a different
;; set of libraries.  repl supplies *1 and *2.
(define LIBS "compile core getopts io math peg repl string utf8")

(define *1 &native nil)  ; most recent evaluation result
(define *2 &native nil)  ; previous result

(define (help)
  (print "Commands:\n"
         "  :q or ^D : exit REPL loop\n"
         "  :        : reset input state\n"
         "  :e       : show environment\n"
         "  :E       : show environment (including imported entries)\n"
         "  ?        : this message\n\n"
         "Global variables in REPL:\n"
         "  *1 = most recent value printed\n"
         "  *2 = second most recent value\n")
  (printf "LIBS = %s\n" LIBS))


(define (describe-binding bound-name defn all)
  (define `(describe-arity arity)
    (.. (subst " " " or "
               "+" " or more"
               arity)
        "arguments"))

  (if (or all (not (filter "i%" (EDefn.scope defn))))
      (case defn
        ((EBuiltin _ _ args)
         "built-in function")
        ((EFunc _ _ arity)
         (sprintf "function (%s)" (describe-arity arity)))
        ((EMacro _ depth arity _)
         (sprintf "compound macro (%s)" (describe-arity arity)))
        ((EVar _ name)
         "variable")
        ((EIL _ _ node)
         (.. "symbol macro: " (format node)))
        ((EXMacro _ name)
         "executable macro")
        ((ERecord _ encs tag)
         "constructor")
        (else ""))))


;; Print descriptions of all environment entries.
;;
(define (describe-env env all)
  (foreach w (reverse (dict-compact env))
           (let ((name (dict-key w))
                 (desc (describe-binding (dict-key w) (dict-value w) all)))
             (if desc
                 (printf "  %s : %s" name desc)))))


;; REPL state
;;
(data REPL
  ;; TEXT = leftover typed text (awaiting completion of an expression)
  ;; ENV = current environment
  ;; ERROR = error response from most recent evaluation
  (REPL text prompts build-dir is-quiet error &list env))


;; Parse and evaluate text, displaying errors or result.
;;
(define (eval-and-print text prompts build-dir is-quiet env)
  (let ((o (compile-text text "[stdin]" env build-dir is-quiet))
        (env env)
        (text text))
    (define `errors (dict-get "errors" o))
    (define `exe    (dict-get "code" o))
    (define `newenv (dict-get "env" o))

    (cond
     ;; partial/unterminated expr --> append more text
     ((and prompts
           (filter "( [ {" (case (first errors)
                             ((PError n desc) (word 1 desc)))))
      (REPL text prompts build-dir is-quiet nil env))

     ;; error?
     (errors
      (for err errors
           (print (describe-error err text "[stdin]")))
      (REPL "" prompts build-dir is-quiet 1 env))

     ;; execute & display result
     (else
      (let ((result (exe)))
        (if result
            (begin
              (set *2 *1)
              (set *1 result)
              (print (format result)))))
      (REPL "" prompts build-dir is-quiet nil newenv)))))


;; Collect another line of input and process it.
;;
;; A `nil` value for PROMPTS indicates non-interactive mode to
;; eval-and-print, in which case continuation lines will not be queried.
;;
;; Returns next state; or nil to terminate loop.
;;
(define (read-eval-print state)
  (case state
    ((REPL text prompts build-dir is-quiet _ env)

     (let ((line (getline (nth (if text 2 1) prompts)))
           (env env)
           (state state))
       (define `(saw str)
         (eq? (native-strip line) str))

       (cond
        ((saw "?")
         (help)
         state)

        ((saw ":")
         ;; Reset text
         (REPL nil prompts build-dir is-quiet nil env))

        ((saw ":q")
         ;; Exit
         nil)

        ((eq? line "")
         ;; Exit (Ctrl-D)
         nil)

        ((saw ":e")
         (describe-env env nil)
         state)

        ((saw ":E")
         (describe-env env 1)
         state)

        (else
         (eval-and-print (.. text line)
                         prompts build-dir is-quiet env)))))))


(define `initial-env
  (begin
    (define `env-text
      (.. (foreach lib LIBS (.. "(require \"" lib "\")"))
          "(declare *1)"
          "(declare *2)"))
    (let ((o (compile-text env-text "[stdin]" nil nil nil)))
      (define `fn (dict-get "code" o))
      (fn) ;; load modules referenced by the environment
      (dict-get "env" o))))


;; Enter REPL mode, and return to caller when the user exits with `:q` or
;; `Ctrl-D`.
;;
;; BUILD-DIR = [build directory](#build-directory); `nil` for default.\
;; PROMPTS = [P1 P2]; P1 is shown when awaiting an expression; P2 is
;;   shown when awaiting completion of an expression.  If `nil`, default
;;   values will be used.
;;
(define (repl ?build-dir ?prompts)
  &public
  (define `default-prompts
    ["> " "+ "])

  ;; These functions will be "on the stack" in the REPL and should not be
  ;; instrumented from the REPL.
  (do-not-trace "~repl ~eval-and-print ~while ~while-0 ~while-N")

  (while identity read-eval-print
         (REPL nil (or prompts default-prompts) build-dir nil nil initial-env))
  (print))


;; Evaluate TEXT and print results and errors as REPL mode does.
;;
;; TEXT = SCAM source text containing zero or more expressions.\
;; BUILD-DIR = [build directory](#build-directory); `nil` for default.\
;; IS-QUIET = When non-nil, suppresses compilation progress messaged.\
;;
;; Result = non-nil on error.
;;
(define (repl-ep text ?build-dir ?is-quiet)
  &public
  (case (eval-and-print text nil build-dir is-quiet initial-env)
    ((REPL _ _ _ _ error _) error)))
