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
(require "trace.scm")


(define default-libs
  "compile core trace getopts io math peg repl string utf8")

(define *1 &native nil)  ; most recent evaluation result
(define *2 &native nil)  ; previous result

(define (help)
  (print "Commands:\n"
         "  :q or ^D : exit REPL loop\n"
         "  :        : reset input state\n"
         "  :e       : show environment\n"
         "  :E       : show environment (including imported entries)\n"
         "  :R LIBS  : reset environment to only LIBS\n"
         "  ?        : this message\n\n"
         "Global variables in REPL:\n"
         "  *1 = most recent value printed\n"
         "  *2 = second most recent value\n")
  (printf "Default libraries = %s\n" default-libs))


(define (describe-binding bound-name defn all)
  (define `(describe-arity arity)
    (.. (subst " " " or "
               "+" " or more"
               arity)
        " arguments"))

  (if (or all (not (filter "i%" (EDefn.scope defn))))
      (case defn
        ((EBuiltin _ _ args)
         "built-in function")
        ((EFunc _ arity _)
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
  (foreach ({=name: value} (reverse (dict-compact env)))
    (let ((desc (describe-binding name value all))
          (name name))
      (if desc
          (printf "  %s : %s" name desc)))))


;; REPL state
;;
(data REPL
  ;; TEXT = leftover typed text (awaiting completion of an expression)
  ;; ENV = current environment
  ;; ERROR = error response from most recent evaluation
  ;; CFG = [prompts build-dir is-quiet]
  (REPL error text cfg &list env))


;; Parse and evaluate text, displaying errors or result.
;;
(define (eval-and-print text cfg env)
  (define `[prompts build-dir is-quiet]
    cfg)

  (let ((o (compile-text text "[stdin]" env build-dir is-quiet))
        (text text)
        (cfg cfg)
        (env env)
        (prompts prompts))
    (define `{errors: errors, code: exe, env: newenv} o)

    (cond
     ;; partial/unterminated expr --> append more text
     ((and prompts
           (filter "( [ {" (case (first errors)
                             ((PError n desc) (word 1 desc)))))
      (REPL nil text cfg env))

     ;; error?
     (errors
      (for (err errors)
        (print (describe-error err text "[stdin]")))
      (REPL 1 "" cfg env))

     ;; execute & display result
     (else
      (let ((result (exe)))
        (if result
            (begin
              (set *2 *1)
              (set *1 result)
              (print (format result)))))
      (REPL nil "" cfg newenv)))))


(define (load-env libs)
  (begin
    (define `env-text
      (.. (foreach (lib libs)
            (.. "(require \"" lib "\")"))
          "(declare *1 &native)"
          "(declare *2 &native)"))
    (let (({code: f, env: e} (compile-text env-text "[stdin]" nil nil nil)))
      (f) ;; load modules referenced by the environment
      e)))


(define `(initial-env libs)
  (load-env (if libs
                (patsubst "-" nil libs)
                default-libs)))


;; Collect another line of input and process it.
;;
;; A `nil` value for PROMPTS indicates non-interactive mode to
;; eval-and-print, in which case continuation lines will not be queried.
;;
;; Returns next state; or nil to terminate loop.
;;
(define (read-eval-print state)
  (case state
    ((REPL _ text cfg env)
     (define `[prompts build-dir is-quiet] cfg)
     (let ((line (getline (nth (if text 2 1) prompts)))
           (state state)
           (text text)
           (cfg cfg)
           (env env))
       (define `(saw str)
         (eq? (native-strip line) str))

       (cond
        ((saw "?")
         (help)
         state)

        ((saw ":")
         ;; Reset text
         (REPL nil nil cfg env))

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

        ((eq? ":R" (word 1 (native-strip line)))
         (REPL nil text cfg (load-env (rest (native-strip line)))))

        (else
         (eval-and-print (.. text line) cfg env)))))))


;; Enter REPL mode, and return when the user exits with `:q` or `Ctrl-D`.
;;
;; BUILD-DIR = [build directory](#build-directory); `nil` for default.\
;; PROMPTS = [P1 P2]; P1 is shown when awaiting an expression; P2 is
;;    shown when awaiting completion of an expression.  If `nil`, default
;;    values will be used.\
;; LIBS = libraries to load and include in the initial environment;
;;    nil => use the default libraries; `"-"` => none.
;;
(define (repl ?build-dir ?prompts ?libs)
  &public
  (define `default-prompts
    ["> " "+ "])

  (while identity read-eval-print
         (REPL nil nil
                 [(or prompts default-prompts) build-dir nil]
                 (initial-env libs)))
  (print))


;; Evaluate TEXT and print results and errors as REPL mode does.
;;
;; TEXT = SCAM source text containing zero or more expressions.\
;; BUILD-DIR = [build directory](#build-directory); `nil` for default.\
;; IS-QUIET = non-nil to suppress compilation progress messages.\
;; LIBS = libraries to require prior to evaluating TEXT; nil => use the
;;        default libraries; `"-"` => none.
;;
;; Result = non-nil on error.
;;
(define (repl-ep text ?build-dir ?is-quiet ?libs)
  &public
  (case (eval-and-print text [nil build-dir is-quiet] (initial-env libs))
    ((REPL error _ _ _) error)))
