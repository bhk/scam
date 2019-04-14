;;--------------------------------
;; repl: Interactive mode for SCAM
;;--------------------------------

(require "core.scm")
(require "io.scm")
(require "parse.scm")
(require "compile.scm")
(require "gen.scm")


(or 1
    ;; Treat these as compile-time dependencies of REPL module, but don't
    ;; automatically load them.  They will be loaded if (repl) is invoked,
    ;; via the implicit prelude for the REPL environment.
    (require "math.scm")
    (require "peg.scm")
    (require "utf8.scm"))


;; Override this on the command line to automatically include a different
;; set of libraries.  repl supplies *1 and *2.
(define LIBS "compile core getopts io math peg string utf8")

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
  (if (or all (not (filter "i%" (EDefn.scope defn))))
      (case defn
        ((EBuiltin name _ args)
         "built-in function")
        ((EFunc name _ argc inln)
         (concat (if (eq? name NoGlobalName)
                     "compound macro"
                     "function")
                 (if inln
                     (sprintf " (%s arguments)" argc))))
        ((EVar name _)
         "variable")
        ((EIL _ _ node)
         (concat "symbol macro: " (format node)))
        ((EXMacro name _)
         (concat "executable macro"))
        ((ERecord encs _ tag)
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
  (REPL text obj-dir is-quiet error &list env))


;; Parse and evaluate text, displaying errors or result.
;;
(define (eval-and-print text obj-dir is-quiet env ?is-interactive)
  (let ((o (compile-text text "[stdin]" env obj-dir is-quiet))
        (env env)
        (text text))
    (define `errors (dict-get "errors" o))
    (define `exe    (dict-get "code" o))
    (define `newenv (dict-get "env" o))

    (cond
     ;; partial/unterminated expr --> append more text
     ((and is-interactive
           (filter "( [ {" (case (first errors)
                             ((PError n desc) (word 1 desc)))))
      (REPL text obj-dir is-quiet nil env))

     ;; error?
     (errors
      (for err errors
           (info (describe-error err text "[stdin]")))
      (REPL "" obj-dir is-quiet 1 env))

     ;; execute & display result
     (else
      (let ((result (exe)))
        (if result
            (begin
              (set *2 *1)
              (set *1 result)
              (print (format result)))))
      (REPL "" obj-dir is-quiet nil newenv)))))


;; Collect another line of input and process it.
;;
;; Returns next state; or nil to terminate loop.
;;
(define (read-eval-print state)
  (define `text-in
    (case state ((REPL text _ _ _ _) text)))

  (let ((line (getline (if text-in "+ " "> ")))
        (state state))
    (define `(saw str)
      (eq? (.strip line) str))

    (case state
      ((REPL text obj-dir is-quiet _ env)
       (cond
        ((saw "?")
         (help)
         state)

        ((saw ":")
         ;; Reset text
         (REPL nil obj-dir is-quiet nil env))

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
         (eval-and-print (concat text line) obj-dir is-quiet env 1)))))))


(define `initial-env
  (begin
    (define `env-text
      (concat (foreach lib LIBS (concat "(require \"" lib "\")"))
              "(declare *1)"
              "(declare *2)"))
    (dict-get "env" (compile-text env-text "[stdin]" nil nil nil))))


;; Read lines of text from stdin, evaluating expressions and displaying
;; results and errors.
;;
(define (repl ?obj-dir)
  &public
  ;; These functions will be "on the stack" in the REPL and should not be
  ;; instrumented from the REPL.
  (do-not-trace "~repl ~eval-and-print ~while ~while-0 ~while-N")
  (while identity read-eval-print
         (REPL nil obj-dir nil nil initial-env))
  (print))


;; Evaluate TEXT and display result and/or errors as REPL does.
;;
(define (repl-rep text ?obj-dir ?is-quiet)
  &public
  (let ((state (eval-and-print text obj-dir is-quiet initial-env nil)))
    (case state
      ((REPL _ _ _ error _) error))))
