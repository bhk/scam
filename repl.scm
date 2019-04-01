;;--------------------------------
;; repl: Interactive mode for SCAM
;;--------------------------------

(require "core.scm")
(require "io.scm")
(require "parse.scm")
(require "compile.scm")
(require "gen.scm")
(begin
  ;; treat as a dependency (for build system purposes)
  (require "math.scm")
  (require "peg.scm"))


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


;; Parse and evaluate text, displaying errors or result.
;; Return:  [ INCOMPLETE-TEXT NEWENV ERROR? ]
;;
(define (eval-and-print text env ?is-interactive)
  (let ((o (compile-text text "[stdin]" env))
        (env env)
        (text text))
    (define `errors (dict-get "errors" o))
    (define `exe    (dict-get "code" o))
    (define `newenv (dict-get "env" o))
    (define `(is-error codes)
      (and is-interactive
           (filter codes (case (first errors)
                           ((PError n desc) (word 1 desc))))))

    (cond
     ;; unterminated expr: append more text
     ((is-error "( [ {") [text env])

     ;; error?
     (errors (begin
               (for err errors
                    (info (describe-error err text "[stdin]")))
               ["" env 1]))

     ;; execute & display result
     (else (begin
             (let ((result (exe)))
               (if result
                   (begin
                     (set *2 *1)
                     (set *1 result)
                     (print (format result)))))
             ["" newenv])))))


;; Collect another line of input and process it.
;;
;; STATE = [TEXT ENV]    (TEXT = previous incomplete expression)
;; Returns next state; or nil to terminate loop.
;;
(define (read-eval-print state)
  (let ((line (getline (if (first state) "+ " "> ")))
        (text (nth 1 state))
        (env (nth 2 state))
        (state state))

    (define `(typed str)
      (eq? (strip-vec line) str))

    (cond ((typed "?")    (begin (help) state))
          ((typed ":")    ["" env]) ; reset input state
          ((typed ":q")   nil)      ; exit
          ((eq? line "")  nil)      ; exit (Ctrl-D)
          ((typed ":e")   (begin (describe-env env nil) state))
          ((typed ":E")   (begin (describe-env env 1) state))
          (else           (eval-and-print (concat text line) env 1)))))


(define `initial-state
  (eval-and-print
   (concat (foreach lib LIBS
                    (concat "(require \"" lib "\")"))
           "(declare *1)"
           "(declare *2)")
   nil))


;; Read lines of text from stdin, evaluating expressions and displaying
;; results and errors.
;;
(define (repl)
  &public
  ;; These functions will be "on the stack" in the REPL and should not be
  ;; instrumented from the REPL.
  (do-not-trace "~repl ~eval-and-print ~while ~while-0 ~while-N")
  (while identity read-eval-print initial-state)
  (print))


;; Evaluate 'text' and print results (without looping).
;;
(define (repl-rep text)
  &public
  (define `env (nth 2 initial-state))
  (word 3 (eval-and-print text env)))
