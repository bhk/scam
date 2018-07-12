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
  (require "num.scm"))


;; Override this on the command line to automatically include a different
;; set of libraries.  repl supplies *1 and *2.
(define LIBS "compile core getopts io num string utf8")

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


;;
;; Display environment entries
;;

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


(define (describe-env env all)
  (foreach w (reverse (dict-compact env))
           (let ((name (dict-key w))
                 (desc (describe-binding (dict-key w) (dict-value w) all)))
             (if desc
                 (printf "  %s : %s" name desc)))))

;;
;; Read, eval, print
;;


;; Parse and evaluate text, displaying errors or result.
;; Return:  [ incomplete-text newenv ]
;;
(define (eval-and-print text env)
  (let ((o (compile-text text env "[console]" ""))
        (env env)
        (text text))
    (define `errors (dict-get "errors" o))
    (define `exe    (dict-get "code" o))
    (define `newenv (dict-get "env" o))
    (define `(is-error codes)
      (filter codes (case (first errors)
                      ((PError n desc) (word 1 desc)))))

    (cond
     ;; unterminated expr: append more text
     ((is-error "( [ {") [text env])

     ;; no expressions found: start fresh at ">" prompt
     ((is-error ".") ["" env])

     ;; error?
     (errors (begin
               (for err errors
                    (info (describe-error err text "[stdin]")))
               ["" env]))

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
;; On entry: state = [text env]    (text = previous incomplete expression)
;; Return value: next state; or nil to terminate loop.
;;
(define (read-eval-print state)
  (let ((line (getline (if (first state) "+ " "> ")))
        (text (nth 1 state))
        (env (nth 2 state))
        (state state))

    (define `(typed str)
      (eq? line (concat str "\n")))

    (cond ((typed "?")    (begin (help) state))
          ((typed ":")    ["" env]) ; reset input state
          ((typed ":q")   nil)      ; exit
          ((eq? line "")   nil)      ; exit (Ctrl-D)
          ((typed ":e")   (begin (describe-env env nil) state))
          ((typed ":E")   (begin (describe-env env 1) state))
          (else           (eval-and-print (concat text line) env)))))


;;
;; main
;;

(define `initial-state
  (eval-and-print
   (concat (foreach lib LIBS (concat "(require \"" lib "\")"))
           "(declare *1)(declare *2)")
   (append (compile-prelude nil))))


(define (repl)
  &public
  ;; These functions will be "on the stack" in the REPL and should not be
  ;; instrumented from the REPL.
  (do-not-trace "~repl ~eval-and-print ~while ~while-0 ~while-N")
  (while identity read-eval-print initial-state)
  (print))


;; Evaluate 'text' and print results (without looping).
;;
(define (repl-rep text filename)
  &public
  (define `env (nth 2 initial-state))

  (let ((o (compile-text text env (or filename "[commandline]") ""))
        (text text))
    (define `errors (dict-get "errors" o))
    (define `exe    (dict-get "code" o))

    (if errors
        (begin
          (for err errors
               (info (describe-error err text nil)))
          1)

        ;; execute & display result
        (let ((result (exe)))
          (if result
              (print (format result)))))))


;; Load and execute file 'file'
;;
(define (repl-file file)
  &public
  (let ((text (read-file file))
        (file file))
    (if text
        (let ((o (compile-text text (compile-prelude nil) file "///~"))
              (text text)
              (file file))
          (define `errors (dict-get "errors" o))
          (define `exe    (dict-get "code" o))

          (if errors
              (begin
                (for err errors
                     (info (describe-error err text file)))
                1)
              (eval exe)))
        (begin (printf "error: empty/missing file %q" file)
               1))))
