;; repl: Interactive mode for SCAM
;;
(require "core")
(require "io")
(require "parse")
(require "compile")
(require "gen")
(require "num") ;; treat as a dependency (for build system purposes)


;; Override this on the command line to automatically include a different
;; set of libraries.
(define LIBS "core io parse escape gen0 gen1 compile num")

(define *1 &global nil)  ; most recent evaluation result
(define *2 &global nil)  ; previous result

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

(define envtypes
  (append (hash-bind "F" "function")
          (hash-bind "V" "variable")
          (hash-bind "B" "builtin")))

(define (describe-binding b)
  (define `type (word 1 b))
  (define `name (nth 2 b))
  (define `defn (nth 4 b))
  (define `desc (or (and (filter "F" type)
                         (filter MacroName name)
                         "compound macro")
                    (hash-get type envtypes)
                    "<unknown>"))

  (if (filter "M" type)
      (concat "symbol macro: " (format-form (nth 2 b)))
      (concat desc
              (if defn
                  (sprintf ": (%s) -> %s"
                           (first defn)
                           (concat-for f (rest defn) " "
                                    (format-form f)))))))


(define (describe-env env all)
  (foreach w (reverse (hash-compact env))
           (if (or all (not (nth 3 (hash-value w))))
               (printf "  %s : %s" (hash-key w)
                       (describe-binding (hash-value w))))))


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
    (define `errors (nth 1 o))
    (define `exe    (nth 2 o))
    (define `newenv (nth 3 o))
    (define `(is-error codes)
      (filter codes (word 2 (first errors))))

    (cond
     ;; unterminated expr: append more text
     ((is-error "( [") [text env])

     ;; no expressions found: start fresh at ">" prompt
     ((is-error ".") ["" env])

     ;; error?
     (errors (begin
               (for err errors
                    (info (describe-error err text)))
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
      (eq line (concat str "\n")))

    (cond ((typed "?")    (begin (help) state))
          ((typed ":")    ["" env]) ; reset input state
          ((typed ":q")   nil)      ; exit
          ((eq line "")   nil)      ; exit (Ctrl-D)
          ((typed ":e")   (begin (describe-env env) state))
          ((typed ":E")   (begin (describe-env env 1) state))
          (else           (eval-and-print (concat text line) env)))))


;;
;; main
;;

(define `initial-state
  (eval-and-print
   (concat "(declare *1 &global)\n"
           "(declare *2 &global)\n"
           (foreach lib LIBS (concat "(require \"" lib "\")")) "\n")
   (compile-prelude)))


(define (repl)
  (print "SCAM interactive mode. Type '?' for help.")

  (while identity read-eval-print initial-state)
  (print))


;; Evaluate 'text' and print results (without looping).
;;
(define (repl-rep text filename)
  (define `env (nth 2 initial-state))

  (let ((o (compile-text text env (or filename "[commandline]") "")))
    (define `errors (nth 1 o))
    (define `exe    (nth 2 o))

    (if errors
        (begin
          (for err errors
               (info (describe-error err text)))
          1)

        ;; execute & display result
        (print (exe)))))


;; Load and excute file 'file'
;;
(define (repl-file file)
  (let ((text (read-file file)))
    (if text
        (let ((o (compile-text text (compile-prelude) file "///~" nil)))
          (define `errors (nth 1 o))
          (define `exe    (nth 2 o))

          (if errors
              (begin
                (for err errors
                     (info (describe-error err text file)))
                1)
              (eval exe)))
        (begin (printf "error: empty/missing file %q" file)
               1))))
