;;--------------------------------
;; repl: Interactive mode for SCAM
;;--------------------------------

(require "core")
(require "io")
(require "parse")
(require "compile")
(require "gen")
(require "num") ;; treat as a dependency (for build system purposes)


;; Override this on the command line to automatically include a different
;; set of libraries.  repl supplies *1 and *2.
(define LIBS "core io parse escape gen0 gen1 compile num repl")

(define *1 &public nil)  ; most recent evaluation result
(define *2 &public nil)  ; previous result

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
        ((EFunc name _ _ inln)
         (concat (if (eq? name NoGlobalName)
                     "compound macro"
                     "function")
                 (if (rest inln)
                     (sprintf ": %s -> %s"
                              (first inln)
                              (format-form (begin-block (rest inln)))))))
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
  (foreach w (reverse (hash-compact env))
           (let ((name (hash-key w))
                 (desc (describe-binding (hash-key w) (hash-value w) all)))
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
    (define `errors (nth 1 o))
    (define `exe    (nth 2 o))
    (define `newenv (nth 3 o))
    (define `(is-error codes)
      (filter codes (case (first errors)
                      ((PError n desc) desc))))

    (cond
     ;; unterminated expr: append more text
     ((is-error "( [") [text env])

     ;; no expressions found: start fresh at ">" prompt
     ((is-error ".") ["" env])

     ;; error?
     (errors (begin
               (for err errors
                    (info (describe-error err text nil)))
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
   (foreach lib LIBS (concat "(require \"" lib "\")\n"))
   (compile-prelude nil)))


(define (repl)
  &public
  (print "SCAM interactive mode. Type '?' for help.")

  (while identity read-eval-print initial-state)
  (print))


;; Evaluate 'text' and print results (without looping).
;;
(define (repl-rep text filename)
  &public
  (define `env (nth 2 initial-state))

  (let ((o (compile-text text env (or filename "[commandline]") "")))
    (define `errors (nth 1 o))
    (define `exe    (nth 2 o))

    (if errors
        (begin
          (for err errors
               (info (describe-error err text nil)))
          1)

        ;; execute & display result
        (print (exe)))))


;; Load and excute file 'file'
;;
(define (repl-file file)
  &public
  (let ((text (read-file file)))
    (if text
        (let ((o (compile-text text (compile-prelude nil) file "///~")))
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
