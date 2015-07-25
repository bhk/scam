;; Test code generation by executing compiled code

(require "core")


;; set/fset and escaping

(define var " $ ")
(expect " $ " var)

(define (fun) nil)
(expect "" fun)

;; run-time escaping of lambda expressions

(define (make-lambda a)
  (lambda (b)
    (lambda (c)
      (concat a b c))))

(eq " $ $ " (( (make-lambda " $ ") "$ ") " $ "))


;; compile-time escaping of assignment values

(define (F) "\n")
(expect "\n" (F))

(define (F) "\n\n")
(expect "\n\n" (F))

(define (F) "\\\n\\")
(expect "\\\n\\" (F))


;; run-time escaping of assignment values

;; Function values sometimes have "$ " inserted to allow 'define ... endef'
;; to work.  This should not affect the function's behavior when expanded,
;; but the raw value might not exactly match the assigned value.
(define `(unmunge val)
  (subst "$ " "" val))

(for str [ " # $a "
           "))})({"
           "$a $$a $$$a $$$$a"
           "\\ "
           " \n "
           "\n"
           " \\"
           "\\"
           ]
     (begin
       (set-global "var" str)
       (expect "simple" (flavor "var"))
       (expect str var)

       (set-rglobal "fun" str)
       (expect "recursive" (flavor "fun"))
       (expect str (unmunge fun))))


;; Flies in the ointment (function values)

(set-rglobal "fun" "define\nendef\n\\")
(expect "$ define\n$ endef\n\\$ " fun)



(print "test-gen ok")
