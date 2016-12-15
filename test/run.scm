;; test/run: Test of code generation.  This module tests the compiler that
;; has compiled it.


;; This module is compiled by .out/a/scam and it cannot use bundled modules,
;; so we use "../core" to identify the location of the source file.
(require "../core")


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


;; compile-time escaping

(define v0 &global "$a$$a$$$a\\\\n")
(expect v0 "$a$$a$$$a\\\\n")

(define v1 &global " $a $$a $$$a \\\\n\n ")
(expect v1 " $a $$a $$$a \\\\n\n ")

(define (f0) &global nil)
(expect "" f0)

;; run-time escaping of assignment values

;; Function values sometimes have "$ " inserted to allow 'define ... endef'
;; to work.  This should not affect the function's behavior when expanded,
;; but the raw value might not exactly match the assigned value.
(define `(unmunge val)
  (subst "$ " "" val))


(declare var &global)
(declare (fun) &global)

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



(for name [ "a# " "a; " "a: " "a ( " "a ) " "a\n" " $a" "a=1" "a:=1"]
     (set-global name name)
     (expect "simple" (flavor name))
     (expect name (value name))

     (set-rglobal name name)
     (expect "recursive" (flavor name))
     (expect name (value name)))

;; append-for

(expect "3 1 2 3" (append-for n "3 4 1" (nth-rest n "1 2 3")))

;; concat-for

(expect " |\t| " (concat-for a [" " "\t" " "] "|" a))
(expect "(1) (2) (3)" (concat-for a "1 2 3" " " (concat "(" a ")")))

;; Flies in the ointment (function values)

(set-rglobal "fun" "define\nendef\n\\")
(expect "$ define\n$ endef\n\\$ " fun)


;; Implicit macros


(define TA 0)
(when 1
   (set TA (concat TA 1))
   (set TA (concat TA 2)))
(when nil
   (set TA (concat TA 4))
   (set TA (concat TA 5)))

(expect TA "012")

;; data

(data TestType
      (CA &word a b &list c)
      (CB))

(expect 1 (word 2 (CA 1 2 3)))

(expect "1 3"
        (case (CA 1 " " 3)
          ((CA a b c)  (concat a b c))
          ((CB)        2)))
(expect "2"
        (case (CB)
          ((CA a b c)  (concat a b c))
          ((CB)        2)))

;;----------------------------------------------------------------
;; Known bugs
;;----------------------------------------------------------------

;; BUG: lambda inside macro inside function
;;
;; (define (fun-in-macro x)
;;   (define `m1 (lambda () x))
;;   (define `(m2 a) a)
;;   (define `(m3 a) (lambda () a))  ;; BUG
;;   [ (lambda () x)  m1  (m2 x)  (m2 m1)  (m3 x) ])
;;
;; (expect [9 9 9 9] (fun-in-macro 9))


;; BUG: lambda inside data/case (another EIL/Lambda example)
;;
;; (data A (C a b c))
;;
;; (define (fun-in-case x)
;;   (case x
;;     ((C a b c)
;;      (let ((B b))
;;        (concat a "," B "," c)))))
;;
;; (print "fun-in-case = " fun-in-case)
;; ;; gets "3,2 3," instead
;; (expect "1,2 3,4" (fun-in-case (C 1 "2 3" 4))))


;; BUG: never occurs in expression syntax, but CAN occur in file syntax:
;;
;; (declare (f))
;; (set f " ($.@ERROR@) ")

;; BUG:  (printf "%q\n" "")

(print "test-gen ok")
