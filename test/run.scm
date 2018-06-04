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

(eq? " $ $ " (( (make-lambda " $ ") "$ ") " $ "))


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

;; current-file-line

(define `(fl0)
  (current-file-line))

;; WARNING: very fragile tests...
(expect "run.scm:106"
        (notdir (current-file-line)))
(expect "run.scm:108"
        (notdir (fl0)))


;; TODO: Reenable these tests.  Buring bootstrapping, the first-gen compiler
;; cannot reliably load its own compiled code.  Unless that can be resolved,
;; this should be moved to second-stage test.
;;
;; ;; Executable macros
;;
;; (define TA 0)
;; (when 1
;;    (set TA (concat TA 1))
;;    (set TA (concat TA 2)))
;; (when nil
;;    (set TA (concat TA 4))
;;    (set TA (concat TA 5)))
;;
;; (expect TA "012")


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


;; macro / lambda translations

(expect 12
        (let ((a 1))
          (define `M
            (concat a (let ((b 2))
                        b)))
          (let ((c 3))
            M)))


;;--------------------------------
;; Regression tests
;;--------------------------------

;; arg9+ references are different from 1...8

(begin
  (define (f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    (let ((a 1))
      (concat a9 a10)))

  (expect 910 (f 1 2 3 4 5 6 7 8 9 10)))


;; lambda inside macro inside function  [9 9 1 9] vs. [9 9 9 9]

(define (fun-in-macro x)
  (define `m1 (lambda () x))
  (define `(m2 a) a)

  ;; BUG:  Macro arg expanded in deeper nesting context.
  ;;      (e.g. from $1 to $.1)
  (define `(m3 a) (let ((b 1)) a))

  ;; Compound macro body used in deeper nesting context than defn.
  (define `(m4) x)

  [ (m1)  (m2 x)  (m3 x) (let ((a 0)) (m4)) ])

(expect [9 9 9 9] (fun-in-macro 9))


;; Lambda inside data/case
;;
(data A (C a b c))

(define (fun-in-case x)
  (case x
    ((C a b c)
     (let ((B b))
       (concat a "," B "," c)))))

;; gets "3,2 3," instead
(expect "1,2 3,4" (fun-in-case (C 1 "2 3" 4)))


;; Should not be confused with embedded errors.
;;
(declare (f))
(set f " ($.@ERROR@) ")

;; Value of macro in different nesting context (up-value must be adjusted)
;;
(expect ((let ((a 7))
           (define `(M x) (concat a x))
           (let ((b 1))
             M)) 2)
        72)

(print "test-gen ok")
