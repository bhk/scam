;; test/run: Test of code generation.  This module tests the compiler that
;; has compiled it.


;; This module is compiled by .out/a/scam and it cannot use bundled modules,
;; so we use "../core" to identify the location of the source file.
(require "../core.scm")


;; current-file-line

(define `(fl0)
  (current-file-line))

;; WARNING: fragile tests... Just update line numbers if they have changed.
(expect "run.scm:17"
        (notdir (current-file-line)))
(expect "run.scm:19"
        (notdir (fl0)))


;; run-time escaping of lambda expressions

(define (make-lambda a)
  (lambda (b)
    (lambda (c)
      (concat a b c))))

(expect " $1  $2  $3 " (( (make-lambda " $1 ") " $2 ") " $3 "))


;; compile-time escaping of assignment values

(define (F) "\n")
(expect "\n" (F))

(define (F) "\n\n")
(expect "\n\n" (F))

(define (F) "\\\n\\")
(expect "\\\n\\" (F))

;; compile-time escaping

(define v0 &native "$a$$a$$$a\\\\n")
(expect v0 "$a$$a$$$a\\\\n")

(define v1 &native " $a $$a $$$a \\\\n\n ")
(expect v1 " $a $$a $$$a \\\\n\n ")

(define (f0) &native nil)
(expect "" f0)

;; run-time escaping of assignment values

;; Function values sometimes have "$ " inserted to allow 'define ... endef'
;; to work.  This should not affect the function's behavior when expanded,
;; but the raw value might not exactly match the assigned value.
(define `(unmunge val)
  (subst "$ " "" val))


(declare var &native)
(declare (fun) &native)

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
       (set-native "var" str)
       (expect "simple" (flavor "var"))
       (expect str var)

       (set-native-fn "fun" str)
       (expect "recursive" (flavor "fun"))
       (expect str (unmunge fun))))


;; GNU Make 3.82 strips spaces from start and end of a variable name
;; (*after* expansion) in `define VARNAME ...  endef`.  We don't need to
;; worry about making native-name well-defined, since it isn't an exposed
;; building block in SCAM anymore.  We just need to worry about it handling
;; the native names of SCAM symbols.  Of note, characters "#+=?\\" are a
;; concern for make.

(for name [ "a#b" "a+" "a?" "a=" "a?=" "a\\#" "override" "include" "'a" "\"a" "`a"]
     (set-native name name)
     (expect (concat "simple:" name) (concat (flavor name) ":" name))
     (expect name (value name))

     (set-native-fn name name)
     (expect (concat "recursive:" name) (concat (flavor name) ":" name))
     (expect name (value name)))

;; append-for

(expect "3 1 2 3" (append-for n "3 4 1" (nth-rest n "1 2 3")))

;; concat-for

(expect " |\t| " (concat-for a [" " "\t" " "] "|" a))
(expect "(1) (2) (3)" (concat-for a "1 2 3" " " (concat "(" a ")")))

;; Flies in the ointment (function values)

(set-native-fn "fun" "define\nendef\n\\")
(expect "$ define\n$ endef\n\\$ " fun)

;; builtins as functions

(expect "by bz" ((or filter) "b%" "ax by bz"))
(expect 2 ((first [or]) nil 2 3))


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

;; automatic var captures

(expect ((foreach N 3 (lambda () N)))
        3)

;; rest arg captures

(expect ((lambda (...z) (let ((a 9)) z)) 1 2 3)
        [1 2 3])

;; macro arguments

(define `(m10 a b c d e f g h i j)
  (concat "9:" i ":10:" j))
(expect (m10 1 2 3 4 5 6 7 "a b" "c d" "e f")
        ;; was "9:c:10:d"
        "9:c d:10:e f")

;; failure to escape newline in file-syntax expression

(set F (or "\n"))


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
       (concat a "," b "," c)))))

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
