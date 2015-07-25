(require "core")

;; May be LF or CRLF, depending on platform
(define -newline
   &private
"
")

(define ascii-CR "")
(define ascii-LF (subst ascii-CR "" -newline))
(define ascii-upper "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z")
(define ascii-lower "a b c d e f g h i j k l m n o p q r s t u v w x y z")
(define ascii-digit "0 1 2 3 4 5 6 7 8 9")
(define ascii-punct
  "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \\ ] ^ _ ` { | } ~")
;; This does not include \t, CR, and LF because they will not be treated as
;; words.
(define ascii-unprintable
   "                           ")


(define `ascii-printable
  (append ascii-upper ascii-lower ascii-digit ascii-punct))

(expect 94 (words ascii-printable))
(expect 28 (words ascii-unprintable))


(define (strlen-smash s ch)
  &private
  (if (word 1 ch)
      (strlen-smash (subst (word 1 ch) 0 s) (rest ch))
      s))


(define (strlen s)
  (let& ((s2 (subst " " 0 "\t" 0 ascii-CR 0 ascii-LF 0 s))
         (s3 (strlen-smash s2 ascii-unprintable))
         (s4 (strlen-smash s3 ascii-printable)))
    (words (subst 0 "0 " s4))))


(expect 9 (strlen "a\"b'\\x \t\n"))
