;; Miscellany
(require "core")

;; Note: This *assumes* STR begins with PREFIX
;;
(define (remove-prefix prefix str)
  (if prefix
      ;; str..prefix will match the start of str..str; the remainder is
      ;; not as long as str..prefix.
      (subst (.. str prefix) nil (.. str str))
      str))


;; Return 1 if STR begins with PREFIX
;;
(define (begins? prefix str)
  ;; If prefix begins str, select == str..prefix *without* prefix
  ;; Otherwise, select = str, or something shorter.
  (define `select
    (subst (.. str prefix) nil (.. str str prefix)))

  (if prefix
      (and (findstring (.. str prefix)
                      (.. prefix select))
           1)
      1))

(expect 1 (begins? "ababa" ""))
(expect 1 (begins? "ababa" "ab"))
(expect nil (begins? "ababa" "b"))
(expect nil (begins? "ababa" "ba"))

(expect "abcd" (remove-prefix " " " abcd"))


;;
;; eq? implementations
;;

(define (eq-old? a b)
  (define `aa (.. 1 a))
  (define `bb (.. 1 b))
  (if (findstring aa (findstring bb aa))
      1))

(define (eq-new? a b)
  ;; inline-able (evaluates each arg only once)
  (findstring (subst (.. b 0) 1 (.. a 0)) 1))


(require "../perf/clocker.scm")
(require "math")

(define large-a (range 1 10000))
(define large-b (range 1  9999))

(clk-show "Sm eq-old y" (eq-old? 1 2))
(clk-show "Sm eq-new y" (eq-new? 1 2))
(clk-show "Sm eq-old n" (eq-old? 1 1))
(clk-show "Sm eq-new n" (eq-new? 1 1))

(clk-show "Lg eq-old y" (eq-old? large-a large-b))
(clk-show "Lg eq-new y" (eq-new? large-a large-b))
(clk-show "Lg eq-old n" (eq-old? large-a large-a))
(clk-show "Lg eq-new n" (eq-new? large-a large-a))
