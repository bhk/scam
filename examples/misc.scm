;;
;; Playground for testing new algorithms, including potentially faster
;; versions of SCAM functions.
;;

(require "../core.scm" &private)
(require "../perf/clocker.scm")
(require "math")
(require "string")


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


(define (clock-eq)
  (define large-a (range 1 10000))
  (define large-b (range 1  9999))

  (clk-show "Sm eq-old y" (eq-old? 1 2))
  (clk-show "Sm eq-new y" (eq-new? 1 2))
  (clk-show "Sm eq-old n" (eq-old? 1 1))
  (clk-show "Sm eq-new n" (eq-new? 1 1))

  (clk-show "Lg eq-old y" (eq-old? large-a large-b))
  (clk-show "Lg eq-new y" (eq-new? large-a large-b))
  (clk-show "Lg eq-old n" (eq-old? large-a large-a))
  (clk-show "Lg eq-new n" (eq-new? large-a large-a)))


;; Remove PREFIX from the beginning of STR.
;;
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
;; indices
;;

(define (indices-a lst)
  &public
  (if (word 10 lst)
      (urange 1 (words lst))
      (wordlist 1 (words lst) nzdigits)))


;; indices-b: faster for larger lists, but more complicated.

;; zz="" => start at 10;  "0" => start at 100
(define (indices-x lst zz nzdigits)
  (._. (foreach (n nzdigits)
         (if (word (.. n 0 zz) lst)
             (permute (.. "0 " nzdigits) zz n)))
       (if (word (.. 100 zz) lst)
           (indices-x lst (.. zz 0) nzdigits))))

(define (indices-b lst)
  &public
  (wordlist 1 (words lst)
            (._. nzdigits
                 (if (word 10 lst)
                     (indices-x (patsubst "%" "." lst) nil nzdigits)))))


(define (clock-indices)
  (for (n [1 2 3 4 10 50 500 1000 5000])
    (define R (range 1 n))
    (expect (indices-a R) (indices R))
    (clk-show (.. "indices   " n) (indices R))
    (clk-show (.. "indices-a " n) (indices-a R))
    (clk-show (.. "indices-b " n) (indices-b R))))


(define (main)
  (clock-eq)
  (clock-indices)
  nil)
