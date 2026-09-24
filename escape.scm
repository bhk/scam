;--------------------------------------------------------------
;; escape : escaping
;;--------------------------------------------------------------

(require "core.scm")

;; The problem of "escaping" strings for inclusion in Make source code can
;; be broken down into two aspects.
;;
;; First, there is escaping `$` characters to survive expansion.  This is
;; handled by the `escape` function.
;;
;; Second, special characters must be quoted that would otherwise have
;; significance in the Make syntax.  This task varies depending on
;; where in the Makefile the code will appear:
;;
;;   - the left-hand-side of a variable definition (before "=", ":=" or "+=")
;;   - the right-hand-side of a variable definition
;;   - the contents of a "define VAR ... endef" statement
;;   - an expression occurring by itself on a line.
;;   - an argument to a function
;;      - arguments to '$(and ...)' and '$(or ...)'
;;      - the first argument to $(call ...)
;;      - other arguments functions
;;   - a variable name between "$(" and ")"
;;
;; These differ in the following respects:
;;
;;   - must "#" must be escaped with a backslash?
;;   - may "#" appear at all?
;;   - are leading and/or trailing whitespace characters discarded?
;;   - may unbalanced parentheses appear?
;;   - may "," appear outside of balanced parentheses?
;;   - may "=" or ":" appear outside of balanced parentheses?
;;   - may newlines be included?
;;   - may "define" or "endef" occur as the first word of a line?
;;
;; Functions that perform the second step are called "protect-XXX".


;; Convert a literal string to a form that will survive expansion.  We use
;; $` instead of $$ to avoid exponential growth after repeated escape
;; operations.
(define `(escape str)
  &public
  (subst "$" "$`" str))


(define `(replace-nl str)
  (subst "\n" "$'" str))

(define `(replace-hash str)
  (subst "#" "$\"" str))


;; Prevent leading spaces from being trimmed.
;;
(define (protect-ltrim str)
  &public
  (define `(begins-white str)
    (findstring (word 1 (.. 0 str 0)) 0))
  (.. (if (begins-white str) "$ ") str))


;; Prevent leading and trailing whitespace from being trimmed by enclosing
;; in "$(if ,,...)".
;;
(define (protect-trim s)
  &public
  (if (findstring " \\ " (.. " \\" (subst "\n" " " s) "\\ "))
      (.. "$(if ,," s ")")
      s))


;; OBJ = non-empty object string split into words at "(..." and "...)"
;; Sanitize (remove "!@" markers from) all matching "(...)".
;;
(define (clear-nested obj)
  (define `RECUR
    (clear-nested
     (subst " " ""
            "!@(" " !@("
            "!@)" "!@) "
            (foreach (w obj)
              (if (filter "!@(%!@)" w)
                  (.. (subst "!@" "" w) " !.")
                  w)))))

  (if (filter "!@(%!@)" obj)
      RECUR
      obj))


(define (protect-comma str)
  (if (findstring "!@," str)
      (.. "$(if ,," (subst "!@" "" str) ")")
      str))


;; Escape "(", ",", and ")" outside of balanced parentheses.
;;
(define `(escape-unnested str)
  (promote
   (protect-comma
    (subst " " ""
           "!@)" "$]"
           "!@(" "$["
           (clear-nested (subst "," "!@,"
                                "(" " !@("
                                ")" "!@) "
                                [str]))))))


(define (protect-arg str)
  &public
  (if (or (findstring "(" str)
          (findstring "," str)
          (findstring ")" str))
      (escape-unnested str)
      str))


;; Escape single-line expression
;;
;;  - encode newlines
;;
;; Interestingly, "#" characters must not be escaped.
;;
(define `(protect-expr str)
  &public
  (replace-nl str))


;; Escape LHS of "=" or ":=" assignment
;;
;;  - replace "#" with alternative
;;  - protect "=", ":", leading space, trailing space, and keywords
;;  - encode newlines
;;
(define (protect-lhs str)
  &public
  (define `keywords
    (._. "ifeq ifneq ifdef ifndef else endif define endef override"
         "include sinclude -include export unexport private undefine vpath"))

  (replace-hash
   (subst "X" (replace-nl (protect-arg str))
          (if (or (findstring ":" str)
                  (findstring "=" str)
                  (not (findstring str (wordlist 1 99999999 str)))
                  (filter keywords str))
              "$(if ,,X)"
              "X"))))


;; Double all backslash characters immediately preceding ".#", and then
;; remove all ".#".
;;
(define (double-bs str)
  (if (findstring "\\.#" str)
      (double-bs (subst "\\.#" ".#\\\\" str))
    (subst ".#" "" str)))


;; Escape RHS of "=" or ":=" assignment
;;
;;  - escape literal "#" with backslashes
;;  - protect trailing "\" with a "#" comment to avoid [1]
;;  - double backslashes immediately preceding comment or \#
;;  - protect leading space
;;  - encode newlines
;;
;; [1]: Make has quirky handling of one or more \ at the end of a line:
;;        0 --> 0;   1 --> 0;  2 --> 2;  3 --> 1 + space;  4 --> 4
;;
(define (protect-rhs str)
  &public
  (define `str-esc
    (subst "#" ".#\\#" str))

  (define `str-bs
    ;; Assert: ".#.#" cannot appear within str-esc
    (subst "\\.#.#" ".#\\\\#" (.. str-esc ".#.#")))

  (define `hash-bs
    (if (or (findstring "#" str)
            (findstring "\\" str))
        (double-bs str-bs)
        str))

  (protect-ltrim (replace-nl hash-bs)))


;; Escape body of "define ... endef" statement
;;
;;  - protect "define" and "endef" when they appear at the start of a line
;;  - protect "\\" when it appears at the end of a line
;;
;; This function conservatively prefixes every 'define' and 'endef' with '$ '.
;;
(define (protect-define str)
  &public
  (if (or (findstring "define" str)
          (findstring "endef" str)
          (findstring "\\" str))
      (begin
        (define `(protect-line line)
          (.. (if (filter "define endef" (word 1 line))
                  "$ ")
              line
              (if (filter "%\\" [line])
                  "$ ")))

        (concat-for (w (split "\n" str) "\n")
          (protect-line w)))
      str))
