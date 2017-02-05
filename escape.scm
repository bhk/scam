;;--------------------------------------------------------------
;; escape : escaping
;;--------------------------------------------------------------

(require "core")

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
(define (escape str)
  &public
  &inline
  (subst "$" "$`" str))


;; Double all backslash characters preceding "!D", and then remove "!D"
(define (protect-hash2 str)
  (if (findstring "\\.#" str)
      (protect-hash2 (subst "\\.#" ".#\\\\" str))
    (subst ".#" "" str)))


;; Quote hash ("#") characters with backslash:
;;    #  -->    \#
;;   \#  -->  \\\#
(define (quote-hash str)
  (if (findstring "#" str)
      (protect-hash2 (subst "#" ".#\\#" str))
    str))

(define (replace-nl str)
  &inline
  (subst "\n" "$'" str))

(define (replace-hash str)
  &inline
  (subst "#" "$\"" str))


;; Prevent leading spaces from being trimmed.
;;
(define (protect-ltrim str)
  &public
  (define `(begins-white str)
    (findstring (word 1 (concat 0 str 0)) 0))
  (concat (if (begins-white str) "$ ") str))


;; Prevent leading and trailing whitespace from being trimmed by enclosing
;; in "$(if ,,...)".
;;
(define (protect-trim s)
  &public
  (if (and (findstring s (wordlist 1 99999999 s))
           (filter-out "\n%" (word 1 s))
           (filter-out "%\n" (lastword s)))
      s
      (if s
          (concat "$(if ,," s ")"))))


;; `balance-match` operates on a demoted string in which "(" and )" have
;; been converted to "!L" and "!R" (each surrounded by spaces).  It replaces
;; balanced pairs of "!L" and "!R" with "(" and ")".  Any occurrences of
;; "!C" enclosed by "(" and ")" are eliminated.  The caller may handle any
;; remaining "!L", "!R", or "!C" as necessary.

(define (balance2 e)
  (promote (if (findstring "!C" e)
               (concat "$(if ,," (subst "!C" "" e) ")")
             e)))

;; `stack` is a list of words -- one for each unmatched !L plus one (the
;; first (which never begins with !L).
;;
;; If we see another !L, we push another word on the stack.
;; If we see a !R, we pop the last word and append "(<content>)" to the previous.
;;
(define (balance-match-r w str stack)
  (if w
      (balance-match-r (word 1 str)
           (rest str)
           (cond
            ;; !L
            ((filter "!L%" w)
             (concat stack " " w))

            ;; !R matching !L
            ((and (filter "!R" w)
                  (word 2 stack))

             ;; butlast is a bit ugly
             (let& ((paired (concat "("
                                    (subst "!C" "" "!L" ""
                                           (lastword stack))
                                    ")")))
                 ;; butlast is inefficient; we know that stack contents are
                 ;; word-encoded, so we can do this:
                 (concat (filter-out "%!" (concat stack "!")) paired)))
            ;; other
            (else (concat stack w))))
      stack))


(define (balance-match str)
  (balance-match-r (word 1 str) (rest str) "!."))


(define (balance str)
  (balance2
   (subst " " "" "!R" "$]" "!L" "$["
          (balance-match (subst "," "!C," ")" " !R " "(" " !L"
                                (demote str))))))


;; check-balance: find matched parentheses, removing them and their contents
;; from the string.  The returned string describes the remainder:
;;    If it contains "!" there was an unmatched paren.
;;    If it contains "," there was a comma outside of matched parens.
;;
;; The algorithm splits the string at each !R, making a list with one more
;; word than the number of !R's.  The first !R is then paired with the last
;; !L and removed from the list.  Any text preceding the !L is prepended to
;; the next word in the list.
;;
(define (check-balance-r str)
  (if (word 2 str)
      (check-balance-r
       (concat (subst " " "" (filter-out "!L%!R" (subst "!L" " !L" (word 1 str))))
               (rest str)))
      str))

(define (check-balance str)
  (check-balance-r (subst " " "" "\t" "" "!" "" "(" "!L" ")" "!R ." str)))


;; Already balanced strings are the most common case, and checking balance
;; is easier than constructing a well-balanced string, so we check before
;; balancing.

(define (make-balanced str chk)
  (if (findstring "!" chk)
      (balance str)
      (if (findstring "," chk)
          (concat "$(if ,," str ")")
          str)))


;; Escape argument to a function
;;
;;  - encode unbalanced parentheses
;;  - encode "," except where enclosed in balanced (unencoded) parens
;;
(define (protect-arg str)
  &public
  (if (or (findstring "(" str)
          (findstring ")" str)
          (findstring "," str))
      (make-balanced str (check-balance str))
      str))


;; Escape single-line expression
;;
;;  - encode newlines
;;
;; Interestingly, "#" characters must not be quoted.
;;
(define (protect-expr str)
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
    (concat "ifeq ifneq ifdef ifndef else endif define endef override "
            "include sinclude -include export unexport private undefine vpath"))

  (replace-hash
   (subst "X" (replace-nl (protect-arg str))
          (if (or (findstring ":" str)
                  (findstring "=" str)
                  (not (findstring str (wordlist 1 99999999 str)))
                  (filter keywords str))
              "$(if ,,X)"
              "X"))))


;; Escape RHS of "=" or ":=" assignment
;;
;;  - escape "#" with backslashes
;;  - protect leading space
;;  - encode newlines
;;
(define (protect-rhs str)
  &public
  (quote-hash (protect-ltrim (replace-nl str))))


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
          (concat (if (filter "define endef" (word 1 line))
                      "$ ")
                  line
                  (if (filter "%\\" [line])
                      "$ ")))

        (concat-for w (split "\n" str) "\n"
                    (protect-line w)))
      str))
