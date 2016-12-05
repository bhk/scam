;;--------------------------------------------------------------
;; parse : parsing
;;--------------------------------------------------------------
;;
;; Parse tree (AST) is a tree of "forms".  Each form is one of:
;;
;;   Q.<n> str       ==>  string:  "str"
;;   S.<n> str       ==>  symbol:  str
;;   L.<n> <node>... ==>  list:  (<node>...)
;;   E.<n> <code>    ==>  error; always at top level of AST
;;   '.<n> <node>    ==>  quoted expression
;;   `.<n> <node>    ==>  quasi-quoted expression
;;   ,.<n> <node>    ==>  unquoted expression
;;   @.<n> <node>    ==>  unquote-splicing expression
;;
;; <n> = word index at which form began
;;
;; Error code is one of:
;;   .       ==> at end of string/no more expressions to parse
;;   "       ==> unterminated string; <n> at open quote
;;   ) or ]  ==> unmatched close; <n> at unmatched char
;;   ( or [  ==> unmatched open, waiting for more.  <n> at unmatched char
;;   ` or ,  ==> quote/unquote without following expression
;;
;; Code may be followed by one or more ")" or "]" characters indicating
;; nesting when error was encountered.

(require "core")

;;==============================================================
;; Form value accessors

(define `(symbol? form)
  (type? "S%" form))

(define `(string? form)
  (type? "Q%" form))

(define `(list? form)
  (type? "L%" form))

(define `(error? form)
  (type? "E%" form))

(define `(quoted? form)
  (type? "'%" form))

(define `(qquoted? form)
  (type? "`%" form))

(define `(unquoted? form)
  (type? ",%" form))

(define `(sunquoted? form)
  (type? "@%" form))

(define form-types
  (hash-bind "L" "list"
             (hash-bind "Q" "literal string"
                        (hash-bind "S" "symbol"))))

(define (form-typename form)
  (or (hash-get (word 1 form) form-types)
      "invalid form"))

(define (form-index form)
  (word 2 (subst "." " " (word 1 form))))


(declare (format-form))

;; form-assert should be called with "FILE:LINE:" (via macros)
;; to highlight problem line of code
(define (form-assert type form msg)
  (if (filter (concat type "%") (word 1 form))
      form
    (error (concat msg ": expected " (form-typename type)
                   ", got " (form-typename form)
                   ":\n" (format-form form) "\n"))))

(define (string-value form)
  &inline
  (nth 2 form))

(define (symbol-name form)
  &inline
  (nth 2 form))

;; These versions might be appropriate for use with user-defined macros.
;;
;;(define (string-value form msg)
;;  (nth 2 (form-assert "Q" form (or msg "string-value"))))
;;
;;(define (symbol-name form msg)
;;  (nth 2 (form-assert "S" form (or msg "symbol-name"))))
;;
;;(define (list-nth index form msg)
;;  (nth (1+ index) (form-assert "L" form (or msg "list-nth"))))

;; convert symbol form to string form
(define (symbol-to-string form)
  &inline
  (concat "Q " (word 2 form)))


;; Convert form (AST node) back to printed representation (suitable for
;; parsing if input tree was valid).  Does not rigorously validate.
;;
(define (format-form form)
  (cond ((list? form)   (concat "(" (concat-for f (rest form) " "
                                                (format-form f)) ")"))
        ((string? form) (format (string-value form)))
        ((symbol? form) (symbol-name form))
        ((quoted? form) (concat "'" (format-form (nth 2 form))))
        ((qquoted? form) (concat "`" (format-form (nth 2 form))))
        (else           (concat "," (format form)))))

;;--------------------------------------------------------------
;; Subject string encoding
;;
;; GNU Make provides no way to index a string by character or byte offset,
;; so we encode the string in a way that allows the `word` builtin to
;; address lexemes -- we surround syntactically significant substrings with
;; spaces.  The first step is a `demote` operation, so spaces and tabs will
;; be preserved (so that error messages can display the location of an error
;; in the context of the original line).  We call the encoded form the
;; "subject" string.
;;
;; This encoding is performed once, and parsing operations operate on
;; the encoded string *many* times, so the encoding step is designed to
;; minimize the word count and the overall size of the subject string.
;;
;; Before `"` is surrounded by spaces, the \" sequence is converted to a
;; special substring -- !Q -- in order to simplify parsing literal strings.
;; This means that `\\` must be first be similarly processed so that `\\"`
;; will not be misinterpreted.
;;
;; Spaces (`!0`) are normally tokens -- isolated as words -- but spaces that
;; follow other spaces or newlines are collapsed into a single word, since
;; these are syntactically equivalent.
;;
;; Consecutive spaces are then compressed ( !0!0 --> !2! ).  This
;; compression must not change the initial character (e.g. "!0") since that
;; is used to identify the type of the lexeme.
;;

;; Collapse spaces following a ";" up to the next "\n" or "\""
(define (compact-comments str)
  &private
  (subst " " "" "!s" " " "; ;" ";;"
         (foreach c (subst " " "!s" "\"" " \"" "\n" " \n" ";" " ;" str)
                  (if (filter ";%" c)
                      (concat (subst "!s" "" c) "!s")
                      c))))

;; When compressing, don't replace the initial "!0" character in a word
;; (which identifies its type to the parser).
(define (compress-spaces str)
  &private
  (subst "\n !0" "\n!0" "\n!0!0" "\n!2" "!0!0!0" "!0!2" "!2!0!2!0" "!6" "!6!6" "!c"
         str))


(define (expand-spaces str)
  &private
  (subst "!c" "!6!6" "!6" "!2!2!2" "!2" "!0!0" str))

;; Use "~" for spaces, and prefix tab and newline with "~"
;;   (foreach w (if (filter "!0% \n% !+%" w) (concat "!0" (subst "!0" "~" (patbust "!0%" "%")))))
;;   subst
;;     ~~~~ !4
;;     !4!4 !8
;;     !8!8 !a
;;     !0 ~

(define (penc text)
  (compress-spaces
   (compact-comments
    (subst "," " , " ", @" ",@ " "`" " ` " "'" " ' " "\\\\" "!b" "\\\"" "!Q"
           ";" " ; " "!0" " !0 " "\n" " \n " "\"" " \" " "]" " ] " "[" " [ "
           ")" " ) " "(" " ( " "0  !" "0!" "  " " "
           ["\t"] (concat " " [" \t"] " ")
           (if text (demote text))))))

;; pdec: undo penc
(define (pdec text)
  (promote
   (expand-spaces
    (subst [" \t"] ["\t"] " " "" "!Q" "\\\"" "!b" "\\\\" text))))


;; pdec-str: undo penc and also process backslash sequences [e.g.: \" -> " ]
;; returning the demoted form of the string.
;;
(define (pdec-str text)
  &private
  (or (expand-spaces
       (subst [" \t"] ["\t"] " " "" "\\t" "!+" "\\n" "\n" "!Q" "\"" "!b" "\\" text))
      "!."))


;; return index of next word that matches pat, starting at pos
;; Check three words at a time to improve speed.
(define (find-word str pos pat)
  (foreach p (1+ (1+ pos))
           (if (filter pat (or (wordlist pos p str) pat))
               (if (filter pat (or (word pos str) pat))
                   pos
                   (foreach q (1+ pos)
                            (if (filter pat (or (word q str) pat))
                                q
                                p)))
               (find-word str (1+ p) pat))))


;;---- Parsing functions ----
;;
;; (parse-XXX str pos) --> "POS NODE"

(declare (parse-exp))

(define (new-Q str start end)
  &private
  (if (word end str)
      (concat end " Q." start " " (pdec-str (subst "\"" "" (wordlist start end str))))
      (concat end " E." start " \"")))



;; parse-seq reads a sequence of expressions, stopping at a matching end
;; token: ")" or "]".
;;
;; Instead of checking for the end token before parsing an expression, we
;; first try to parse an expression, expecting "]" or ")" to result in a
;; "mismatched end token" parse error.  When the parse "error" matches our
;; end token, we consider it success.  [Checking for the end token BEFORE
;; parsing an expression would require skipping whitespace and comments
;; first -- currently this is performed as part of `parse`.  Refactoring the
;; parser to move `skipwhite` logic outside of `parse` reduced performance.]
;;
;; One wrinkle to be aware of is that we may encounter an error at a
;; matching end token but it still might not be a valid terminating token:
;;
;;      [ 1 ( 2 ]
;;
;; Here, when parsing the "(" sequence we get "E x ]", which causes it to
;; fail with "E x ] )".  The instance of parse-seq that is reading the "["
;; sequence then sees an error at "]", but it should not treat it as valid
;; -- it needs to inspect the third element of the vector to determine it
;; was an error returned from parse-seq itself, not a "simple" parse error.

(define (parse-seq-err endch succ item)
  &private
  (if (filter "." (word 3 item))
      ;; unmatched at EOF => unterminated
      (concat (word 1 succ) " E." (word 1 succ) " "
              (subst ")" "(" "]" "[" endch))
      ;; mismatched end, etc.
      (concat item " " endch)))

(define (parse-seq str endch succ item lst)
  &private
  (if (filter "E%" (word 2 item))
      (if (and (filter endch (word 3 item))
               (not (word 4 item)))
          ;; matching end character
          (concat (word 1 item) " L." succ " " lst)
          ;; mis-matched/unmatched
          (parse-seq-err endch succ item))
      (parse-seq str endch succ
                 (parse-exp str (1+ (word 1 item)))
                 (if (word 2 item) (conj lst (rest item))))))


(define (parse-array str pos)
  &private
  (parse-seq str "]" (concat pos " S!0vector") pos))


;; quote, backquote, unquote

(define (parse-x2 w pos result)
  &private
  (if (filter "E%" (word 2 result))
      (if (filter "." (word 3 result))
          (concat pos " E." pos " " w)
          result)
      (append (word 1 result)
              (concat (or (filter "' ` , @" (patsubst ",@" "@" w))
                          (error ("Impossible x2: " w)))
                      "."
                      pos
                      " "
                      [ (rest result) ]))))


(define (parse-x w str pos)
  &private
  (if (filter "!0% \n% ;%" (word (1+ pos) str))
      ;; invalid location for "'", "`", ",", or ",@"
      (concat pos " E." pos " " w)
    (parse-x2 w pos (parse-exp str (1+ pos)))))


;; parse-exp:  Parse one expression; return "POS NODE"
;;
;;  POS = word index of last word in the expression
;;  NODE = AST node: TYPE " " VALUE
;;
;; Use `foreach` as a cheaper form of `let` (when dealing with individual
;; words)

(define (parse-exp str pos)
  (or
   (foreach
    w (word pos str)
    (cond ((filter "!0% \n%" w)  (parse-exp str (1+ pos)))
          ((filter ") ]" w)      (concat pos " E." pos " " w))
          ((filter "(" w)        (parse-seq str ")" pos pos))
          ((filter "\"" w)       (new-Q str pos (find-word str (1+ pos) "\"")))
          ((filter ";%" w)       (parse-exp str (1+ (find-word str pos "\n%"))))
          ((filter "[" w)        (parse-array str pos))
          ((filter "' ` , ,@" w) (parse-x w str pos))
          ((isnumber w)          (concat pos " Q." pos " " w))
          (else                  (concat pos " S." pos " " w))))
   (concat pos " E." pos " .")))


;;--------------------------------
;; Parse error diagnostics
;;--------------------------------


;; Return the line number in which `form` occurs in `subj`
;;
(define (describe-lnum pos subj)
  (define `pre (wordlist 2 (or pos 1) (concat "x" subj)))
  (words (concat "1 " (filter "\n" (subst "\n" "\n " pre)))))


;; Return the line containing `form` as a vector: [pre tok post]
;;   tok = the token `form` corresponds to
;;   pre = text preceding tok on the line
;;   post = text following tok
;;
(define (describe-line pos subj)
  &private
  (define `ndx (or pos 1))
  (let ((pre  (last (split "\n" (wordlist 2 ndx (concat "X " subj)))))
        (post (first (split "\n" (nth-rest ndx subj)))))
    [ (pdec pre)
      (pdec (word 1 post))
      (pdec (rest post)) ]))


;; return description line, given error code or description string
;;
(define (get-error-desc code)
  &private
  (cond ((filter "` '" code)
         (concat "prefix \"" code "\" must immediately precede expression"))

        ((filter "( ) [ ]" code)
         (concat "unmatched \"" code "\""))

        ((filter "\"" code)
         "unterminated string")

        (else code)))


;; Is n a natural number? (valid for `word` and `wordlist`?)
;;
(define (natural? n)
  &private
  (isnumber (subst "E" "~" "e" "~" "-" "~" "." "~" "0" "" n)))


;; Construct error message, given error form and source text
;;     FILE:LINE: DESCRIPTION
;;     at: PRE*TOK*POST
(define (describe-error form text filename)
  (if (not (and (error? form)
                (natural? (or (form-index form) 1))))
      "(internal error: bad error form)\n"

      (let ((subj (penc text))
            (desc (get-error-desc (nth 2 form)))
            (file (if filename
                      (concat filename ":")
                      "line "))
            (pos (form-index form)))

        (define `lnum (describe-lnum pos subj))
        (define `ltext (describe-line pos subj))
        (concat file lnum ": " desc "\n"
                "at: " (concat-vec ltext "*") "\n"))))


(define (parse-subj-r subj o lst)
  &private
  (if (filter "E%" (word 2 o))
      (if (filter "." (word 3 o))
          lst                      ; EOF
          (conj lst (rest o)))     ; error
      (parse-subj-r subj
                    (parse-exp subj (1+ (word 1 o)))
                    (conj lst (rest o)))))


;; Parse all sexps in `subject`.  Return vector of forms.
;; `subject` is the penc-encoded form of the original text.
;;
(define (parse-subject subj)
  (parse-subj-r subj (parse-exp subj 1)))

(define (parse-text text)
  (parse-subject (penc text)))
