;;--------------------------------------------------------------
;; parse : parsing
;;--------------------------------------------------------------

(require "core")

;; These records, called "forms", describe the parse tree (AST):

(data P
      &public
      (PList    &word n &list forms)  ; (FORMS...)
      (PString  &word n value)        ; quoted string or number
      (PSymbol  &word n value)        ; identifier
      (PDict    &word n &list pairs)  ; { ... }
      (PQuote   &word n &list form)   ; quoted expression (syntax)
      (PQQuote  &word n &list form)   ; quasi-quoted expression
      (PUnquote &word n &list form)   ; unquoted expression
      (PSplice  &word n &list form)   ; unquoted splice expression
      (PError   &word n desc))        ; parsing error

;; N = word index at which form began
;;
;; The first word of DESC in PError is one of the following:
;;
;;   .       ==> at end of string/no more expressions to parse.
;;   "       ==> un-terminated string; N is at open quote.
;;   (, [, { ==> un-terminated form; N is at start.
;;   ), ], } ==> unbalanced terminator; N is at the terminator.
;;   ` or ,  ==> quote/unquote without following expression.
;;   :?      ==> missing expected ":" in a dictionary
;;   v?      ==> missing expected value for dictionary entry
;;   :       ==> unexpected ":" token
;;
;; If the error occurred within a nested context, the first word of DESC
;; will be followed by one or more ")", "]", or "}".  This allows us to
;; distinguish a non-expression token (e.g.  a terminator) that was
;; immediately encountered (where it might be valid) from the same token
;; found in a nested context where it was not valid.

(define (symbol-name form)
  &public
  (case form
    ((PSymbol n value) value)
    (else (concat "ERROR:symbol-name(" form ")"))))

;; convert symbol form to string form
(define (symbol-to-string form)
  &public
  (case form
    ((PSymbol n value) (PString n value))
    (else (concat "ERROR:symbol-to-string(" form ")"))))

(define (string-value form)
  &public
  (case form
    ((PString n value) value)
    (else (concat "ERROR:string-value(" form ")"))))

(define (PList-is-empty? form)
  &public
  (case form
    ((PList n forms) (not forms))
    (else (concat "ERROR:PList.is-empty?(" form ")"))))

(define (form-index form)
  &public
  (if (filter "!:%" (word 1 form))
      (word 2 form)
      (if (numeric? form)
          form
          0)))

(define (form-typename form)
  &public
  (case form
    ((PList n value) "list")
    ((PSymbol n value) "symbol")
    ((PString n value) "literal string")
    (_ "invalid form")))

;; Set all form positions to POS.
;;
(define (form-set-indices pos form)
  &public
  (define `(recur f)
    (form-set-indices pos f))

  (if form
      (case form
        ((PString  n v) (PString pos v))
        ((PSymbol  n v) (PSymbol pos v))
        ((PError   n v) (PError pos v))
        ((PList    n subs) (PList pos (for f subs (recur f))))
        ((PQuote   n sub) (PQuote pos (recur sub)))
        ((PQQuote  n sub) (PQQuote pos (recur sub)))
        ((PUnquote n sub) (PUnquote pos (recur sub)))
        ((PSplice  n sub) (PSplice pos (recur sub)))
        (else (concat "ERROR:form-set-indices(" form ")")))))


;; End Type internals
;;----------------------------------------------------------------


;; Convert form (AST node) back to printed representation (suitable for
;; parsing if input tree was valid).  Does not rigorously validate.
;;
(define (format-form form)
  &public
  (case form
    ((PList n forms)
     (concat "(" (concat-for f forms " " (format-form f)) ")"))
    ((PString n value) (format value))
    ((PSymbol n value) value)
    ((PQuote n frm) (concat "'" (format-form frm)))
    ((PQQuote n frm) (concat "`" (format-form frm)))
    (else (concat "," (format form)))))


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
  (subst " " "" "!s" " " "; ;" ";;"
         (foreach c (subst " " "!s" "\"" " \"" "\n" " \n" ";" " ;" str)
                  (if (filter ";%" c)
                      (concat (subst "!s" "" c) "!s")
                      c))))

;; When compressing, don't replace the initial "!0" character in a word
;; (which identifies its type to the parser).
(define (compress-spaces str)
  (subst "\n !0" "\n!0" "\n!0!0" "\n!2" "!0!0!0" "!0!2" "!2!0!2!0" "!6" "!6!6" "!c"
         str))


(define (expand-spaces str)
  (subst "!c" "!6!6" "!6" "!2!2!2" "!2" "!0!0" str))


(define (penc text)
  &public
  (compress-spaces
   (compact-comments
    (subst "," " , " ", @" ",@ " "`" " ` " "'" " ' " "\\\\" "!b" "\\\"" "!Q"
           ";" " ; " "!0" " !0 " "\n" " \n " "\"" " \" " "]" " ] " "[" " [ "
           "{" " { " "}" " } " ")" " ) " "(" " ( " "$" " $ " ":" " : "
           "%" " !p " "0  !" "0!" "  " " "
           ["\t"] (concat " " [" \t"] " ")
           (if text (demote text))))))

;; Undo `penc`.
;;
(define (pdec text)
  &public
  (promote
   (expand-spaces
    (subst [" \t"] ["\t"] " " "" "!Q" "\\\"" "!b" "\\\\" "!p" "%" text))))


;; Undo `penc` and also process backslash sequences [e.g.: \" -> " ],
;; returning the demoted form of the string.
;;
(define (pdec-str text)
  (or (expand-spaces
       (subst [" \t"] ["\t"] " " "" "\\t" "!+" "\\n" "\n" "!Q" "\"" "!b" "\\" "!p" "%" text))
      "!."))


;; Return index of next word that matches PAT, starting at POS.
;; Check three words at a time to improve speed.
;;
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


;; Construct a parse function result.
;;
;;   POS = position of the last token matched
;;   FORM = instatce `data P` type.
;;
(define `(POut pos form)
  (concat pos " " form))

(define `(POut-pos st)
  (word 1 st))

(define `(POut-form st)
  (rest st))

(define (POut-format value)
  (if (and (numeric? (word 1 value))
           (filter "!:%" (word 2 value)))
      (concat "(POut " (word 1 value) " " (format (rest value)) ")")))


(declare (parse-exp subj pos))


;; Extract literal string value, or generate "unterminated quote" error.
;;
(define (new-Q str start end)
  (concat end " "
          (if (word end str)
              (PString start (promote (pdec-str (subst "\"" "" (wordlist start end str)))))
              (PError start "\""))))


;; parse-seq
;;
;; Read a sequence of expressions, stopping at an "end token" --")" or "]"
;; -- that matches the "(" or "[" that opened the sequence.
;;
;; SUBJ = subject string
;; TERM = terminating token: "]" or ")"
;; START-POS = position of the "(" or "["
;; OUT  = result of parsing the expression after those in LST
;; LST  = list of sub-forms already parsed

;; This is a separate function to keep this rare case outside the tight
;; loop.
(define (parse-seq-err term start-pos err-n err-desc)
  (if (filter "." err-desc)
      ;; EOF: unterminated sequence
      (POut start-pos (PError start-pos (subst ")" "(" "]" "[" term)))
      ;; other error
      (POut err-n (PError err-n (concat err-desc " " term)))))


(define (parse-seq subj term start-pos out lst)
  (case (POut-form out)
    ((PError n desc)
     ;; A ")" or "]" error closes this sequence UNLESS it is nested, as in:
     ;;     ( ... [ ... *)*
     ;; In such a case, desc will be ") [" and not ")".
     (if (eq? term desc)
         ;; Done (matching terminator)
         (POut (POut-pos out) (PList start-pos lst))
         ;; Error (mis-matched terminator)
         (parse-seq-err term start-pos n desc)))

    (else
     (parse-seq subj term start-pos
                (parse-exp subj (1+ (POut-pos out)))
                (conj lst (POut-form out))))))


(define `(parse-list subj pos)
  (parse-seq subj ")" pos (parse-exp subj (1+ pos)) nil))

(define (parse-array subj pos)
  (parse-seq subj "]" pos (parse-exp subj (1+ pos)) [(PSymbol 0 "vector")]))


;; parse-dict


;; Advance to next "significant" word in SUBJ.  Return (concat POS " " WORD).
(define (parse-skip subj pos)
  (if (filter "!0% \n% ;%" (word pos subj))
      (parse-skip subj (1+ pos))
      (concat pos " " (word pos subj))))


(declare (parse-dict-1 subj start-pos pairs out))
(declare (parse-dict-2 subj start-pos pairs key out))
(declare (parse-dict-3 subj start-pos pairs key out))
(declare (parse-dict-4 subj start-pos pairs out))

(define (parse-dict subj pos)
  (parse-dict-1 subj pos nil (parse-exp subj (1+ pos))))

;; EOF-CODE = error code to report when EOF is encountered.
;; EOF-POS = pos to place in PError when EOF is encountered.
;; OK-CODES = pattern describing error codes to ignore
;; START-POS =
(define (parse-dict-error pout ok-codes start-pos)
  (case (POut-form pout)
    ((PError n desc)
     (if (eq? "." desc)
         (POut n (PError start-pos "{"))
         (if (not (filter ok-codes [desc]))
             (POut n (PError n (concat desc " }"))))))))

;; expect KEY or "}"
(define (parse-dict-1 subj start-pos pairs out)
  (define `pos (POut-pos out))
  (define `form (POut-form out))
  (or (parse-dict-error out "}" start-pos)
      (case form
        ((PError n desc)
         (POut pos (PDict start-pos pairs)))
        (else
         (parse-dict-2 subj start-pos pairs form (parse-exp subj (1+ pos)))))))

;; expect ":"
(define (parse-dict-2 subj start-pos pairs key out)
  (define `pos (POut-pos out))
  (define `form (POut-form out))
  (or (parse-dict-error out ": }" start-pos)
      (case form
        ((PError n desc)
         (if (eq? desc "}")
             (POut n (PError n ":?")))))
      (parse-dict-3 subj start-pos pairs key (parse-exp subj (1+ pos)))))

;; expect VALUE
(define (parse-dict-3 subj start-pos pairs key out)
  (define `pos (POut-pos out))
  (define `form (POut-form out))
  (or (parse-dict-error out "}" start-pos)
      (case form
        ((PError n desc)
         (if (eq? desc "}")
             (POut n (PError n "v?")))))
      (parse-dict-4 subj start-pos (append pairs {(or key): form})
                    (parse-skip subj (1+ pos)))))

;; expect "," or "}"
(define (parse-dict-4 subj start-pos pairs out)
  (define `pos (word 1 out))
  (define `w (word 2 out))
  (parse-dict-1 subj start-pos pairs
                (parse-exp subj (if (filter "," w)
                                    (1+ pos)
                                    pos))))


;; parse-x : Handle quoting operators.
;;
;;   W = "`", "'", ",", or ",@"
;;   POS = position of W
;;   OUT = result of parsing expression following W
;;
(define (parse-x2 w pos out)
  (define `form-ctor
    (cond ((filter "'" w)  PQuote)
          ((filter "`" w)  PQQuote)
          ((filter "," w)  PUnquote)
          ((filter ",@" w) PSplice)
          (else (lambda () (PError pos (concat "internal:parse-x2:" w))))))

  (case (POut-form out)
    ((PError n desc) out)
    (else (POut (POut-pos out)
                (form-ctor pos (POut-form out))))))


(define (parse-x w subj pos)
  (if (filter "!0% \n% ;% ()" (or (word (1+ pos) subj) "()"))
      ;; quoting tokens must immediately precede an expression
      (POut pos (PError pos w))
    (parse-x2 w pos (parse-exp subj (1+ pos)))))


;; parse-exp:  Parse one expression; return "POS NODE"
;;
;;  POS = word index of last word in the expression
;;  NODE = AST node: TYPE " " VALUE
;;
;; Use `foreach` as a cheaper form of `let` (when dealing with individual
;; words)

(define (parse-exp subj pos)
  (or
   (foreach
    w (word pos subj)
    (cond ((filter "!0% \n%" w)  (parse-exp subj (1+ pos)))
          ((filter ") ] }" w)    (POut pos (PError pos w)))
          ((filter "(" w)        (parse-list subj pos))
          ((filter "\"" w)       (new-Q subj pos (find-word subj (1+ pos) "\"")))
          ((filter ";%" w)       (parse-exp subj (1+ (find-word subj pos "\n%"))))
          ((filter "[" w)        (parse-array subj pos))
          ((filter "{" w)        (parse-dict subj pos))
          ((filter "' ` , ,@" w) (parse-x w subj pos))
          ((numeric? w)          (POut pos (PString pos w)))
          ((filter "$ : !p" w)   (POut pos (PError pos (pdec w))))
          (else                  (POut pos (PSymbol pos (promote w))))))
   (POut pos (PError pos "."))))


;;--------------------------------
;; Parse error diagnostics
;;--------------------------------


;; Return the line number for the token at index POS in SUBJ.
;;
(define (describe-lnum pos subj)
  &public
  (define `pre (wordlist 2 (or pos 1) (concat "x" subj)))
  (words (concat "1 " (filter "\n" (subst "\n" "\n " pre)))))


;; Return the line containing the token at index POS as a vector:
;; [PRE TOK POST]
;;   TOK = the token at index POS
;;   PRE = text preceding tok on the line
;;   POST = text following tok
;;
(define (describe-line pos subj)
  (define `ndx (or pos 1))
  (let ((pre  (last (split "\n" (wordlist 2 ndx (concat "X " subj)))))
        (post (first (split "\n" (nth-rest ndx subj)))))
    [ (pdec pre)
      (pdec (word 1 post))
      (pdec (rest post)) ]))


;; Return description line, given error code or description string.
;;
(define (get-error-msg desc)
  (define `code
    (word 1 desc))

  (cond ((filter "` '" code)
         (concat "prefix \"" code "\" must immediately precede expression"))

        ((filter "( ) [ ] { }" code)
         (concat "unmatched \"" code "\""))

        ((filter "\"" code)
         "unterminated string")

        ((filter ":" code)
         "saw \":\" where not expected")

        ((filter ":?" code)
         "expected \": VALUE\" following dictionary key")

        ((filter "v?" code)
         "expected value following dictionary \"KEY:\"")

        (else desc)))


;; Construct error message, given error form and source text
;;     FILE:LINE: DESCRIPTION
;;     at: PRE*TOK*POST
;;
(define (describe-error form text filename)
  &public
  (case form
    ((PError pos desc)
      (let ((subj (penc text))
            (file (if filename
                      (concat filename ":")
                      "line "))
            (msg (get-error-msg desc))
            (pos pos))
        (if (natural? pos)
            (sprintf "%s%s: %s\nat: %s\n"
                     file (describe-lnum pos subj) msg
                     (concat-vec (describe-line pos subj) "*"))
            (sprintf "%s:?: %s\n"
                     file msg))))))

;; (parse-forms subj pos k) -->  (k form-list err)
;;    ERR = nil if sequence ended at EOF,  (PError ...) otherwise.
;;
(define (parse-forms-r subj k o form-list)
  (define `form (POut-form o))
  (define `pos (POut-pos o))

  (case form
    ((PError n desc)
     (k form-list (if (eq? desc ".") nil form)))

    (else (parse-forms-r subj k
                         (parse-exp subj (1+ pos))
                         (conj form-list form)))))

(define `(parse-forms subj pos k)
  (parse-forms-r subj k (parse-exp subj pos) nil))


;; Parse all sexps in `subject`.  Return vector of forms.
;; `subject` is the penc-encoded form of the original text.
;;
(define (parse-subject subj)
  &public
  (parse-forms subj 1 (lambda (form-list err)
                        (if err
                            (conj form-list err)
                            form-list))))

(define (parse-text text)
  &public
  (parse-subject (penc text)))
