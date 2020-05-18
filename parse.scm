;;--------------------------------------------------------------
;; parse : parsing
;;--------------------------------------------------------------

(require "core.scm")
(require "string.scm")

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
      (PError   &word n desc)         ; parsing error
      (PVec     &word n forms))       ; vector constructor


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
    (else (.. "ERROR:symbol-name(" form ")"))))

;; convert symbol form to string form
(define (symbol-to-string form)
  &public
  (case form
    ((PSymbol n value) (PString n value))
    (else (.. "ERROR:symbol-to-string(" form ")"))))

(define (string-value form)
  &public
  (case form
    ((PString n value) value)
    (else (.. "ERROR:string-value(" form ")"))))

(define (PList-is-empty? form)
  &public
  (case form
    ((PList n forms) (not forms))
    (else (.. "ERROR:PList.is-empty?(" form ")"))))

(define (form-index form)
  &public
  (case form
    ((PList    n forms) n)
    ((PString  n value) n)
    ((PSymbol  n value) n)
    ((PDict    n pairs) n)
    ((PQuote   n form) n)
    ((PQQuote  n form) n)
    ((PUnquote n form) n)
    ((PSplice  n form) n)
    ((PError   n desc) n)))

(define (form-typename form)
  &public
  (case form
    ((PList n value) "list")
    ((PSymbol n value) "symbol")
    ((PString n value) "literal string")
    (_ "invalid form")))


;; End Type internals
;;----------------------------------------------------------------


;; Convert form (AST node) back to printed representation (suitable for
;; parsing if input tree was valid).  Does not rigorously validate.
;;
(define (format-form form)
  &public
  (case form
    ((PList n forms)
     (.. "(" (concat-for (f forms) (format-form f)) ")"))
    ((PString n value) (format value))
    ((PSymbol n value) value)
    ((PQuote n frm) (.. "'" (format-form frm)))
    ((PQQuote n frm) (.. "`" (format-form frm)))
    (else (.. "," (format form)))))


;;--------------------------------------------------------------
;; Subject string encoding
;;
;; We encode the text to be parsed as a sequence of words so that Make's
;; builtin `word` function can be used to address lexemes.  First we
;; word-encode the text, and then we surround lexically-significant
;; substrings with spaces.  The encoding is reversible so that we can
;; precisely locate the lexeme in the original source in error messages.
;;
;; Aside from word-encoding and space insertion, a few other transformations
;; are done:
;;
;;  - `\\` --> `!b`
;;  - `'"` --> `!Q`
;;  - `%`  --> `!p`
;;
;; Spaces (`!0`) are significant and therefore separated as words, but
;; spaces that follow other spaces or newlines are collapsed into a single
;; word, since these are syntactically equivalent.  Consecutive spaces are
;; then compressed ( !0!0 --> !2 ).  This compression must not change the
;; initial character (e.g. "!0") since that is used to identify the type of
;; the lexeme.
;;

;; Collapse spaces following a ";" up to the next `\n` or `"` or `\`.  When
;; a ";" occurs quotes, the rest of the line is ignored.  When inside
;; nothing except `\` or `"` will have any special handling.
;;
(define `(compact-comments str)
  (subst
   " " "" "!s" " " "; ;" ";;"
   (foreach (c (subst " " "!s" "\"" " \"" "\\" " \\" "\n" " \n" ";" " ;" str))
     (if (filter ";%" c)
         (.. (subst "!s" "" c) "!s")
         c))))

;; When compressing, don't replace the initial "!0" character in a word
;; (which identifies its type to the parser).
(define `(compress-spaces str)
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
           "\\" " \\" "%" " !p " ["\t"] (.. " " ["\t"] " ") "  " " "
           "!0 !0" "!0!0" "!0 !0" "!0!0"
           (if text (demote text))))))

;; Undo `penc`.
;;
(define (pdec text)
  &public
  (promote
   (expand-spaces
    (subst " " "" "!Q" "\\\"" "!b" "\\\\" "!p" "%" text))))


;; Undo `penc` and also process backslash sequences [e.g.: \" -> " ],
;; returning the demoted form of the string.
;;
(define `(pdec-str text)
  (or (expand-spaces (subst "!Q" "\"" "!b" "\\" "!p" "%" text))
      "!."))


;; Return index of next word that matches PAT, starting at POS.
;; Check three words at a time to improve speed.
;;
(define (find-word str pos pat)
  (foreach (p (1+ (1+ pos)))
    (if (filter pat (or (wordlist pos p str) pat))
        (if (filter pat (or (word pos str) pat))
            pos
            (foreach (q (1+ pos))
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
  (._. pos form))

(define `(POut-pos st)
  (word 1 st))

(define `(POut-form st)
  (rest st))

(declare (parse-exp subj pos))


;; parse-string: Parse string literals.
;;
(declare (parse-string subj start pos ?wstr))


;; Convert a hex digit to a word list of that length.
(define (hex-ticks h)
  (define `tail (word 2 (subst h " " "xFf:Ee:Dd:Cc:Bb:Aa:9:8:7:6:5:4:3:2:1:")))
  (filter ":" (subst ":" " : " tail)))


;; This makeshift arithmetic works only with numbers of limited size, but is
;; faster than the general-purpose algorithms in the math module.
;;
;; DIGITS = [H H], two hex digits (MSB-first)
;;
(define (hh-to-dec digits)
  (define `d1 (word 1 digits))
  (define `d2 (word 2 digits))
  (define `(tick+ a b) (._. a b))
  (define `(tick*16 a) (subst ":" ": : : : : : : : : : : : : : : :" a))
  (words (tick+ (tick*16 (hex-ticks d1)) (hex-ticks d2))))


;; Scan to end of string before returning error
(define `(PQError subj pos desc)
  (POut (find-word subj pos "\"") (PError pos desc)))


(define hex-digits
  "0 1 2 3 4 5 6 7 8 9 a b c d e f A B C D E F")


(define (parse-string-bs subj start pos wstr w)
  ;; If w matches "\xHH" this will contain one word: `H\nH`
  (define `match-hh
    (foreach (d1 hex-digits)
      (if (filter (.. "\\x" d1 "%") w)
          (foreach (d2 hex-digits)
            (if (filter (.. "\\x" d1 d2 "%") w)
                (.. d1 "\n" d2))))))

  (or (if (filter "\\n% \\t%" w)
          (parse-string subj start (1+ pos)
                        (.. wstr (subst "\\n" "\n" "\\t" ["\t"] w))))

      ;; Match `\xHH`
      (foreach (hh match-hh)
        (define `hex (subst "\n" "" hh))
        (define `byte (bytes-from-bytecodes (hh-to-dec (native-strip hh))))
        (parse-string subj start (1+ pos)
                      (.. wstr (subst (.. "\\x" hex) byte w))))

      (PQError subj pos "!B")))


;; START = pos of intial `"`
;; WSTR = accumulated string content so far (word-encoded)
;;
(define (parse-string subj start pos ?wstr)
  (or (foreach (w (word pos subj))
        (if (filter "\"" w)
            ;; Note: wstr may contain embedded "!." sequences, so it is not
            ;; properly vector-encoded (which `promote` expects).
            (POut pos (PString start (promote (pdec-str (subst "!." "" wstr)))))
            ;; Note the odd escaping required for `\%` with filter.
            (if (filter "\\\\%" w)
                (parse-string-bs subj start pos wstr w)
                (parse-string subj start (1+ pos) (.. wstr w)))))

      (POut pos (PError start "\""))))


;; parse-seq : Read a sequence of expressions, stopping at an end token
;;             that closes the "(" or "[" that opened the sequence.
;;
;; SUBJ = subject string
;; TERM = terminating token: "]" or ")"
;; START-POS = position of the "(" or "["
;; OUT  = result of parsing the expression after those in LST
;; LST  = list of sub-forms already parsed

;; This is a separate function to keep this rare case outside the tight
;; loop.
(define (parse-seq-err term start-pos err-n err-desc)
  (define `err-form
    (if (filter "." err-desc)
        ;; EOF: unterminated sequence
        (PError start-pos (subst ")" "(" "]" "[" term))
        ;; other error
        (PError err-n (._. err-desc term))))

  (POut err-n err-form))


(define (parse-seq subj term start-pos out lst ctor)
  (case (POut-form out)
    ((PError n desc)
     ;; A ")" or "]" error closes this sequence UNLESS it is nested, as in:
     ;;     ( ... [ ... *)*
     ;; In such a case, desc will be ") [" and not ")".
     (if (eq? term desc)
         ;; Done (matching terminator)
         (POut (POut-pos out) (ctor start-pos lst))
         ;; Error (mis-matched terminator)
         (parse-seq-err term start-pos n desc)))

    (else
     (parse-seq subj term start-pos
                (parse-exp subj (1+ (POut-pos out)))
                (conj lst (POut-form out))
                ctor))))


(define `(parse-list subj pos)
  (parse-seq subj ")" pos (parse-exp subj (1+ pos)) nil PList))

(define (parse-vector subj pos)
  (parse-seq subj "]" pos (parse-exp subj (1+ pos)) nil PVec))


;; parse-dict


;; Advance to next "significant" word in SUBJ.  Return (.. POS " " WORD).
(define (parse-skip subj pos)
  (if (filter "!0% !+% \n% ;%" (word pos subj))
      (parse-skip subj (1+ pos))
      pos))


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
             (POut n (PError n (.. desc " }"))))))))

;; expect KEY or "}"
(define (parse-dict-1 subj start-pos pairs out)
  (define `pos (POut-pos out))
  (define `form (POut-form out))
  (or (parse-dict-error out "}" start-pos)
      (case form
        ((PError n desc)
         (POut pos (PDict start-pos pairs)))
        (else
         (parse-dict-2 subj start-pos pairs form (parse-skip subj (1+ pos)))))))

;; expect ":"
(define (parse-dict-2 subj start-pos pairs key pos)
  (if (filter ":" (word pos subj))
      (parse-dict-3 subj start-pos pairs key (parse-exp subj (1+ pos)))
      (POut pos (PError pos ":?"))))

;; expect VALUE
(define (parse-dict-3 subj start-pos pairs key out)
  (define `pos (POut-pos out))
  (define `form (POut-form out))
  (or (parse-dict-error out "}" start-pos)
      (case form
        ((PError n desc)
         (if (eq? desc "}")
             (POut n (PError n "v?")))))
      (parse-dict-4 subj start-pos (append pairs { =key: form})
                    (parse-skip subj (1+ pos)))))

;; expect "," or "}"
(define (parse-dict-4 subj start-pos pairs pos)
  (define `w (word pos subj))
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
          (else (lambda () (PError pos (.. "internal:parse-x2:" w))))))

  (case (POut-form out)
    ((PError n desc) out)
    (else (POut (POut-pos out)
                (form-ctor pos (POut-form out))))))


(define (parse-x w subj pos)
  (if (filter "!0% !+% \n% ;% ()" (or (word (1+ pos) subj) "()"))
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
   (foreach (w (word pos subj))
     (cond ((filter "!0% !+% \n%" w)  (parse-exp subj (1+ pos)))
           ((filter ") ] }" w)    (POut pos (PError pos w)))
           ((filter "(" w)        (parse-list subj pos))
           ((filter "\"" w)       (parse-string subj pos (1+ pos)))
           ((filter ";%" w)       (parse-exp subj (1+ (find-word subj pos "\n%"))))
           ((filter "[" w)        (parse-vector subj pos))
           ((filter "{" w)        (parse-dict subj pos))
           ((filter "' ` , ,@" w) (parse-x w subj pos))
           ((numeric? w)          (POut pos (PString pos w)))
           ((filter "$ : !p" w)   (POut pos (PError pos (pdec w))))
           (else                  (POut pos (PSymbol pos (promote w))))))
   (POut pos (PError pos "."))))


;; Parse all sexps in `subject`.  Return vector of forms.
;; `subject` is the penc-encoded form of the original text.
;;
(define (parse-subject subj)
  &public
  (case (POut-form (parse-seq subj "." 0 (parse-exp subj 1) nil PList))
    ((PList pos lst) lst)
    (form [form])))


(define (parse-text text)
  &public
  (parse-subject (penc text)))


;;--------------------------------
;; Parse error diagnostics
;;--------------------------------


;; Get line number on which POS occurs in SUBJ.
;; If POS = 0 or nil, return 0.
;;
(define (get-subject-line pos subj)
  &public
  (words (filter "\n%" (wordlist 1 (or pos 1) (.. "\n " subj)))))


;; Get "LINE:COL" or POS in SUBJ.
;;
(define `(get-subject-line-col pos subj)
  ;; prefix lines with "\n" to handle POS when at start of line
  (let ((lines (subst " " "" "\n" " \n"
                      (wordlist 1 (or pos 1) (.. "\n " subj)))))
    (.. (words lines) ":" (string-len (pdec (lastword lines))))))


;; Return description line, given error code or description string.
;;
(define (get-error-msg desc)
  (define `code
    (word 1 desc))

  (cond ((filter "` '" code)
         (.. "prefix \"" code "\" must immediately precede expression"))

        ((filter "( ) [ ] { }" code)
         (.. "unmatched \"" code "\""))

        ((filter "\"" code)
         "unterminated string")

        ((filter ": ," code)
         "saw \":\" where not expected")

        ((filter ":?" code)
         "expected \": VALUE\" following dictionary key")

        ((filter "v?" code)
         "expected value following dictionary \"KEY:\"")

        ((filter "!B" code)
         "invalid backslash sequence in string")

        ;; match "%" and "$"
        ((filter code "$")
         (.. "invalid symbol character \"" code "\""))

        (else desc)))


;; Construct an error message, given error FORM and source TEXT.
;;
(define (describe-error form text filename)
  &public
  (case form
    ((PError pos desc)
     (let ((lc (get-subject-line-col pos (penc text)))
           (text text)
           (filename filename)
           (msg (get-error-msg desc)))
       (define `lnum (word 1 (subst ":" " " lc)))
       (define `col (word 2 (subst ":" " " lc)))
       (define `line-text (nth lnum (split "\n" text)))
       (define `ptr (subst " ^" "^" (.. (string-repeat " " col) "^")))
       (if (word-index? pos)
           (sprintf "%s:%s: %s\n%s\n%s\n" filename lc msg line-text ptr)
           (sprintf "%s: %s\n" filename desc))))))
