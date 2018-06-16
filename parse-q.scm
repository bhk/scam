;;--------------------------------
;; parse-q : parsing tests
;;--------------------------------

(require "core")
(require "parse" &private)

;;--------------------------------
;; utilities
;;--------------------------------

(format-add POut-format)

(define (check-eq expected actual)
    (if (eq? expected actual)
        1
        (print "Expected: " (format expected) "\n"
               "  Actual: " (format actual) )))

;; parse test
(define (pt pos o i)
  (check-eq (append pos o) (parse-exp (penc i) 1)))

(define (p1 text)
  (parse-exp (penc text) 1))


;;--------------------------------
;; tests
;;--------------------------------

(expect 3 (form-index (PString 3 0)))

;; penc isolates tokens as words

(expect "0 ( "                  (penc "0("))
(expect "a : b"                 (penc "a:b"))
(expect "a !0!0 b !0!0 ( "      (penc "a  b  ("))
(expect "a !0!0 b !0!0 ( !0 !1" (penc "a  b  ( !"))
(expect "!b \" "                (penc "\\\\\""))
(expect "!b!Q"                  (penc "\\\\\\\""))
(expect " , , ,@ , "            (penc ",,,@,"))
;; semicolones are combined with following text, but this must stop
;; at `\n`, `\` or `"`
(expect " ; \\t"                (penc ";\\t"))
(expect " ;abc \" "             (penc ";abc\""))
(expect " ;!0 \n "              (penc "; \n"))

(expect 1 (words (penc "                                ")))

;; pdec un-does penc

(define `(pde s)
  (expect s (pdec (penc s))))

(pde "")
(pde " ")
(pde "x\ty \t z")
(pde "                    ")
(pde "a b !0 c\"d(x);1{3}z")
(pde "x; \\  ;; \\\" \\\\\" 0 ! ")

;; pdec-str un-does penc but also decodes backslash sequences
;; and returns a demoted string.

(define (tdse o ?i)
  (check-eq (demote o) (pdec-str (subst " " "" (penc (or o i))))))

(expect 1 (tdse ""))
(expect 1 (tdse "\t\t"))
(expect 1 (tdse "                     "))
(expect 1 (tdse "a\"b"
                "a\\\"b"))


(expect "a b c" (string-value (PString 0 "a b c")))
(expect "a b c" (symbol-name (PSymbol 0 "a b c")))
(fexpect (PString 1 "x") (symbol-to-string (PSymbol 1 "x")))

;; format-form
(fexpect "(foo (x \"y\"))"  (format-form
                            (PList 0 [ (PSymbol 0 "foo")
                                       (PList 0 [ (PSymbol 0 "x") (PString 1 "y") ])])))
(fexpect "(foo ,\"x\")" (format-form (PList 0 [ (PSymbol 0 "foo") "x" ])))
(fexpect "`(foo)" (format-form (PQQuote 0 (PList 0 [ (PSymbol 0 "foo") ]))))

;; find-word
(fexpect 3 (find-word "b a b c" 2 "b"))

;; parse-exp: un-terminated error
(fexpect (POut 2 (PError 2 ")"))
        (parse-exp "!0 )" 1))

;; parse-exp: string
(fexpect (cons 5 (PString 1 "abc def"))
         (parse-exp (penc "\"abc def\"") 1))

;; simple errors
(fexpect (POut 2 (PError 2 "."))  (p1 " "))
(fexpect (POut 2 (PError 2 "."))  (p1 " "))
(fexpect (POut 1 (PError 1 ")"))  (p1 ")"))
(fexpect (POut 1 (PError 1 "]"))  (p1 "]"))
(fexpect (POut 2 (PError 2 "'"))  (p1 " ' a)"))

;; invalid characters
(fexpect (POut 1 (PError 1 "$")) (p1 "$b"))
(fexpect (POut 1 (PError 1 ":")) (p1 ":b"))
(fexpect (POut 1 (PError 1 "%")) (p1 "%b"))
(fexpect (POut 2 (PError 2 "%")) (p1 " %"))

;; symbols
(fexpect (POut 1 (PSymbol 1 "abc"))  (p1 "abc def"))

;; numbers
(fexpect (POut 1 (PString 1 123))  (p1 "123 def"))

;; literal strings
(fexpect (POut 2 (PString 1 ""))        (p1 "\"\" abc"))
(fexpect (POut 6 (PString 1 "a b\nc"))  (p1 "\"a b\\nc\""))
(fexpect (POut 5 (PString 1 "x bc"))    (p1 "\"x bc\" def"))
(fexpect (POut 4 (PString 1 "y\"b;c"))  (p1 "\"y\\\"b;c\" def"))
(fexpect (POut 4 (PString 1 "y\"b;c"))  (p1 "\"y\\\"b;c\" def"))
(fexpect (POut 5 (PString 1 " \t "))    (p1 "\" \\t \""))
(fexpect (POut 4 (PString 1 ";\t"))     (p1 "\";\\t\""))
(fexpect (POut 4 (PString 1 " \t"))     (p1 "\" \\x09\""))
(fexpect (POut 3 (PString 1 "A"))       (p1 "\"\\x41\""))
(fexpect (POut 6 (PString 1 "ab\\tAb")) (p1 "\"a\\x00b\\x5ct\\x41b\""))
(fexpect (POut 3 (PString 1 ""))        (p1 "\"\\x00\""))
(fexpect (POut 2 (PError 1 "\""))       (p1 "\""))         ; unterminated string
(fexpect (POut 4 (PError 3 "!B"))       (p1 "\"a\\.\""))   ; invalid `\` sequence

;; comments
(expect (POut 3 (PSymbol 3 "abc"))    (p1 ";comment\nabc def"))

;; lists
(fexpect (POut 3 (PList 1 [ (PSymbol 2 "pyt") ]))             (p1 "(pyt)"))
(fexpect (POut 5 (PList 1 [ (PSymbol 3 "xyz") ]))             (p1 "( xyz )"))
(fexpect (POut 7 (PList 1 [ (PSymbol 2 "a") (PList 4 []) ]))  (p1 "(a () )"))
(fexpect (POut 1 (PError 1 ")"))                              (p1 ")"))
(fexpect (POut 4 (PError 1 "("))                              (p1 "( a"))
(fexpect (POut 5 (PError 5 "] )"))                            (p1 "( a ]"))

;; vectors
(fexpect (POut 5 (PList 1 [ (PSymbol 0 "vector")
                           (PString 2 1) (PString 4 293) ]))
        (p1 "[1 293] 3"))
(fexpect (POut 2 (PError 1 "[") ) (p1 "["))
(fexpect (POut 3 (PError 3 ") ]")) (p1 "[ ) ]"))
(fexpect (POut 5 (PError 5 "] ) ]")) (p1  "[ ( ]"))

;; dictionaries
(fexpect (POut 2 (PDict 1 []))     (p1 "{}"))
(fexpect (POut 4 (PDict 2 []))     (p1 " { } "))
(fexpect (POut 9 (PDict 1 { (PSymbol 3 "a"): (PString 7 1) }))
         (p1 "{ a : 1 } "))
(fexpect (POut 9 (PDict 1 { (PSymbol 3 "a"): (PString 7 1) }))
         (p1 "{ a : 1,} "))
(fexpect (POut 11 (PDict 1 { (PSymbol 2 "a"): (PString 4 1),
                             (PSymbol 8 "b"): (PString 10 2) }))
         (p1 "{a:1 , b:2} "))

(fexpect (POut 2 (PError 1 "{") ) (p1 "{"))
(fexpect (POut 3 (PError 3 ") }")) (p1 "{ ) }"))
(fexpect (POut 5 (PError 5 "} ) }")) (p1  "{ ( }"))
(fexpect (POut 3 (PError 3 ":?") ) (p1 "{a"))
(fexpect (POut 3 (PError 3 ":?") ) (p1 "{a}"))
(fexpect (POut 4 (PError 4 ":?") ) (p1 "{a b}"))
(fexpect (POut 4 (PError 4 ":?") ) (p1 "{a (f 1 2)}"))
(fexpect (POut 4 (PError 4 ":?") ) (p1 "{1 1"))
(fexpect (POut 4 (PError 4 "v?") ) (p1 "{a:}"))


;; quote with "'"
(fexpect (POut 7 (PQuote 2 (PList 3 [ (PSymbol 4 "qwe") (PString 6 1) ])))
         (p1 " '(qwe 1)"))
(fexpect (POut 2 (PQuote 1 (PSymbol 2 "jkl")))
         (p1 "'jkl"))

;; backquote & unquote
(fexpect (POut 7 (PQQuote 1 (PList 2 [ (PString 3 1) (PUnquote 5 (PSymbol 6 "b")) ])))
         (p1 "`(1 ,b)"))
(fexpect (POut 1 (PError 1 "`")) (p1 "`"))


;; get-subject-line-col

(define test-lnum-subj
  (penc "A xxx x B\nC D\n\nE\n    \n\nF xx G"))

(define (test-lnum ch)
  (define `ndx (words (concat "x " (first (split ch test-lnum-subj)))))
  (get-subject-line-col ndx test-lnum-subj))

(expect "1:1" (test-lnum "A"))
(expect "1:9" (test-lnum "B"))
(expect "2:1" (test-lnum "C"))
(expect "2:3" (test-lnum "D"))
(expect "4:1" (test-lnum "E"))
(expect "7:1" (test-lnum "F"))
(expect "7:6" (test-lnum "G"))

;; describe-error: location

(expect (concat "FILE:2:5: MESSAGE\n"
                " def\n"
                "    ^\n")
        (describe-error (PError 4 "MESSAGE")
                        "abc\n def\njkl"
                        "FILE"))

(expect (concat "FILE:3:1: MESSAGE\n"
                ": def\n"
                "^\n")
        (describe-error (PError 4 "MESSAGE")
                        "a\n\n: def"
                        "FILE"))

;; describe-error

(define (tde form)
  (describe-error form "\"abc)\n(def)\n" "TFILE"))

(expect 1 (see "TFILE:1:1: unterminated string\n\"abc)\n^\n"
               (tde (PError 1 "\""))))

(expect 1 (see "TFILE:2:1: unmatched \"(\"\n(def)\n^\n"
               (tde (PError 5 "( blahblah"))))

(expect 1 (see "TFILE:1:5: unmatched \")\"\n"
               (tde (PError 3 ")"))))

(expect 1 (see "prefix \"'\" must immediately precede"
               (tde (PError 1 "'"))))

(expect 1 (see "prefix \"`\" must"
               (tde (PError 1 "`"))))

(expect 1 (see "TFILE:2:1: invalid frob\n(def)\n^\n"
               (tde (PError 5 "invalid frob"))))

(expect 1 (see "TFILE:1:1: invalid symbol character"
               (tde (PError 1 "$"))))

;; parse-text

(fexpect [ (PList 1 [ (PSymbol 2 "or") (PString 4 1) ])
           (PSymbol 7 "a") ]
         (parse-text "(or 1) a"))
