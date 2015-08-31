;;--------------------------------
;; parse-q : parsing tests
;;--------------------------------

(require "core")
(require "parse" &private)

;;--------------------------------

(define (check-eq expected actual)
    (if (eq expected actual)
        1
        (print "Expected: \"" expected "\"\n"
               "  Actual: \"" actual "\"")))

;;--------------------------------

;; penc isolates tokens as words

(expect "a !0!0 b !0!0 ( "      (penc "a  b  ("))
(expect "a !0!0 b !0!0 ( !0 !1" (penc "a  b  ( !"))
(expect "!b \" "                (penc "\\\\\""))
(expect "!b!Q"                  (penc "\\\\\\\""))
(expect " , , ,@ , "            (penc ",,,@,"))

;; pdec un-does penc

(define `(pde s)
  (expect s (pdec (penc s))))

(pde "")
(pde " ")
(pde "x\ty \t z")
(pde "                    ")
(pde "a b !0 c\"d(x);z")
(pde "x; \\  ;; \\\" \\\\\" 0 ! ")

;; pdec-str un-does penc but also decodes backslash sequences
;; and returns a demoted string.

(define (tdse o i)
  (check-eq (demote o) (pdec-str (penc (or o i)))))

(expect 1 (tdse ""))
(expect 1 (tdse "\t\t"))
(expect 1 (tdse "                     "))
(expect 1 (tdse "a\"b"
                "a\\\"b"))


;;--------------------------------


(define (pexpect pat value)
  (if (not (filter pat value))
      (expect pat value)))


;; form value accessors
(pexpect "S%" (symbol? `x))
(expect ""  (symbol? `"x"))
(pexpect "Q%" (string? "Q x"))
(expect ""  (string? "S x"))
(pexpect "L%" (list? "L"))
(expect "a b c" (string-value ["Q" "a b c"]))
(expect "a b c" (symbol-name ["S" "a b c"]))
(expect "Q x" (symbol-to-string "S x"))

;(expect "S z" (list-nth 3 ["L" "S x" "S y" "S z"]))

;; format-form
(expect "(foo (x \"y\"))"  (format-form
                            ["L" "S foo" ["L" "S x" "Q y"]]))
(expect "(foo ,\"x\")" (format-form ["L" "S foo" "x"]))
(expect "`(foo)" (format-form ["`" ["L" "S foo"]]))

;; find-word
(expect 3 (find-word "b a b c" 2 "b"))

;; Q ...
(expect "5 Q.1 abc!0def" (parse-exp (penc "\"abc def\"") 1))

;; parse test
(define (pt pos o i)
  (check-eq (append pos o) (parse-exp (penc i) 1)))

;; simple errors
(expect 1 (pt 2 "E.2 ." " "))
(expect 1 (pt 1 "E.1 )" ")"))
(expect 1 (pt 1 "E.1 ]" "]"))
(expect 1 (pt 2 "E.2 '" " ' a)"))

;; symbols
(expect 1 (pt 1 "S.1 abc"       "abc def"))

;; numbers
(expect 1 (pt 1 "Q.1 123"       "123 def"))

;; literal strings
(expect 1 (pt 5 ["Q.1" "a b\nc"]   "\"a b\\nc\""))
(expect 1 (pt 5 ["Q.1" "x bc"]     "\"x bc\" def"))
(expect 1 (pt 2 ["Q.1" ""]         "\"\" abc"))
(expect 1 (pt 4 ["Q.1" "y\"b;c"]   "\"y\\\"b;c\" def"))
(expect 1 (pt 4 ["Q.1" "y\"b;c"]   "\"y\\\"b;c\" def"))
(expect 1 (pt 2 ["E.1" "\""]       "\""))

;; comments
(expect 1 (pt 3 "S.3 abc"       ";comment\nabc def"))

;; lists
(expect 1 (pt 3 ["L.1" "S.2 pyt"]    "(pyt)"))
(expect 1 (pt 5 ["L.1" "S.3 xyz"]    "( xyz )"))
(expect 1 (pt 7 ["L.1" "S.2 a" "L.4"]  "(a () )"))
(expect 1 (pt 1 ["E.1" ")"] ")"))
(expect 1 (pt 1 ["E.1" "("] "( a"))
(expect 1 (pt 5 ["E.5" "]" ")"] "( a ]"))

;; vectors
(expect 1 (pt 5 ["L.1" "S vector" "Q.2 1" "Q.4 293"]  "[1 293] 3"))
(expect 1 (pt 1 "E.1 ["  "["))
(expect 1 (pt 3 "E.3 ) ]"  "[ ) ]"))
(expect 1 (pt 5 "E.5 ] ) ]"  "[ ( ]"))

;; quote with "'"
(expect 1 (pt 7 ["'.2" ["L.3" "S.4 qwe" "Q.6 1"]] " '(qwe 1)"))
(expect 1 (pt 2 ["'.1" "S.2 jkl"] "'jkl"))

;; backquote & unquote
(expect 1 (pt 9 ["`.1" ["L.2" "S.3 a" "Q.5 1" [",.7""S.8 b"]]]
    "`(a 1 ,b)"))
(expect 1 (pt 1 "E.1 `" "`"))


;; get-line-info


(define test-lnum-subj
  (penc "A xxx x B\nC D\n\nE\n    \n\nF xx G"))

(define (test-lnum ch)
  (define `ndx (words (concat "x " (first (split ch test-lnum-subj)))))
  (describe-lnum ndx test-lnum-subj))


(expect 1 (test-lnum "A"))
(expect 1 (test-lnum "B"))
(expect 2 (test-lnum "C"))
(expect 2 (test-lnum "D"))
(expect 4 (test-lnum "E"))
(expect 7 (test-lnum "F"))
(expect 7 (test-lnum "G"))

(expect ["def ghi" "x" "jkl m"]
        (describe-line 4 "abc \n def!0ghi x jkl!0m \n op"))

(expect ["abc" "" ""]
        (describe-line 2 "abc \n def"))

(expect ["    " "\\\"" "mnb vcx"]
        (describe-line 3 "abc \n!0!2!0 \\\" mnb!0vcx"))

;; describe-error

(expect 1 (natural? 1))
(expect "" (natural? 1.0))
(expect "" (natural? 1e5))
(expect "" (natural? -1))
(expect "" (natural? 0))


(define (tde form)
  (describe-error form "\"abc)\n(def)\n" "TFILE"))

(expect 1 (see "TFILE:1: unterminated string\nat: *\"*abc"
               (tde "E.1 \"")))

(expect 1 (see "TFILE:2: unmatched \"(\"\nat: *(*def)\n"
               (tde "E.5 ( blahblah")))

(expect 1 (see "TFILE:1: unmatched \")\"\n"
               (tde "E.1 )")))

(expect 1 (see "prefix \"'\" must immediately precede"
               (tde "E.1 '")))

(expect 1 (see "prefix \"`\" must"
               (tde "E.1 `")))

(expect 1 (see "TFILE:2: invalid frob\nat: *(*def)\n"
               (tde ["E.5" "invalid frob"])))

;; parse-text

(expect [ ["L.1" "S.2 or" "Q.4 1"] "S.7 a" ]
        (parse-text "(or 1) a"))

(print "parse ok")
