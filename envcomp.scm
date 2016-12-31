;;--------------------------------
;; envcomp.scm
;;--------------------------------

;; Analyze compression performance.
;; compression performance

(require "core")
(require "gen" &private)
(require "io")
(require "num")
(require "strlen")


;; Single character values that can be used to replace longer strings.
;; Ideally, these should occur rarely or never in the environment.
;;
(define `subchars
  "; \\ , ` ' < > [ ] | @ { } # \" & ( ) + _ / $")

(expect subchars
        (uniq subchars))

;; Escape codes that will replace subchars.
;;
(define `escape-codes
  (addprefix
   "!"
   "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"))


;; Return number of bytes in STR.
;;
(define (bytes str)
  (word 1 (shell (concat (echo-command str) " | wc -c"))))


;; Count number of occurrences of SUBSTR in STR.
;;
(define (count substr str)
  (- (words (split substr str)) 1))


;; Count occurrences of each string in SUBS, and calculate potential
;; savings (if replaced with a single byte).  Returns a sorted
;; vector of [SAVINGS COUNTS STR].
;;
(define (freqs subs env)
  (reverse
   (sort
    (for c subs
         (let ((n (count c env)))
           [ (zero-pad (* n (- (strlen c) 1)) 5)  ; bytes saved
             (zero-pad n 4)                       ; # occurrences
             c ])))))                             ; substring


;; Display occurrences and savings for each string in SUBS.  MAP maps the
;; string to some other value (e.g its value before prior substitutions).
;;
(define (rank-freqs subs env ?map)
  (for e (freqs subs env)
       (printf "%s %s \"%s\""
               (nth 1 e)
               (nth 2 e)
               (hash-get (nth 3 e) map (nth 3 e)))))


;; Compress target by replacing each string in SUBS with a string in CHARS.
;; don't care which byte is used to encode the substring).  It is assumed
;; that none of TO-STRINGS appear anywhere in FROM-STRINGS.
;;
(define (reduce-string target from-strings to-strings)
  (define `s1 (first from-strings))
  (define `(reduce x) (subst s1 (first to-strings) x))

  (if from-strings
      (reduce-string (reduce target)
                     (for s (rest from-strings) (reduce s))
                     (rest to-strings))
      target))

(expect (reduce-string "abcdefg" ["abc" "abcd" "abcdef"] [0 1 2])
        "2g")


;; Substitute FROM-STRINGS with TO-STRINGS in SUBS.  Each substitution
;; also applies to subsequent strings in FROM_STRINGS.
;;
(define (reduce-strings subs from-strings to-strings)
  (for s subs
       (reduce-string s from-strings to-strings)))


(define (compare-size label original new)
  (printf "%s : %s -> %s  %s%% reduction"
          label original new
          (/ (* (- original new) 100) original)))


(define (compile-subs subs subchars)
  (define `from (first subs))
  (define `to (first subchars))

  (if subs
      (append [from to]
              (compile-subs (for s (rest subs)
                                 (subst from to s))
                            (rest subchars)))))


(define (compile-cmp subs subchars)
  (define `(format-string str)
    (concat "\"" (subst "\\" "\\\\" "\"" "\\\"" str) "\""))
  (define `chars-used
    (wordlist 1 (words subs) subchars))

  ;; Encode subchars first, while the conditions that ensure reversibility
  ;; still hold ("!" only appears in an escape code).
  (let ((args (append
                (compile-subs chars-used escape-codes)
                (compile-subs subs subchars))))
    (printf "(define (cmp s) (subst %s s))"
            (concat-for a args " " (format-string a)))
    (printf "(define (exp s) (subst %s s))"
            (concat-for a (reverse args) " " (format-string a)))))


;; Compress TARGET with SUBS and then show relative value of each of
;; CANDIDATES.
;;
(define (rank-after target subs candidates)
  (let ((ex (reduce-string target subs subchars))
        (cx (append-for c candidates
                        (hash-bind
                         (reduce-string c subs subchars)
                         c)))
        (size0 (bytes target)))
    (compile-cmp subs subchars)
    (compare-size "size" size0 (bytes ex))
    (rank-freqs (hash-keys cx) ex cx)
    ex))


;; Remove imported environment entries from environment V.
;; This reduces size by ~60%.
;;
(define (strip-imports v)
  (strip-vec
  (foreach w v
           (if (not (filter "i%" (EDefn.scope (hash-value w))))
               w))))


;;--------------------------------

;;
;; Load exports from bin/scam
;;

(define scam-env
  (let ((exports (shell! "grep '# Exports' bin/scam")))
    (printf "exports: %s bytes" (bytes exports))
    (strip-vec
     (foreach line (split "\n" exports)
              (env-parse line)))))

(printf "raw: %s entries, %s bytes" (words scam-env) (bytes scam-env))
(set scam-env (strip-imports scam-env))
(printf "no-imports: %s bytes" (bytes scam-env))
(set scam-env (tokenize-key scam-env))
(printf "no-name-dup: %s bytes" (bytes scam-env))

;; Uncomment for manual inspection:
;; (write-file ".out/env" scam-env)


;;
;; Rank sub-chars, lest-frequent first.
;;

(define best-chars
  (sort-by (lambda (c) (zero-pad (count c scam-env) 6))
           subchars))

;; (printf "best-chars: (%s)  %q " (words best-chars) best-chars)
;; Display actual number of occurrences:
;; (foreach c best-chars (printf "'%s' x %s" c (count c scam-env)))


(define `candidate-strings
  [
    "!0"
    "!0p"
    "!0p!0"
    "!0x!0"
    "x!01 "
    "!0p "
    "!0x "
    "!0~"
    "!0!11:P0!10"
    "!0!11:P1!10"
    "!1"
    "!10!111:P"
    "!10!111:P0!110"
    "!10!111:P1!110"
    "!10!111:P2!110"
    "!10"
    "!101"        ;; argc?
    "!11"
    "!110!1111:P0!1110"
    "!110!1111:P1!1110"
    "!110!1111:P2!1110"
    "!110"
    "!111"
    "!1110!11111:P0!11110"
    "!1110!11111:P1!11110"
    "!1110!11111:P2!11110"
    "!1110"
    "!1111"
    "!11110"
    "!=!1:EDefn"
    "!=!1:EDefn0"
    "!=!1:EDefn1"
    "!=!1:EDefn2"
    "!=!1:EDefn3"
    "!=!1:EDefn1!0~"
    "!=!1:EDefn1!0~%!0"
    "!=!1:EDefn1!0~%"
    "!=!1:EDefn1!0~%!0p"
    "!=!1:EDefn1!0~%!0p!0"
    "!=!1:EDefn1!0~%!0p!01 "
    "!=!1:EDefn1!0~%!0p!02 "
    "!=!1:EDefn1!0~%!0p!03 "
    "!=!1:EDefn1!0:!0"
    "!=!1:EDefn1!0:!0p"
    "!=!1:EDefn1!0:!0p!0"
    "!0:!0"
    "!0:!0p"
    "!0:!0p!0"
    "0!11"
    "0!111"
    "10!111"
    "10"
    "110"
    "111"
    "1110"
    "1111"
    "11111"
    "111:P2!11"
    ":P0"
    ":P1"
    ":P2"
    "11:P"
    "11:P0"
    "special-"
    "ml.special-"
    " ml.special-"
    "1 "
    "2 "
    "!01 "
    "!02 "
    "filter"
    "concat"
    "word"
    ])

;;(rank-after scam-env [] candidate-strings)

(define `compress-strings
  [
   "!0"                      ;; 634
   "10"                      ;; 162
   "!10"                     ;; 178
   "!110"                    ;; 428
   "!1111"                   ;; 464
   "!1110"                   ;; 225
   "!11110"                  ;; 260
   "11:P"                    ;; 240
   "!=!1:EDefn"              ;; 1890
   "!=!1:EDefn1!0~%!0"       ;; 640
   "ml.special-"             ;; 280
   "!=!1:EDefn1!0~%!0p!0"    ;; 312
   "!=!1:EDefn1!0~%!0p!01 "  ;; 176
   "!0!11:P0!10"             ;; 325
   "!10!111:P2!110"          ;; 992
   "!10!111:P0!110"          ;; 392
   "!110!1111:P2!1110"       ;; 475
   "!=!1:EDefn1!0:!0p!0"     ;; 198
   ;;"x!01 "                   ;; 144
   ;;"!110!1111:P0!1110"       ;; 140
   "!1110!11111:P2!11110"    ;; 348
   "!10!111:P1!110"          ;; 608
   ])


;;
;; Estimate savings of compress-strings.
;; Output source for the resulting compress and expand functions.
;; Display benefits of additional candidate strings.
;;
;;(print "in: " scam-env)
(rank-after scam-env compress-strings candidate-strings)


;;
;; Validate compress & expand functions generated by a recent run.
;;

(define (cmp s) (subst ";" "!A" "\\" "!B" "," "!C" "`" "!D" "'" "!E" "<" "!F" ">" "!G" "[" "!H" "]" "!I" "|" "!J" "@" "!K" "{" "!L" "}" "!M" "#" "!N" "\"" "!O" "&" "!P" "(" "!Q" ")" "!R" "+" "!S" "_" "!T" "!0" ";" "10" "\\" "!\\" "," "!1\\" "`" "!1111" "'" "!11\\" "<" "!111\\" ">" "11:P" "[" "!=!1:EDefn" "]" "]1;~%;" "|" "ml.special-" "@" "|p;" "{" "{1 " "}" ";![0," "#" ",!1[2`" "\"" ",!1[0`" "&" "`':P2<" "(" "]1;:;p;" ")" "<'1:P2>" "+" ",!1[1`" "_" s))
(define (exp s) (subst "_" ",!1[1`" "+" "<'1:P2>" ")" "]1;:;p;" "(" "`':P2<" "&" ",!1[0`" "\"" ",!1[2`" "#" ";![0," "}" "{1 " "{" "|p;" "@" "ml.special-" "|" "]1;~%;" "]" "!=!1:EDefn" "[" "11:P" ">" "!111\\" "<" "!11\\" "'" "!1111" "`" "!1\\" "," "!\\" "\\" "10" ";" "!0" "!T" "_" "!S" "+" "!R" ")" "!Q" "(" "!P" "&" "!O" "\"" "!N" "#" "!M" "}" "!L" "{" "!K" "@" "!J" "|" "!I" "]" "!H" "[" "!G" ">" "!F" "<" "!E" "'" "!D" "`" "!C" "," "!B" "\\" "!A" ";" s))

(compare-size "size" (bytes scam-env) (bytes (cmp scam-env)))

(expect (exp (cmp scam-env))
        scam-env)
