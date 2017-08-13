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
           [ (num-pad (* n (- (strlen c) 1)) 5 0) ; bytes saved
             (num-pad n 4 0)                      ; # occurrences
             c ])))))                             ; substring


;; Display occurrences and savings for each string in SUBS.  MAP maps the
;; string to some other value (e.g its value before prior substitutions).
;;
(define (rank-freqs subs env ?map)
  (for e (freqs subs env)
       (printf "%s %s \"%s\""
               (nth 1 e)
               (nth 2 e)
               (dict-get (nth 3 e) map (nth 3 e)))))


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
                        {(reduce-string c subs subchars): c}))
        (size0 (bytes target)))
    (compile-cmp subs subchars)
    (compare-size "size" size0 (bytes ex))
    (rank-freqs (dict-keys cx) ex cx)
    ex))


;; Remove imported environment entries from environment V.
;; This reduces size by ~60%.
;;
(define (strip-imports v)
  (strip-vec
  (foreach w v
           (if (not (filter "i%" (EDefn.scope (dict-value w))))
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
  (sort-by (lambda (c) (num-pad (count c scam-env) 6 0))
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
    "!1"
    "!10"
    "!101"        ;; argc?
    "!11"
    "!110"
    "!111"
    "!1110"
    "!1111"
    "!11110"
    "!0!11"
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
    "!=!1:EDefn1!0~%!0x!0"
    "!=!1:EDefn1!0~%!0x!01 "
    "!=!1:EDefn1!0~%!0x!02 "
    "!=!1:EDefn1!0~%!0x!03 "
    "!=!1:EDefn1!0:!0x!0"
    "!0:!0p!0"
    "0!11"
    "10"
    "110"
    "111"
    "1110"
    "1111"
    "11111"
    ":IL0"
    ":IL2"
    ":IL3"
    ":IL4"
    ":IL6"
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
   "!0"                      ;; 1950  ;
   "!1"                      ;; 165   \\
   "!11"                     ;; 2468  ,
   "!0!11"                   ;; 199
   ":IL0"
   ":IL2"
   ":IL3"
   ":IL4"
   "!10"                     ;; 1056
   "!110"                    ;; 242
   "!1111"                   ;; 984
   "111"                     ;; 400
   "!1110"                   ;; 326
   "!=!1:EDefn"              ;; 1449
   "!=!1:EDefn1!0~%!0"       ;; 5264
   "!=!1:EDefn1!0:!0"        ;; 240   "
   " ml.special-"            ;; 286
   "!=!1:EDefn1!0~%!0p!0"    ;; 498
   "!=!1:EDefn1!0~%!0p!01 "  ;; 172
   "!=!1:EDefn1!0~%!0x!0"    ;; 254
   ])


;;
;; Estimate savings of compress-strings.
;; Output source for the resulting compress and expand functions.
;; Display benefits of additional candidate strings.
;;
;; (print "in: " scam-env)
(print "saves reps str")
(print "----- ---- ---------")
(rank-after scam-env compress-strings candidate-strings)


;;
;; Validate compress & expand functions generated by a recent run.
;;

(define (cmp s) (subst ";" "!A" "\\" "!B" "," "!C" "`" "!D" "'" "!E" "<" "!F" ">" "!G" "[" "!H" "]" "!I" "|" "!J" "@" "!K" "{" "!L" "}" "!M" "#" "!N" "\"" "!O" "&" "!P" "(" "!Q" ")" "!R" "+" "!S" "_" "!T" "!0" ";" "!1" "\\" "\\1" "," ";," "`" ":IL0" "'" ":IL2" "<" ":IL3" ">" ":IL4" "[" "\\0" "]" ",0" "|" ",11" "@" "111" "{" ",10" "}" "!=\\:EDefn" "#" "#1;~%;" "\"" "#1;:;" "&" " ml.special-" "(" "\"p;" ")" ")1 " "+" "\"x;" "_" s))
(define (exp s) (subst "_" "\"x;" "+" ")1 " ")" "\"p;" "(" " ml.special-" "&" "#1;:;" "\"" "#1;~%;" "#" "!=\\:EDefn" "}" ",10" "{" "111" "@" ",11" "|" ",0" "]" "\\0" "[" ":IL4" ">" ":IL3" "<" ":IL2" "'" ":IL0" "`" ";," "," "\\1" "\\" "!1" ";" "!0" "!T" "_" "!S" "+" "!R" ")" "!Q" "(" "!P" "&" "!O" "\"" "!N" "#" "!M" "}" "!L" "{" "!K" "@" "!J" "|" "!I" "]" "!H" "[" "!G" ">" "!F" "<" "!E" "'" "!D" "`" "!C" "," "!B" "\\" "!A" ";" s))

(compare-size "size" (bytes scam-env) (bytes (cmp scam-env)))

(expect (exp (cmp scam-env))
        scam-env)
