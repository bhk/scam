;; # core: General-Purpose Functions
;;
;; The `core` library provides general-purpose functions, such as `eq?`, and
;; common operations on the fundamental data types in SCAM -- word lists,
;; vectors, and dictionaries.

(declare SCAM_DEBUG &native)

;; Return 1 if A and B are equal, nil otherwise.
;;
(define (eq? a b)
  &public
  (define `aa (.. 1 a))
  (define `bb (.. 1 b))
  (if (findstring aa (findstring bb aa))
      1))

;; Return A.
;;
(define `(identity a)
  &public
  a)

;; Return the parameter that is not nil (unless both or none are nil).
;;
(define (xor a b)
  &public
  (if a (if b nil a) b))

;; Concatenate strings in VEC, separating them with DELIM.
;;
(define (concat-vec vec ?delim)
  &public
  (promote (subst " " (demote delim) vec)))

;; Add ITEM to the front of vector VEC.
;;
(define (cons item vec)
  &public
  (.. (demote item) (if vec " ") vec))

;; Add ITEM to end of vector VEC.
;;
(define (conj vec item)
  &public
  (.. vec (if vec " ") (demote item)))

;; Return the last item in vector VEC.
;;
(define `(last vec)
  &public
  (promote (lastword vec)))

;; Remove redundant spaces from a vector or list.
;;
;; Consecutive spaces and tabs will be collapsed to a single space and
;; leading and trailing spaces will be removed.  Newline characters are
;; not disturbed.
;;
(define `(strip vec)
  &public
  (filter "%" vec))

;; Return elements in vector VEC *except* for the last one.
;; This may also be applied to word lists or dictionaries.
;;
(define (butlast vec)
  &public
  (wordlist 2 (words vec) (.. "X " vec)))

;; Return a vector of all members of VEC for which (FN member) is non-nil.
;;
(define (select-vec fn list)
  &public
  (filter-out "!" (foreach dx list
                           (if (fn (promote dx)) dx "!"))))

;; Return a list of words in LIST for which `(FN <word>)` is non-nil.
;;
(define (select-words fn list)
  &public
  ;; wrap in outer 'foreach' to eliminate redundant spaces
  (foreach a
           (foreach x list (if (fn x) x))
           a))

;; Return the first non-nil member of VEC.
;;
(define `(vec-or vec)
  &public
  (first (filter-out [""] vec)))


(define (vec-filter fname pat v)
  (if (findstring "%" pat)
      ;; escaping would be expensive
      (subst "!P" "%" (call fname (subst "%" "!P" pat)
                                  (subst "%" "!P" v)))
      (call fname pat v)))


;; Return entries in vector A that also appear in vector B.
;; This may also be applied to dictionaries.
;;
(define `(vec-intersect a b)
  &public
  (vec-filter "filter" b a))


;; Return entries in vector A that do not appear in vector B.
;; This may also be used to applied to dictionaries.
;;
(define `(vec-subtract a b)
  &public
  (vec-filter "filter-out" b a))


(define (indices-b max len lst)
  (if (filter max len)
      len
      (.. len " " (indices-b max (words lst) (.. ". " lst)))))

(define (indices-a max)
  (if (filter-out 0 max)
      (indices-b max 1 ". .")))

;; Return a vector of the indices (1, 2, ...) of words in word list (or vector)
;; LST.
;;
(define `(indices lst)
  &public
  (indices-a (words lst)))


;; Reverse a list in groups sized by powers of ten.  Simpler implementations
;; are O(n^2) because (rest VEC) is an O(n) operation.
;;
;; z=""    -->      10       9   ...     2       1
;; z="0"   ->    91-100   81-90  ...  11-20   01-10
;; z="00"  ->   901-1000 801-900 ... 101-200 001-100
;;
(define (rev-by-10s list z)
  (define `z+1
    (patsubst "%0" "%1" z))
  (define `z/10
    (patsubst "0%" "%" z))
  (define `(1- n)
    (word n "0 1 2 3 4 5 6 7 8 9"))
  (define `(group prefix)
    (wordlist (.. (1- prefix) z+1)
              (.. prefix z)
              list))

  (if list
      (if z
          (foreach p [10 9 8 7 6 5 4 3 2 1] (rev-by-10s (group p) z/10))
          (foreach p [10 9 8 7 6 5 4 3 2 1] (word p list)))))


;; Detect length of list to the nearest order of magnitude.
;; 0..10 items => "";  11-100 => "0";  101..1000 => "00", ...
(define (rev-zeroes list z)
  (if (word (.. 1 z 1) list)
      (rev-zeroes list (.. 0 z))
      z))

;; Reverse word list (or vector) LIST.
;;
(define (reverse list)
  &public
  (nth-rest 1 (rev-by-10s list (rev-zeroes list nil))))


;; while: This implementation limits recursion depth to log(N).  A very
;;   simple implementation yields O(N) recursion depth, which can degrade
;;   performance seriously in Make:
;;
;;      (define (while pred fn value)
;;        (if (pred value)
;;            (while pred fn (fn value))
;;            value))
;;
;; while-N has two parameters that control its recursion: k and level.  It
;; invokes itself in three different ways:
;;    forward: same level, increment k
;;    downward: decrease level, start k at 1
;;    upward: increase level, start k at (MAX-1)
;;
;; When invoking downward, the are three iterations at each level because 3
;; is close to `e`, which minimizes x*log(x).  While we are increasing the
;; recursion depth, however, we iterate only once per level.


;; Recursively apply FN to VALUE until `(PRED result)` is nil, then return
;; the final value.
;;
(declare (while pred do initial)
         &public)

(begin
  ;; The "level 0" iteration recurses 20 times and then returns
  ;;
  (define (while-0 pred do value k0)
    ;; while-0 loops are cheaper than while-N
    (define `k0-done "iiiiiiiiiiiiiiiiiiii")

    (if (filter k0-done k0)
        ;; return incomplete status (continue=1)
        [1 value]
        ;; too deeply nested
        (if (pred value)
            (while-0 pred do (do value) (.. "i" k0))
            ;; done (continue=0)
            [0 value])))

  (define (while-N pred do o level k)
    (define `done (filter 0 (word 1 o)))
    (define `val (nth 2 o))

    (if done
        o
        (if (filter "iii" k)
            ;; done at this level
            (if (filter 1 level)
                ;; We're at the top (haven't been called by a higher level) so
                ;; expand the recursion depth.
                (while-N pred do o (.. level " " 0) "ii")
                ;; return to higher level
                o)

            ;; recurse and repeat
            (while-N pred do
                     (if level
                         (while-N pred do o (rest level) nil)
                         (while-0 pred do val nil))
                     level
                     (.. "i" k)))))

  (define (while pred op initial)
    (if (pred initial)
        (let ((new-value (op initial))
              (pred pred)
              (op op))
          (nth 2 (while-N pred op (while-0 pred op new-value nil) 1 "ii")))
        initial)))


;; Return S if S is a valid numeric literal in SCAM, nil otherwise.
;;
(define (numeric? s)
  &public
  ;; reduce all digits to '0' and (E|e)[-]<digit> to 'e'
  (define `a (subst 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0
                    "+" "-" "e" "E" "E-" "E" "E0" "e" " " "_" s))
  ;; remove one leading "-"
  (define `b (subst "0" "" (patsubst "-%" "%" a)))
  ;; after removing '.' and 'e', there should be nothing left
  (define `c (patsubst ".%" "%" (patsubst "%e" "%" b)))

  (if (filter "0% 1% 2% 3% 4% 5% 6% 7% 8% 9%" (subst "-" "" s))
      (if c "" s)))


;; Return non-nil if N is safe to pass to WORD or WORDLIST.  This means it
;; consists only of decimal digits and is non-zero.
;;
(define (word-index? n)
  &public
  (if (subst 0 nil 1 nil 2 nil 3 nil 4 nil 5 nil 6 nil 7 nil 8 nil 9 nil n)
      nil
      (subst 0 nil n)))


;; Combine one or more (potentially empty) vectors, word lists, or
;; dictionaries.
;;
(define (append ?a ?b ?c ?d ?e ?f ?g ?h ...others)
  &public
  (strip (._. a b c d e f g h (if others (promote others)))))


;; Return the key portion of PAIR.
;;
(define (dict-key pair)
  &public
  (promote (subst "!8" "%" (word 1 (subst "!=" " " pair)))))

;; Return the value portion of PAIR.
;;
(define `(dict-value pair)
  &public
  (nth 2 (subst "!=" " " pair)))

(define `(dict-matches key dict)
  (filter (.. (subst "%" "!8" [key]) "!=%") dict))

;; Return the first pair matching KEY.  Unlike `dict-get`, this indicates
;; whether a match was found (every pair is non-nil).
;;
(define (dict-find key dict)
  &public
  (word 1 (dict-matches key dict)))

;; Return the value bound to KEY.  If more than one pair matches KEY, only
;; the first is returned.  If no pair is found, DEFAULT is returned.
;;
(define (dict-get key dict ?default)
  &public
  (promote (or (word 2 (subst "!=" " " (dict-matches key dict)))
               (subst "!" "!1" default))))

;; Remove all pairs whose key portion is KEY from DICT.
;;
(define (dict-remove key dict)
  &public
  (filter-out (.. (subst "%" "!8" [key]) "!=%") dict))

;; Bind KEY to VALUE in dictionary DICT, removing other entries for KEY.
;;
(define (dict-set key value dict)
  &public
  (foreach p (.. (subst "%" "!8" [key]) "!=")
           (.. p [value] " " (filter-out (.. p "%") dict))))

;; Remove pairs in a dictionary that are preceded by pairs that share the
;; same KEY.
;;
(define (dict-compact dict ?result)
  &public
  (if (not dict)
      result
      (let& ((pair (word 1 dict))
             (prefix (word 1 (subst "!=" "!=% " pair))))
        (append pair
                (dict-compact (filter-out prefix (rest dict)))))))

;; Return a vector of keys from DICT.
;;
(define (dict-keys dict)
  &public
  (subst "!8" "%" (filter-out "!=%" (subst "!=" " !=" dict))))

;; Return a vector of values from DICT.
;;
(define (dict-values dict)
  &public
  (filter-out "%!=" (subst "!=" "!= " dict)))

;; Create a new dictionary in which each key from DICT appears only once,
;; bound to a vector of all its values in DICT.
;;
(define (dict-collate pairs)
  &public
  (foreach p (word 1 (subst "!=" "!= " (word 1 pairs)))
           (append (.. p [(filtersub (.. p "%") "%" pairs)])
                   (dict-collate (filter-out (.. p "%") pairs)))))


(declare (format value))


;; Return STR if it should be displayed as a symbol (versus a quoted
;; string) in a dictionary.
(define (symbol? str)
  (and (findstring str (word 1 str))
       (not (or (findstring "\n" str)
                (findstring "(" str)
                (findstring ")" str)
                (findstring "[" str)
                (findstring "]" str)
                (findstring "," str)
                (findstring ";" str)
                (findstring ":" str)
                (findstring "'" str)
                (findstring "`" str)
                (findstring "\"" str)
                (findstring "!=" str)))
       str))

;; Convert H to dictionary syntax; return nil if dictionary syntax
;; cannot represent H.
;;
(define (format-dict h)
  (define `pairs
    (foreach e h
             (begin
               (define `key (dict-key e))
               (define `key-fmt (or (symbol? key)
                                    (format key)))
               (define `value (dict-value e))
               [(.. key-fmt ": " (format value))])))

  (define `(dict-elem w ndx)
    (nth ndx (subst "!=" " " w)))

  (if (findstring "!=" h)
      (if (eq? h (foreach w h
                          {(dict-elem w 1): (dict-elem w 2)} ))
          (.. "{" (concat-vec pairs ", ") "}"))))


;; Extract members from a record, applying FUNC to them, appending the
;; result to ACCUM (with a single space separator for each).
;;
(define (data-foreach func encodings values accum)
  (define `e (word 1 encodings))
  (define `value
    (cond ((filter "L" e) values)
          ((filter "S" e) (nth 1 values))
          ((filter "W" e) (word 1 values))
          (else            (error "bad encoding in ctor pattern"))))

  (if encodings
      (data-foreach func (rest encodings) (rest values)
                    (.. accum (if accum " ") (func value e)))
      accum))


;; Return description of record as a constructor invocation, or nil if the
;; value is not a record.
;;
;; Example:   `!:T0 !0`  -->  `(Ctor " ")`
;;
(define (format-record record)
  (define `tag (word 1 record))

  (if (filter "!:%" tag)
      (let ((pattern (dict-get tag ^tags))
            (values (rest record))
            (tag tag)
            (record record))
        (define `ctor-name (nth 1 pattern))
        (define `encodings (rest pattern))
        (define `reconstructed
          (data-foreach (lambda (v e) (if (eq? "S" e) [v] v))
                        encodings values tag))
        (define `arg-text
          (data-foreach (lambda (v e)
                          (if (and (eq? "L" e) (not v))
                              "[]"
                              (format v)))
                        encodings values ""))

        (and pattern
             (eq? (strip record) (strip reconstructed))
             (.. "(" ctor-name (if encodings " ") arg-text ")")))))

(define *format-funcs* nil)

;; Register a formatting function that will be used by `format`.
;;
(define (format-add func)
  &public
  (set *format-funcs* (cons func *format-funcs*)))

(define (format-custom str funcs)
  (if funcs
      (or ((first funcs) str)
          (format-custom str (rest funcs)))))


;; Return a SCAM literal that evaluates to VALUE, using the "friendliest"
;; representation available.
;;
(define (format str)
  &public
  (define `(format-vector str)
    (if (eq? str (foreach w str (demote (promote w))))
        (.. "[" (foreach w str (format (promote w))) "]")))

  (or (format-custom str *format-funcs*)
      (if (findstring "!" str)
          (or (format-dict str)
              (format-record str)))
      (if (or (findstring "!" str)
              (and (findstring " " str)
                   (numeric? (subst " " "" str))))
          (format-vector str))
      (numeric? str)
      (.. "\"" (subst "\\" "\\\\" "\"" "\\\"" "\n" "\\n" "\t" "\\t"
                      "\x0d" "\\x0d" str) "\"")))


;; Split STR into multiple words after each "!%C" where C is a word
;; in CODES, and convert each "!%C" to "!:C".
;;
(define (vsp-split codes str)
  ;; Okay to pass NIL for C.
  (define `(split c s)
    (subst (.. "!%" (or c "%")) (.. "!:" c " ") s))

  (split (word 1 codes)
         (split (word 2 codes)
                (if (word 3 codes)
                    (vsp-split (nth-rest 3 codes) str)
                    str))))


;; Like `vsprintf`, but without any built-in notion of format
;; codes.  Instead it accepts two additional arguments:
;;
;; CODES = list of supported format codes, e.g. "s q" for "%s" and "%q".\
;; FMT-FN = a function that formats a value, given a format code.  It is
;;    called as (FMT-FN CODE VALUE-VEC) where VALUE-VEC is a demoted value,
;;    and returns a vector of strings (which will be concatenated).
;;
(define (vsprintfx fmt values codes fmt-fn)
  &public
  ;; Each field = "TEXT!:CODE", but last one may not have "!:CODE".
  (define `fields
    (subst "!%" "%" (vsp-split codes (subst "%" "!%" "!%!%" "%" [fmt]))))

  (concat-vec
   (foreach w (join fields (addprefix "!:%" values))
            ;; W = "TEXT!:C!:%VALUE" or "TEXT!:C" or "TEXT!:%VALUE" or "TEXT"
            ;; TEXT may be "".
            (.. (word 1 (subst "!:" " !. " w))
                (if (findstring "!:" (subst "!:%" nil w))
                    (fmt-fn (word 2 (subst "!:" " " (.. "x" w)))
                            (word 2 (subst "!:%" " " w))))))))


;; Expand FMT, replacing escape sequences with values from vector VALUES,
;; returning the resulting string.
;;
;; The following escape sequences are supported:
;; -  `%s` -> value
;; -  `%q` -> `(format value)`
;;
(define (vsprintf fmt values)
  &public
  (define `(vsp-format code value-vec)
    (if (filter "q" code)
        [(format (promote value-vec))]
        value-vec))

  (vsprintfx fmt values "s q" vsp-format))


;; Like `vsprintf`, but values are provided as separate arguments.
;;
(define (sprintf fmt ...values)
  &public
  (vsprintf fmt values))


;; Display a message to stdout, followed by a newline.  See `vsprintf` for
;; handling of FMT and VALUES.
;;
(define (printf fmt ...values)
  &public
  (info (vsprintf fmt values)))


;; Compare A to B, and if unequal display diagnostics and terminate
;; execution.  This function is exported to enable clients to easily
;; implement variants of `expect`, since FILE-LINE data must come
;; from a macro in order to reflect the location of the caller.
;;
;; FILE-LINE = "file:line:" prefix for the diagnostic message.
;;
(define (expect-x a b file-line)
  &public
  (if (eq? a b)
      (if (findstring "O" SCAM_DEBUG)
          (print file-line ": OK: " a))
      (begin
        (print file-line ": error: assertion failed\n"
               "A: " (format a) "\n"
               "B: " (format b) "\n\n"
               "Raw:\n"
               "A: " a "\n"
               "B: " b "\n")
        (if (findstring "K" SCAM_DEBUG)
            (at-exit error 1)
            (error "")))))


;; Compare A to B; if they are not equal, display diagnostics and terminate
;; execution.
;;
(define `(expect a b)
  &public
  (expect-x a b (current-file-line)))


;; Like `expect`, but evaluation of A and B is done with tracing enabled.
;;
(define `(trace-expect a b)
  &public
  (let ((ab (tracing "%" [a b])))
    (expect (nth 1 ab) (nth 2 ab))))


(define (assert-x cond file-line)
  (or cond
      (error (print file-line ": error: assertion failed"))))

;; If COND is nil, display diagnostics and terminate execution.
;;
(define `(assert cond)
  &public
  (assert-x cond (current-file-line)))


;; Like `expect`, but only the formatted versions of A and B are compared.
;; This accommodates only minor differences in the concrete layout that do
;; not affect the meaning in some contexts.  For example, a record ending in
;; a `&list` member (that is empty) will have a trailing space when
;; constructed, but not after being retrieved from another record (when
;; stored as a trailing `&list` parameter).
;;
(define `(fexpect a b)
  &public
  (expect-x (format a) (format b) (current-file-line)))


;; Return 1 if SUBSTR appears within STR.  Print a diagnostic otherwise.
;; This is intended for use in unit tests, as follows:
;;
;;     (expect 1 (see SUBSTR STR))
;;
(define (see substr str)
  &public
  (if (findstring substr str)
      1
      (begin (print "Expected: " (subst "\n" "\n          " substr))
             (print "  Within: " (subst "\n" "\n          " str)))))


(define (uniq-x lst)
  (if lst
      (.. (word 1 lst) " " (uniq-x (filter-out (word 1 lst) (rest lst))))))


;; Return the unique members of VEC *without* re-ordering.  The first
;; occurrence of each member is retains.  This can be applied to word lists
;; or dictionaries as well.
;;
;; The `sort` function returns unique items and is much faster, but it does
;; not preserve ordering.
;;
(define (uniq vec)
  &public
  (subst "~p" "%" "~1" "~"
         (strip (uniq-x (subst "~" "~1" "%" "~p" vec)))))


;; Split STR at each occurrence of DELIM.  Returns vector whose length is
;; one more than the number of occurrences of DELIM.
;;
(define (split delim str)
  &public
  ;; Encode "!" as "!! and " " as " ! "
  (define `(enc s)
    (subst "!" "!!" " " " ! " s))
  (define `(tovec s)
    (subst "!x" nil
           (patsubst "!x" "!."
                     (.. "!x" (subst " ! " "!0" "!!" "!1" "\t" "!+" s)))))
  (tovec (subst (enc delim) " !x" (enc str))))


;; Add one to N.  N must contain only decimal digits.
;;
(define (1+ n)
  &public
  (cond
   ((filter "%1 %2 %3 %4" n)
    (subst "4~" 5 "3~" 4 "2~" 3 "1~" 2 (.. n "~")))

   ((filter "%5 %6 %7" n)
    (subst "7~" 8 "6~" 7 "5~" 6 (.. n "~")))

   ((findstring "9~" (.. n "~"))
    (.. (1+ (or (subst "9~" "" (.. n "~")) 0)) 0))

   (else (patsubst "%0" "%1" (patsubst "%8" "%9" n)))))


;---- memoize ----

(define (mcache varname func a b c more)
  (if more
      (info "Warning: memoized function passed more than three arguments"))
  (if (not (bound? varname))
      (set-native varname (func a b c)))
  (value varname))


(define (memoenc a ?b ?c)
  (if (or a b c)
      (.. "~~" (subst "~" "~0" a) (memoenc b c))))


;; Memoize a function that accepts up to three arguments.
;;
(define (memoize funcname)
  &public
  (if (not (bound? funcname))
      (info (.. "Warning: [memoize-1] function '" funcname "' not defined."))
      (let ((func (value funcname))
            (varbase (.. "*memo" (memoenc funcname)))
            (funcname funcname))
        (set-native-fn funcname
                     (lambda (a b c d e f g h)
                       (mcache (.. varbase (memoenc a b c)) func a b c
                               (or d e f g h)))))))


;; Sort a vector VALUES in order of increasing `(KEY-FUNC i)` for each item i.
;;
(define (sort-by key-func values)
  &public
  (define `keyed
    (foreach w values
             (.. (demote (key-func (promote w))) "!!" w)))

  (filter-out "%!!" (subst "!!" "!! " (sort keyed))))


;; Return items that match PREFIX or begin with `(.. PREFIX " ")`.
;;
(define (assoc-initial prefix vec)
  &public
  (define `assoc-pct
    (subst "!8" prefix
           (filter "!8 !8!0%"
                   (subst prefix "!8" vec))))

  (promote
   (firstword
    (if (findstring "%" prefix)
        assoc-pct
        (filter (.. prefix " " prefix [" %"]) vec)))))


;; Return the first vector in VECV whose initial items match those in KEY-VEC.
;;
(define `(assoc-vec key-vec vecv)
  &public
  (assoc-initial [key-vec] vecv))


;; Return the first vector in VECV whose first item is KEY.
;;
(define `(assoc key vecv)
  &public
  ;; double-demote requires just one subst more than single demote
  (assoc-initial (subst "!" "!1" [key]) vecv))


;; Return the index of ITEM in VEC, or 0 if ITEM is not found.
;;
(define (index-of vec item)
  &public
  (define `(wrap str) (.. "!_" str "!_"))

  (words
   (subst "!_" " "
          (filter "%!|"
                  (subst (wrap [item])
                         "!_!| "
                         (wrap (subst " " "!_" vec)))))))


;; Apply the two-argument function F to all elements of V, starting at the
;; left with `(F Z <first>)`.  If V is empty, return Z.
;;
(define (foldl f z v)
  &public
  (if (word 1 v)
      (foldl f (f z (first v)) (rest v))
      z))


;; Like `foldl`, but starting from the right with `(F <last> Z)`.
;;
(define (foldr f z v)
  &public
  (if (word 1 v)
      (f (first v) (foldr f z (rest v)))
      z))


;; Insert VALUE into VEC between every two adjacent items.  If VEC is empty,
;; the result is empty.  Otherwise, the result has one less than twice as
;; many elements as VEC.
;;
(define (intersperse value vec)
  &public
  (subst " " (.. " " [value] " ")
         vec))


;; Return a list of N words, constructed by appending copies of V.
;; N must be an integer; if less than one, the result is empty.
;;
(define (repeat-words v n)
  &public
  (if (filter-out "-% 0" n)
      (if (word n (._. v v v))
          (wordlist 1 n (._. v v v))
          (repeat-words (._. v v v) n))))
