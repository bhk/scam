;;--------------------------------------------------------------
;; core : general-purpose functions
;;--------------------------------------------------------------

(declare SCAM_DEBUG &global)

;; Return 1 if A and B are equal, nil otherwise.
(define (eq? a b)
  &public
  (define `aa (concat 1 a))
  (define `bb (concat 1 b))
  (if (findstring aa (findstring bb aa))
      1))

(define (identity a)
  &public
  &inline
  a)

;; Return the parameter that is not nil (unless both or none are nil).
(define (xor a b)
  &public
  (if a (if b nil a) b))

;; Concatenate strings in VEC, separating them with DELIM.
(define (concat-vec vec ?delim)
  &public
  (promote (subst " " (demote delim) vec)))

;; Add ITEM to the front of vector VEC.
(define (cons item vec)
  &public
  (concat (demote item) (if vec " ") vec))

;; Add ITEM to end of vector VEC.
(define (conj vec item)
  &public
  (concat vec (if vec " ") (demote item)))

;; Return the last item in vector VEC.
(define (last vec)
  &public
  &inline
  (promote (lastword vec)))

;; Collapse spaces and tabs, leaving "\n" alone.  This can be used to remove
;; redundant spaces from a vector or dictionary without modifying its
;; contents.  (The Make builtin `strip` will convert newlines to spaces.)
(define (strip-vec vec)
  &public
  &inline
  (filter "%" vec))

;; Return a vector of all items in VEC except for the last one.
(define (butlast vec)
  &public
  (wordlist 2 (words vec) (concat "X " vec)))

;; Return a vector of all members of VEC for which (FN member) is non-nil.
(define (select-vec fn list)
  &public
  (filter-out "!" (foreach dx list
                           (if (fn (promote dx)) dx "!"))))

;; Return a list of words for which (FN word) is non-nil.
(define (select-words fn list)
  &public
  ;; wrap in outer 'foreach' to eliminate redundant spaces
  (foreach a
           (foreach x list (if (fn x) x))
           a))

;; Return the first non-nil member of VEC.
(define (vec-or vec)
  &public
  &inline
  (first (filter-out [""] vec)))

(define (indices-b max len lst)
  (if (filter max len)
      len
      (concat len " " (indices-b max (words lst) (concat ". " lst)))))

(define (indices-a max)
  (if (filter-out 0 max)
      (indices-b max 1 ". .")))

;; Return a list/vector of the indices (1, 2, ...) of words in LST.
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
    (wordlist (concat (1- prefix) z+1)
              (concat prefix z)
              list))

  (if list
      (if z
          (foreach p [10 9 8 7 6 5 4 3 2 1] (rev-by-10s (group p) z/10))
          (foreach p [10 9 8 7 6 5 4 3 2 1] (word p list)))))


;; Detect length of list to the nearest order of magnitude.
;; 0..10 items => "";  11-100 => "0";  101..1000 => "00", ...
(define (rev-zeroes list z)
  (if (word (concat 1 z 1) list)
      (rev-zeroes list (concat 0 z))
      z))

;; Reverse word list (or vector) LIST.
(define (reverse list)
  &public
  (nth-rest 1 (rev-by-10s list (rev-zeroes list nil))))


;; Recursively apply FN to VALUE until (PRED result) is nil.
;;
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
            (while-0 pred do (do value) (concat "i" k0))
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
                (while-N pred do o (concat level " " 0) "ii")
                ;; return to higher level
                o)

            ;; recurse and repeat
            (while-N pred do
                     (if level
                         (while-N pred do o (rest level) nil)
                         (while-0 pred do val nil))
                     level
                     (concat "i" k)))))

  (define (while pred do initial)
    (if (pred initial)
        (let ((new-value (do initial)))
          (nth 2 (while-N pred do (while-0 pred do new-value nil) 1 "ii")))
        initial)))


;; Return S if S is a valid numeric literal in SCAM, nil otherwise.
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


;; Return 1 if N is safe to pass to WORD or WORDLIST.  This means it
;; consists only of decimal digits and is non-zero.
;;
(define (word-index? n)
  &public
  (numeric? (subst "E" "~" "e" "~" "-" "~" "." "~" "0" "" n)))


;; Replace PAT with REPL if STR matches PAT; return nil otherwise.
;;
(define `(filtersub pat repl str)
  &public
  (patsubst pat repl (filter pat str)))


;; Join one or more (potentially empty) vectors, word lists, or
;; dictionaries.
;;
(define (append ?a ?b ?c ?d ?e ?f ?g ?h ...others)
  &public
  (strip-vec (concat a " " b " " c " " d " " e " " f " " g " " h " "
                     (if others (promote others)))))


;;---- Dictionary operations ----
;;
;; A `dictionary` is a word list of key/value pairs.  Keys and values are
;; encoded and delimited with "!=".
;;
;; (dict-get KEY DICT DEFAULT)
;;     Return the value bound to KEY.  If more than one pair matches KEY,
;;     only the first is returned.  If no pair is found, DEFAULT is
;;     returned.
;;
;; (dict-find KEY DICT)
;;     Return the first pair matching KEY.  Unlike dict-get, this
;;     indicates whether a match was found (every pair is non-nil).
;;
;; (dict-key PAIR)
;;     Return the key portion of a pair.
;;
;; (dict-value PAIR)
;;     Return the value portion of a pair.
;;
;; (dict-compact DICT)
;;     Remove pairs in a dictionary that are preceded by pairs that
;;     share the same KEY.
;;
;; (dict-keys DICT)
;;     Return a vector of KEYS from DICT.
;;

(define (dict-key pair)
  &public
  &inline
  (promote (subst "!8" "%" (word 1 (subst "!=" " " pair)))))

(define (dict-value pair)
  &public
  &inline
  (nth 2 (subst "!=" " " pair)))

(define (dict-find key dict)
  &public
  (word 1 (filter (concat (subst "%" "!8" [key]) "!=%") dict)))

(define (dict-get key dict ?default)
  &public
  (nth 2 (concat (subst "!=" " " (dict-find key dict))
                 (if default (concat " x " (demote default))))))

(define (dict-compact dict ?result)
  &public
  (if (not dict)
      result
      (let& ((pair (word 1 dict))
             (prefix (word 1 (subst "!=" "!=% " pair))))
        (append pair
                (dict-compact (filter-out prefix (rest dict)))))))

(define (dict-keys h)
  &public
  (foreach e h
           (subst "!8" "%" (word 1 (subst "!=" " " e)))))

(declare (format value)
         &public)


;; Return STR if it should be displayed as a symbol (versus a quoted
;; string) in a dictionary.
(define (symbol? str)
  (and (findstring str (promote (word 1 str)))
       (not (or (findstring "\n" str)
                (findstring "(" str)
                (findstring ")" str)
                (findstring "[" str)
                (findstring "]" str)
                (findstring "," str)
                (findstring ";" str)
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
               [(concat key-fmt ": " (format value))])))

  (define `(dict-elem w ndx)
    (nth ndx (subst "!=" " " w)))

  (if (findstring "!=" h)
      (if (eq? h (foreach w h
                          {(dict-elem w 1): (dict-elem w 2)} ))
          (concat "{" (concat-vec pairs ", ") "}"))))


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
                    (concat accum (if accum " ") (func value e)))
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
             (eq? (strip-vec record) (strip-vec reconstructed))
             (concat "(" ctor-name (if encodings " ") arg-text ")")))))

(define *format-funcs* nil)

;; Register a formatting function that will be used by `format`.
(define (format-add func)
  &public
  (set *format-funcs* (cons func *format-funcs*)))

(define (format-custom str funcs)
  (if funcs
      (or ((first funcs) str)
          (format-custom str (rest funcs)))))

;; Return the most readable SCAM source representation of STR.
(define (format str)
  &public
  (define `(format-vector str)
    (if (eq? str (foreach w str (demote (promote w))))
        (concat "[" (foreach w str (format (promote w))) "]")))

  (or (format-custom str *format-funcs*)
      (if (findstring "!" str)
          (or (format-dict str)
              (format-record str)))
      (if (or (findstring "!" str)
              (and (findstring " " str)
                   (numeric? (subst " " "" str))))
          (format-vector str))
      (numeric? str)
      (concat "\"" (subst "\\" "\\\\" "\"" "\\\"" "\n" "\\n" "\t" "\\t"
                          str) "\"")))


;; Expand FMT, replacing escape sequences with values from vector VALUES,
;; returning the resulting string.
;;
;; The following escape sequenaces are supported:
;;   %s  ->  value
;;   %q  ->  (format value)
;;
(define (vsprintf fmt values)
  &public

  (define `fields
    (subst "%" " !%" " !% !%" "%" (concat "%s" [fmt])))

  (concat-vec
   (foreach w (join (concat "!. " values) fields)
            (cond
             ((findstring "!%s" w)
              (subst "!%s" "" w))
             ((findstring "!%q" w)
              (cons (format (first (subst "!%q" "!. " w)))
                    (word 2 (subst "!%q" "!. " w))))
             ((findstring "!%" w)
              ;; "!%x" => bad format string
              (subst "!%" "[unknown % escape]%" w))))))

;; Like `vsprintf`, but values follow as arguments following FMT.
(define (sprintf fmt ...values)
  &public
  (vsprintf fmt values))


;; Like `sprintf`, but the result is written to stdout using `info` (note
;; that this will unavoidably append an additional newline.)
(define (printf format ...values)
  &public
  (info (vsprintf format values)))


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
        (if (not (findstring "K" SCAM_DEBUG))
            (error "")))))

;; Compare A to B; if they are not equal, display diagnostics and terminate
;; execution.
(define `(expect a b)
  &public
  (expect-x a b (current-file-line)))

;; Like `expect, but only the formatted versions of A and B are compared.
;; This accommodates only minor differences in the concrete layout that do
;; not affect the meaning in some contexts.  For example, a record ending in
;; a &list member (that is empty) will have a trailing space when
;; constructed, but not after being retrieved from another record (when
;; stored as a trailing &list parameter).
(define `(fexpect a b)
  &public
  (expect-x (format a) (format b) (current-file-line)))

;; Return 1 if SUBSTR appears within STR.  Print a diagnostic otherwise.
(define (see substr str)
  &public
  (if (findstring substr str)
      1
      (begin (print "Expected: " (subst "\n" "\n          " substr))
             (print "  Within: " (subst "\n" "\n          " str)))))


;; Return a vector/wordlist of the unique members of a vector/wordlist.
;; Order is preserved; the first occurrence of each member is retained.  The
;; Make builtin `sort` returns unique items and is much faster, but it does
;; not preserve ordering.
(declare (uniq vec)
         &public)

(begin
  (define (uniq-x lst)
    (if lst
        (concat (word 1 lst) " " (uniq-x (filter-out (word 1 lst) (rest lst))))))

  (define (uniq vec)
    (subst "~p" "%" "~1" "~"
           (strip-vec (uniq-x (subst "~" "~1" "%" "~p" vec))))))


;; Split STR at each occurrence of DELIM.  Returns vector whose length is one
;; more than the number of occurrences of DELIM.
(define (split delim str)
  &public
  ;; Ensure that the end or start of delim cannot overlap part of an escape
  ;; sequence.  In an encoded string, "{" and "}" appear only in the
  ;; following escape sequences: {L} {R} {s} {t} {}
  (define `(enc str)
    (or (subst "{" "{L" "}" "{R}" "{L" "{L}" " " "{s}" "\t" "{t}" str) "{}"))
  (define `(dec str)
    (subst "{}" "" "{t}" "\t" "{s}" " " "{L}" "{L" "{R}" "}" "{L" "{" str))

  (foreach w (subst (enc delim) "{} {}" (enc str))
           [(dec w)]))


;; Add one to N.  N must contain only decimal digits.
(define (1+ n)
  &public
  (cond
   ((filter "%1 %2 %3 %4" n)
    (subst "4~" 5 "3~" 4 "2~" 3 "1~" 2 (concat n "~")))

   ((filter "%5 %6 %7" n)
    (subst "7~" 8 "6~" 7 "5~" 6 (concat n "~")))

   ((findstring "9~" (concat n "~"))
    (concat (1+ (or (subst "9~" "" (concat n "~")) 0)) 0))

   (else (patsubst "%0" "%1" (patsubst "%8" "%9" n)))))


;---- memoize ----

(define (mcache varname func a b c more)
  (if more
      (info "Warning: memoized function passed more than three arguments"))
  (if (not (bound? varname))
      (set-global varname (func a b c)))
  (value varname))


(define (memoenc a ?b ?c)
  (if (or a b c)
      (concat "~~" (subst "~" "~0" a) (memoenc b c))))


;; Memoize a function with up to three arguments.
(define (memoize funcname)
  &public
  (if (not (bound? funcname))
      (info (concat "Warning: [memoize-1] function '" funcname "' not defined."))
      (let ((func (value funcname))
            (varbase (concat "*memo" (memoenc funcname)))
            (funcname funcname))
        (set-rglobal funcname
                     (lambda (a b c d e f g h)
                       (mcache (concat varbase (memoenc a b c)) func a b c
                               (or d e f g h)))))))

;; Sort a vector VALUES in order of increading (KEY-FUNC i) for each item i.
(define (sort-by key-func values)
  &public
  (define `keyed
    (foreach w values
             (concat (demote (key-func (promote w))) "!!" w)))

  (filter-out "%!!" (subst "!!" "!! " (sort keyed))))


;; Return items that match prefix or begin with 'PREFIX %'.
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
        (filter (concat prefix " " prefix [" %"]) vec)))))


;; Return the first vector in VEC whose initial items match those in KEY-VEC.
(define `(assoc-vec key-vec vec)
  &public
  (assoc-initial [key-vec] vec))


;; Return the first vector in VEC whose first item is KEY.
(define `(assoc key vec)
  &public
  ;; double-demote requires just one subst more than single demote
  (assoc-initial (subst "!" "!1" [key]) vec))


;; Return the index of ITEM in VEC, or 0 if ITEM is not found.
(define (index-of vec item)
  &public
  (define `(wrap str) (concat "!_" str "!_"))

  (words
   (subst "!_" " "
          (filter "%!|"
                  (subst (wrap [item])
                         "!_!| "
                         (wrap (subst " " "!_" vec)))))))


;; Apply two-argument function F to all elements of V, starting at the left
;; with (F Z <first>).  If there is only one element, return it.  If V is
;; empty, return Z.
(define (foldl f z v)
  &public
  (if (firstword v)
      (foldl f (f z (first v)) (rest v))
      z))


;; Like foldl, but starting from the right with (F <last> Z).
(define (foldr f z v)
  &public
  (if (firstword v)
      (f (first v) (foldr f z (rest v)))
      z))

;; Insert VALUE into VEC between every two adjacent items.  If VEC is empty
;; the result, is empty.  Otherwise, the result has one less than twice as
;; many elements as VEC.
(define (intersperse value vec)
  &public
  (subst " " (concat " " [value] " ")
         vec))
