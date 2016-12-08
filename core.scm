;;--------------------------------------------------------------
;; core : general-purpose functions
;;--------------------------------------------------------------

(declare SCAM_DEBUG &global)

(define (eq a b)
  (define `aa (concat 1 a))
  (define `bb (concat 1 b))
  (if (findstring aa (findstring bb aa))
      1))

(define (identity a)
  &inline
  a)

;; Return the parameter that is not nil (unless both or none are nil)
(define (xor a b)
  (if a (if b nil a) b))

;; concatenate strings in VEC, separating them with DELIM
(define (concat-vec vec delim)
  (promote (subst " " (demote delim) vec)))

;; add an item to the front of a vector
(define (cons item vec)
  (concat (demote item) (if vec " ") vec))

;; add `item` to end of a vector `vec`
(define (conj vec item)
  (concat vec (if vec " ") (demote item)))

;; return last item in vector `vec`
(define (last vec)
  &inline
  (promote (lastword vec)))

;; Remove redundant spaces and tabs without removing "\n" characters.  Use
;; this instead of `strip` when operating on vectors.  Word demotion does
;; not encode newline characters.
;;
(define `(strip-vec vec)
  (filter "%" vec))

;; `butlast` is like `rest`, but from the end of the vector
(define (butlast vec)
  (wordlist 2 (words vec) (concat "X " vec)))

(define (map-call funcname vec)
  (for x vec (call funcname x)))

;; `select-vec` = return vector of items for which (fn item) is true
(define (select-vec fn list)
  (filter-out "!" (foreach dx list
                           (if (fn (promote dx)) dx "!"))))

;; `select-words` = return new list of words for which (fn word) is true
(define (select-words fn list)
  ;; wrap in outer 'foreach' to eliminate redundant spaces
  (foreach a
           (foreach x list (if (fn x) x))
           a))


;; first non-nil member of vec
(define (vec-or vec)
  &inline
  (first (filter-out [""] vec)))

(define (indicesX in out)
  (if (word (words out) in)
      (concat (words out) " " (indicesX in (concat ". " out)))))

;; return list of indices, one for each word in list
(define (indices list)
  &inline
  (indicesX list "." 1))


;; Reverse a list in groups sized by powers of ten.
;;
;; z=""    -->      10       9   ...     2       1
;; z="0"   ->    91-100   81-90  ...  11-20   01-10
;; z="00"  ->   901-1000 801-900 ... 101-200 001-100
;;
(define (rev-by-10s list z)
  &private
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
  &private
  (if (word (concat 1 z 1) list)
      (rev-zeroes list (concat 0 z))
      z))

;; Simpler implementations of `reverse` are O(n^2) because `rest` is
;; an O(n) operation.
(define (reverse list)
  (nth-rest 1 (rev-by-10s list (rev-zeroes list nil))))


;; Keep applying `fn` to `value` while `(pref value)` is true.
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

(declare (while pred do initial))

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
                         (while-N pred do o (rest level))
                         (while-0 pred do val))
                     level
                     (concat "i" k)))))

  (define (while pred do initial)
    (if (pred initial)
        (let ((new-value (do initial)))
          (nth 2 (while-N pred do (while-0 pred do new-value) 1 "ii")))
        initial)))


(define (isnumber s)
  ;; reduce all digits to '0' and (E|e)[-]<digit> to 'e'
  (define `a (subst 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0
                    "e" "E" "E-" "E" "E0" "e" " " "_" s))
  ;; remove one leading "-"
  (define `b (subst "0" "" (patsubst "-%" "%" a)))
  ;; after removing '.' and 'e', there should be nothing left
  (define `c (patsubst ".%" "%" (patsubst "%e" "%" b)))

  (if (filter "0% 1% 2% 3% 4% 5% 6% 7% 8% 9%" (subst "-" "" s))
      (if c "" s)))


;; concatenate one or more (potentially empty) vectors, word lists, or
;; hashes.
;;
(define (append a b c d e f g h)
  ;; "$9" is a vector of arguments beyond the eighth
  (define `... (value 9))

  (strip-vec (concat a " " b " " c " " d " " e " " f " " g " " h " "
                     (if ... (promote ...)))))


;;---- Hash operations ----
;;
;; A `hash` is a word list of key/value pairs.  Keys and values are
;; encoded and delimited with "!=".
;;
;; (hash-bind KEY VALUE [HASH])
;;     Bind KEY to VALUE, perpending it to HASH (if given).
;;
;; (hash-get KEY HASH DEFAULT)
;;     Return the value bound to KEY.  If more than one entry matches KEY,
;;     only the first is returned.  If no entry is found, DEFAULT is
;;     returned.
;;
;; (hash-find KEY HASH)
;;     Return the first hash entry matching KEY.  Unlike hash-get, this
;;     indicates whether a match was found (every hash word will be
;;     non-nil).
;;
;; (hash-key ENTRY)
;;     Return the key portion of a hash word.
;;
;; (hash-value ENTRY)
;;     Return the value portion of a hash word.
;;
;; (hash-compact HASH)
;;     Remove entries in a hash that are superseded by earlier entries that
;;     share the same KEY.
;;

(define (hash-bind key val hash)
  (concat (subst "%" "!8" [key]) "!=" [val]
          (if hash " ")
          hash))

(define (hash-key entry)
  &inline
  (promote (subst "!8" "%" (word 1 (subst "!=" " " entry)))))

(define (hash-value entry)
  &inline
  (nth 2 (subst "!=" " " entry)))

(define (hash-find key hash)
  (word 1 (filter (concat (subst "%" "!8" [key]) "!=%") hash)))

(define (hash-get key hash default)
  (nth 2 (concat (subst "!=" " " (hash-find key hash))
                 (if default (concat " x " (demote default))))))

(define (hash-compact hash result)
  (if (not hash)
      result
      (let& ((entry (word 1 hash))
             (prefix (word 1 (subst "!=" "!=% " entry))))
        (append entry
                (hash-compact (filter-out prefix (rest hash)))))))


(declare (format value))

;; Convert h to hash syntax, or return nil if it is not a valid hash.
;;
(define (format-hash h)
  (define `pairs
    (foreach e h
             (begin
               (define `key (hash-key e))
               (define `value (hash-value e))
               [(concat (format key) ": " (format value))])))
  (define `(hash-elem w ndx)
    (nth ndx (subst "!=" " " w)))

  (if (findstring "!=" h)
      (if (eq h (foreach w h (hash-bind (hash-elem w 1) (hash-elem w 2))))
          (concat "{" (concat-vec pairs ", ") "}"))))


;; Extract members from a record, applying `func` to them, appending the
;; result to `accum` (with a single space separator for each).
;;
(define (data-foreach func encodings values accum)
  &private
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
      (let ((pattern (hash-get tag ^tags))
            (values (rest record))
            (tag tag)
            (record record))
        (define `ctor-name (nth 1 pattern))
        (define `encodings (rest pattern))
        (define `reconstructed
          (data-foreach (lambda (v e) (if (eq "S" e) [v] v))
                        encodings values tag))
        (define `arg-text
          (data-foreach (lambda (v e)
                          (if (and (eq "L" e) (not v))
                              "[]"
                              (format v)))
                        encodings values ""))

        (and pattern
             (eq record reconstructed)
             (concat "(" ctor-name " " arg-text ")")))))


;; Return readable and parseable representation of STR.
;;
(define (format str)
  (define `(format-vector str)
    (if (eq str (foreach w str (demote (promote w))))
        (concat "[" (foreach w str (format (promote w))) "]")))

  (or (if (findstring "!" str)
          (or (format-vector str)
              (format-hash str)
              (format-record str)))
      (isnumber str)
      (concat "\"" (subst "\\" "\\\\" "\"" "\\\"" "\n" "\\n" "\t" "\\t"
                          str) "\"")))


;;----  printf  ----
;;
;; Usage:
;;   (printf FMT VALUE...)
;;
;; Format expressions:
;;   %s  ->  argument as-is
;;   %q  ->  describe argument with SCAM literal or vector syntax
;;           (number, string, or vector)

(define (vsprintf args)
  (define `(printf-warn args)
    (print "** Warning: bad format string: '" (nth 1 args) "'"))

  (define `fields
    (subst "%" " !%" " !% !%" "%" (concat "%s" (word 1 args))))

  (concat-vec
   (foreach w (join (concat "!. " (rest args)) fields)
            (if (findstring "!%s" w)
                (subst "!%s" "" w)
                (if (findstring "!%q" w)
                    (concat (demote (format (first (subst "!%q" " " w))))
                            (word 2 (subst "!%q" "!. " w)))
                    (if (findstring "!%" w)
                        ;; "!%x" => bad format string
                        ;; otherwise, it's an arg without a format field
                        (begin (printf-warn args)
                               (word 2 (subst "!%" "! %" w)))))))))

(define (sprintf format ...)
  (vsprintf *args*))

(define (printf format ...)
  (info (vsprintf *args*)))


(define (expect-x o i file-line)
  (if (eq o i)
      (if (findstring "O" SCAM_DEBUG)
          (print "OK: " (format o)))
    (begin
      (print file-line ": error: assertion failed"
             "\nA: " (format o)
             "\nB: " (format i) "\n")
      (error ""))))

(define `(expect o i)
  (expect-x o i (current-file-line)))


;; Return 1 if substr appears within str.  Print diagnostic otherwise.
(define (see substr str)
  (if (findstring substr str)
      1
      (begin (print "Expected: " (subst "\n" "\n          " substr))
             (print "  Within: " (subst "\n" "\n          " str)))))


;; Return a vector/wordlist of the unique members of a vector/wordlist.
;; Order is preserved; the first occurrence of each member is retained.
(declare (uniq vec))

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
  (foreach w
           (subst [delim] "!. !." [str])
           (or (subst "!." "" w) "!.")))

;; (1+ NUM) : add one to a non-negative integer
(define (1+ n)
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
  &private
  (if more
      (info "Warning: memoized function passed more than three arguments"))
  (if (not (bound? varname))
      (set-global varname (func a b c)))
  (value varname))


(define (memoenc a b c)
  &private
  (if (or a b c)
      (concat "~~" (subst "~" "~0" a) (memoenc b c))))


;; memoize a function with up to three arguments
;;
(define (memoize funcname)
  (if (not (bound? funcname))
      (info (concat "Warning: [memoize-1] function '" funcname "' not defined."))
      (let ((func (value funcname))
            (varbase (concat "*memo" (memoenc funcname)))
            (funcname funcname))
        (set-rglobal funcname
                     (lambda (a b c d e f g h)
                       (mcache (concat varbase (memoenc a b c)) func a b c
                               (or d e f g h)))))))

(define (sort-by key-func values)
  (define `keyed
    (foreach w values
             (concat (demote (key-func (promote w))) "!!" w)))

  (filter-out "%!!" (subst "!!" "!! " (sort keyed))))


;; Check the "type" of a "structure".  This assumes the convention used
;; throughout scam sources, in which structures are stored in vectors, with
;; the first element identifying the type.
;;
;; `pat` can contain multiple patterns in order to match one or more types.
;;
(define `(type? pat struct)
  (filter pat (word 1 struct)))


;; Return items that match prefix or begin with 'prefix %'.
;;
(define (assoc-initial prefix vec)
  (define `assoc-pct
    (subst "!8" prefix
           (filter "!8 !8!0%"
                   (subst prefix "!8" vec))))

  (promote
   (firstword
    (if (findstring "%" prefix)
        assoc-pct
        (filter (concat prefix " " prefix [" %"]) vec)))))


;; Return the first vector whose initial items match those in key-vec.
;;
(define `(assoc-vec key-vec vec)
  (assoc-initial [key-vec] vec))


;; Return the first vector whose first item is `key`, given a vector of
;; vectors.
;;
(define `(assoc key vec)
  ;; double-demote requires just one subst more than single demote
  (assoc-initial (subst "!" "!1" [key]) vec))


;; Return the index of ITEM in VEC, or 0 if ITEM is not found.
;;
(define (index-of vec item)
  (define `(wrap str) (concat "!_" str "!_"))

  (words
   (subst "!_" " "
          (filter "%!|"
                  (subst (wrap [item])
                         "!_!| "
                         (wrap (subst " " "!_" vec)))))))
