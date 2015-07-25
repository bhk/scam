;;--------------------------------------------------------------
;; core : general-purpose functions
;;--------------------------------------------------------------

(declare SCAM_DEBUG)

(define `nil "")

(define (not v)
  &inline
  (if v nil "1"))

(define (eq a b)
  (if (findstring a (findstring b a))
      "1"
      (not (or a b))))

(define (identity a)
  a)

;; Return the parameter that is not nil (unless both or none are nil)
(define (xor a b)
  (if a (if b nil a) b))

(define (first vec)
  &inline
  (promote (word 1 vec)))

;; (nth-rest n vec) == vector starting at `n`th item in `vec`
(define (nth-rest n vec)
  &inline
  (wordlist n 99999999 vec))

(define (rest vec)
  &inline
  (nth-rest 2 vec))

(define (rrest vec)
  &inline
  (nth-rest 3 vec))


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
;; If z == "000", we return:
;;    rev(9001..10000) rev(8001..9000) ... rev(0001..1000)
;;
(define (rev-by-10s list z)
  &private
  (define `(1- n) (word n "0 1 2 3 4 5 6 7 8 9"))
  (define `z+1 (patsubst "%0" "%1" z))
  (define `z/10 (patsubst "0%" "%" z))

  ;; When z="00", (group 3) --> (wordlist 201 300 list)
  (define `(group prefix)
    (wordlist (concat (1- prefix) z+1) (concat prefix z) list))

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
  (define `z (if (word 11 list) (rev-zeroes list 0)))
  (nth-rest 1 (rev-by-10s list z)))


;; Keep applying `fn` to `value` while `(pref value)` is true.
;;
(define (while pred fn value)
  (if (pred value)
      (while pred fn (fn value))
      value))

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


;; Return readable and parseable representation of STR.
;;
(define (format str)
  (if (and (findstring "!" str)
           (eq str (foreach w str (demote (promote w)))))
      (concat "[" (foreach w str (format (promote w))) "]")
    (or (isnumber str)
        (concat "\"" (subst "\\" "\\\\" "\"" "\\\"" "\n" "\\n" "\t" "\\t"
                            str) "\""))))


;;----  printf  ----

(define (printf-warn args)
  &private
  (print "** Warning: bad format string: '" (nth 1 args) "'"))

(define (vsprintf args)
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
             "\nExpected: " (format o)
             "\n     Got: " (format i) "\n")
      (error ""))))

(define `(expect o i)
  (expect-x o i (current-file-line)))


;; Return 1 if substr appears within str.  Print diagnostic otherwise.
(define (see substr str)
  (if (findstring substr str)
      1
      (begin (print "Expected: " (subst "\n" "\n          " substr))
             (print "  Within: " (subst "\n" "\n          " str)))))


;; (bound? VAR) -> 1 if variable VAR is defined
;;
;; Note that VAR must be a string that names the variable, not
;; a quoted symbol: (bound? "map"), not (bound? 'map).
;;
(define (bound? var)
  (if (filter-out "u%" (flavor var)) 1))

;; (count-chars STR SUB) count number of occurrences of SUB in TEXT
(define (count-chars text ch)
  (words (rest (subst (demote ch) ". ." (demote text)))))

;; (count-words STR WORD) --> NUMBER
(define (count-words str pat)
  (words (filter pat str)))


;;---- Hash operations ----


;; A `map` is a vector of pairs: [ KEY!=VAL KEY!=VAL .. ]

;; create/extend new hash
(define (hash-bind key val hash)
  (concat (subst "%" "!8" [key]) "!=" [val]
          (if hash " ")
          hash))

(define (hash-key entry)
  &inline
  (nth 1 (subst "!=" " " entry)))

(define (hash-value entry)
  &inline
  (nth 2 (subst "!=" " " entry)))

;; Return the first key/value pair in hash matching 'key'.
;; Result is encoded according the the has structures internal rules.
(define (hash-find key hash)
  (word 1 (filter (concat (subst "%" "!8" [key]) "!=%") hash)))

;; (hash-get KEY HASH) returns the value associated with KEY in a map.
(define (hash-get key hash default)
  (nth 2 (concat (subst "!=" " " (hash-find key hash))
                 (if default (concat " x " (demote default))))))


;; concatenate one or more (potentially empty) vectors
(define (append a b c d e f g h i)
  (concat
   (concat a (and a b " ") b)
   (if (or c d e f g h i)
       (concat (if (or a b) " ") (append c d e f g h i)))))

(define (compact hash result)
  (if (not hash)
      result
      (let& ((a (word 1 hash))
             (name (word 1 (subst "!=" " " (word 1 hash)))))
            (compact (filter-out (concat name "!=%") (rest hash))
                     (append result a)))))

(define (uniq-x in out)
  &private
  (if in
      (uniq-x (rest in) (concat out " " (filter-out out (word 1 in))))
      (filter "%" out)))


;; Return a vector/wordlist of the unique members of a vector/wordlist.
;; Order is preserved; the first occurrence of each member is retained.
(define (uniq vec)
  (subst "^p" "%" "^1" "^"
         (uniq-x (subst "^" "^1" "%" "^p" vec)
                 "")))


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
  (foreach w
           (sort (foreach w values
                          (concat (demote (key-func (promote w)))
                                  "!!"
                                  w)))
           (word 2 (subst "!!" " " w))))

;; Check the "type" of a "structure".  This assumes the convention used
;; throughout scam sources, in which structures are stored in vectors, with
;; the first element identifying the type.
;;
;; `pat` can contain multiple patterns in order to match one or more types.
;;
(define `(type? pat struct)
  (filter pat (word 1 struct)))
