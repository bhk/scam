;; # peg: PEG Parser Generator
;;
;; The peg module exports parsing functions, and *generators* that create
;; parsing functions and combine parsing functions.
;;
;; A *parsing* function attempts to "recognize" or "match" a syntactic
;; construct (a "pattern") within a subject (a sequence of symbols).  On
;; success, it may "consume" zero or more symbols.
;;
;; The generators `peg-p` and `peg-empty` construct primitive parsing
;; functions, recognizing just 1 and 0 symbols, respectively.  Other
;; generators like `peg-and`, `peg-or`, and `peg-*` can be used to
;; construct more complicated parsing functions from these primitives.
;;
;; Concretely, parsing functions have this prototype:
;;
;;     (func subject start) -> [next ...captures] | nil
;;
;; SUBJECT is a vector of symbols describing the string.  Before parsing,
;; text must be converted to a vector of symbols.  The `lex` and
;; `gen-lex` functions can be used for this purpose.
;;
;; START and NEXT are indices into the SUBJECT vector.  The difference
;; between NEXT and START is the number of symbols consumed by the match.
;;
;; CAPTURES is the vector of values produced by the parsing function.


(require "core.scm")
(require "string.scm")


(define `(Yes index ?captures)
  (append index captures))


(define (spread-tokens v subj)
  (if v
      (spread-tokens (rest v)
                     (subst (word 1 v) (concat " " (word 1 v) " ") subj))
      (strip-vec subj)))


;; Convert text to a vector of symbols.  TOKENS is a vector including
;; strings that are symbols of interest to the parsing functions and/or
;; strings that delimit those symbols.
;;
;; See also `un-lex`.
;;
(define (lex text tokens)
  &public
  (spread-tokens tokens [text]))


;; Return a function that converts text to a vector of symbols.  See `lex`.
;;
(define (gen-lex tokens)
  &public
  (gen-polysub (for t tokens [t])
               (for t tokens (concat " " [t] " "))
               (lambda (text) [text])))


;; Recover original text from a string of symbols.
;;
(define (un-lex subj)
  &public
  (promote (subst " " nil subj)))


;; Construct PEG empty string.
;;
;; Return a parsing function that always succeeds, consuming no symbols,
;; with captures CAPS.
;;
(define (peg-empty ?caps)
  &public
  (lambda (subj pos)
    (Yes pos caps)))


;; Construct a PEG terminal symbol.
;;
;; Return a parsing function that will succeed and consume a single symbol
;; when `(filter IN (filter-out OUT [SYMBOL]))` is true.  On success, its
;; captures will be CAPS.
;;
;; Examples:
;;
;;  - `(peg-p "%")` matches any symbol.
;;  - `(peg-p "%" nil [1])` matches any symbol, returning the capture [1].
;;  - `(peg-p "%" "\n")` matches any symbol except `"\n"`.
;;
(define (peg-p in ?out ?caps)
  &public
  (cond
   ((and out (eq? "%" in))
    (lambda (subj pos)
      (if (filter-out out (word pos subj))
          (Yes (1+ pos) caps))))

   (out
    (lambda (subj pos)
      (if (filter-out out (filter in (word pos subj)))
          (Yes (1+ pos) caps))))

   (else
    (lambda (subj pos)
      (if (filter in (word pos subj))
          (Yes (1+ pos) caps))))))


(define (match-or subj pos pfv)
  (if pfv
      (or ((first pfv) subj pos)
          (match-or subj pos (rest pfv)))))


;; Match any of MS; return result of first match.
(define (peg-or-v pfv)
  (lambda (subj pos)
    (match-or subj pos pfv)))


;; Construct a PEG prioritized choice.
;;
;; Return a parsing function that will call parsing functions in PFS, one
;; after another until one succeeds.  It returns the first successful
;; result, or `nil` if none of them succeed.
;;
(define (peg-or ...pfs)
  &public
  (peg-or-v pfs))


(define (match-and subj pos pfs caps)
  (if pfs
      (let ((m ((first pfs) subj pos)))
        (define `pm (word 1 m))
        (define `cm (rest m))
        (if m
            (match-and subj pm (rest pfs) (append caps cm))))
      (Yes pos caps)))


;; Match all of PFV in sequence; append all captures
;;
(define (peg-and-v pfv)
  (lambda (subj pos)
    (match-and subj pos pfv nil)))


;; Construct a PEG sequence.
;;
;; Return a parsing function that will call parsing functions in PFS, one
;; after another, until one fails.  After each succes, the next function
;; will be called with the remaining un-consumed symbols.  If all functions
;; in PFS succeed, the resulting position will will be the position
;; resulting from the final match, and the captures returned will be all
;; captures from all functions, appended.
;;
(define (peg-and ...pfs)
  &public
  (peg-and-v pfs))


(define (match* subj pos pf caps)
  (let ((m (pf subj pos)))
    (define `pm (word 1 m))
    (define `cm (rest m))
    (if m
        (match* subj pm pf (append caps cm))
        (Yes pos caps))))


;; Construct PEG zero-or-more repetition.
;;
;; Return a parsing function that will call PF sequentially until it fails,
;; and then succeed.  The resulting position will be what was returned by
;; the last successful match (or the starting position if none matched), and
;; the resulting captures will be all captures from successful matches,
;; appended.
;;
(define (peg-* pf)
  &public
  (lambda (subj pos)
    (match* subj pos pf nil)))


;; Construct PEG negative lookahead.
;;
;; Return a parsing function that will succeed if PF fails and fail if PF
;; succeeds.
;;
(define (peg-not pf)
  &public
  (lambda (subj pos)
    (if (pf subj pos)
        nil
        (Yes pos nil))))


;;--------------------------------
;; Derived operators
;;--------------------------------

;; Construct PEG optional match.
;;
;; Return a parsing function that will always suceed, returning PF's results
;; if PF succeds, or returning the starting position and no captures
;; otherwise.
;;
(define (peg-? pf)
  &public
  (peg-or pf (peg-empty)))


;; Construct PEG positive lookahead.
;;
;; Return a parsing function that will succeed if PF succeeds, but which
;; will not consume any symbols or produce any captures.
;;
(define (peg-at pf)
  &public
  ;; same as: (peg-not (peg-not pf))
  (lambda (subj pos)
    (if (pf subj pos)
        (Yes pos nil))))


;; Construct PEG one-or-more repetition.
;;
;; See `peg-*`.
;;
(define (peg-+ pf)
  &public
  (peg-and pf (peg-* pf)))


;;--------------------------------
;; Captures
;;--------------------------------


;; Construct symbol capture.
;;
;; Return a parsing function that suceeds when PF succeeds, and on success,
;; adds to the set of captures a dictionary pair whose key is NAME and whose
;; value is a vector of all matched symbols.
;;
(define (peg-c name pf)
  &public
  (lambda (subj pos)
    (let ((m (pf subj pos)))
      (define `pm (word 1 m))
      (define `cm (rest m))
      (if m
          (Yes pm (append cm {=name: (butlast (wordlist pos pm subj))}))))))
