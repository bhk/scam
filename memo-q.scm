(require "'core")
(require "'io")
(require "memo" &private)


(define (showdb)
  (print "memo-db:")
  (foreach pair *memo-db* (printf "  %q" pair))
  nil)

;; utility for counting calls
(define *log* nil)
(define (log name)
  (set *log* (concat *log* " " name)))
(define (log-count name)
  (words (filter name *log*)))


;; memo-call without IO

(define (fn-ab a b)
  (log "fn-ab")
  (concat a "$!1" b))

(define t12 (fn-ab 1 2))
(expect t12 (memo-call (global-name fn-ab) 1 2))
(expect 2 (log-count "fn-ab"))
(expect t12 (memo-call (global-name fn-ab) 1 2))
(expect 2 (log-count "fn-ab"))

(memo-call (global-name fn-ab) 1 2)
(expect (read-file "todo")
        (memo-call (global-name read-file) "todo"))


;; IO record & playback, tags, 2 and more stages

(define fetch-tbl {A:123})

;; utilize multiple arguments to validate argument handling
(define (fetch a b c d e f g h)
  (log "fetch")
  (expect 2345678 (concat b c d e f g h))
  (dict-get a fetch-tbl))

(define (test-impure a b)
  (log "test-impure")
  (expect 1 b)
  (concat a "="
          (while (lambda (name) (filter "A B C" name))
                 (lambda (name) (memo-io (global-name fetch) name 2 3 4 5 6 7 8))
                 a)))

(expect "A=123" (test-impure "A" 1))

(set *memo-db* nil)
(set *log* nil)
(expect "A=123" (memo-call (global-name test-impure) "A" 1))
(expect 1 (log-count "test-impure"))
(expect 1 (log-count "fetch"))

;; check playback (success case)
(expect (Result "A=123")
        (memo-playback (concat (global-name test-impure) " A 1")))
(expect 2 (log-count "fetch"))

;; check playback (failure case)
(let-global ((fetch-tbl {A:"B", B:"C", C:456}))
  (expect nil
          (memo-playback (concat (global-name test-impure) " A 1")))
  (expect 3 (log-count "fetch"))

  ;; memo fallback in failure case
  (expect "A=456" (memo-call (global-name test-impure) "A" 1))
  (expect 2 (log-count "test-impure"))
  ;; +1 for playback, +3 for record
  (expect 7 (log-count "fetch")))

;; memo-db now knows two possible outcomes
(set *log* nil)
(expect "A=123" (memo-call (global-name test-impure) "A" 1))
(expect 0 (log-count "test-impure"))
(expect 1 (log-count "fetch"))

(let-global ((fetch-tbl {A:"B", B:"C", C:456}))
  (expect "A=456" (memo-call (global-name test-impure) "A" 1)))
(expect 0 (log-count "test-impure"))
(expect 4 (log-count "fetch"))

;; memo-call nested in memo-call


(define (test-simple-io-bare n)
  (word n "X Y Z"))

(define (test-simple-io n)
  (memo-io (global-name test-simple-io-bare) n))

(declare (test-recur a))

(define (test-recur-raw str)
  (log "test-recur-raw")
  (if (word 3 str)
      str
      (test-recur
       (append str (test-simple-io (words (concat "1 " str)))))))

(define (test-recur a)
  (memo-call (global-name test-recur-raw) a))

(expect "X Y Z" (test-recur nil))
(expect 4 (log-count "test-recur-raw"))
(expect "X Y Z" (test-recur nil))
(expect 4 (log-count "test-recur-raw"))

;; memo save & restore

(define db-old *memo-db*)
(define sav (memo-get-cache))
(memo-set-cache nil)
(expect nil *memo-db*)
(memo-set-cache sav)
(expect db-old *memo-db*)
