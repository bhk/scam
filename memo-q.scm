(require "core.scm")
(require "io.scm")
(require "string.scm")
(require "memo.scm" &private)


(define TMPDIR
  (define `test-dir (assert (value "TEST_DIR")))
  (concat (shell (concat "mktemp -d " test-dir "memo-q.XXXX")) "/"))

(define db-file (concat TMPDIR "db.txt"))

;; Utilities

(define (showdb)
  (print "memo-db:")
  (foreach pair *memo-db* (printf "  %q" pair))
  nil)


(define *log* nil)

;; Increment count for event EVT
(define (log evt) (set *log* (concat *log* " " evt)))

;; Get number of occurrences of event EVT
(define (log-count evt) (words (filter evt *log*)))


;; Construct a session on DB-FILE and clear *log*
;;
(define `(memo-session expr)
  (let-global ((*log* nil))
    (memo-on db-file expr)))


(define (reset-cache)
  (write-file db-file nil)
  (set *memo-db* nil)
  (set *memo-db-disk* nil)
  (set *memo-tag* 0))


;; Assert: memo-new enters and exits a memo session.
(reset-cache)
(expect nil *memo-on*)
(memo-session
 (begin
   (expect 1 *memo-on*)
   (expect nil *memo-db*)
   (expect 0 *memo-tag*)))
(expect nil *memo-on*)


;; Assert: Tag is successfully saved/restored on session exit/entry, and
;; does not pollute DB.

(memo-session
 (begin
   (set *memo-tag* 23)
   (set *memo-db* {xyz:789})))

(expect 1 (see 23 (read-file db-file)))
(set *memo-tag* nil)
(memo-read-db db-file)
(expect 23 *memo-tag*)
(expect {xyz:789} *memo-db*)


(memo-session
 (begin
   (expect 23 *memo-tag*)
   (expect nil (findstring 23 *memo-db*))

   ;; Assert: Nested session activations do not re-load/re-save (only the
   ;; top-level does).
   (set *memo-tag* 77)
   (memo-session (expect 77 *memo-tag*))
   (expect nil (findstring 77 (read-file db-file)))))


;;----------------------------------------------------------------
;; Pure function
;;----------------------------------------------------------------

(define (fn-ab a b)
  (log "fn-ab")
  ;; include some potentially problematic encoding cases
  (concat a "$!1 \n\t" b " "))

(define ab-out (fn-ab 1 2))

(memo-session
 (begin
   ;; Assert: Calling through memo-call (first time) returns the same
   ;; results as a non-memoized call.
   (expect ab-out (memo-call (native-name fn-ab) 1 2))
   (expect 1 (log-count "fn-ab"))
   ;; Assert: Calling through memo-call a second time with the same function
   ;; and arguments returns the previous result with no re-invocation of the
   ;; function.
   (expect ab-out (memo-call (native-name fn-ab) 1 2))
   (expect 1 (log-count "fn-ab"))))


;; Assert: Cached results are successfully saved and restored between
;; sessions.
(memo-session
 (begin
   (expect ab-out (memo-call (native-name fn-ab) 1 2))
   (expect 0 (log-count "fn-ab"))))


;; Assert: On session end, ephemeral cache is dropped.
(expect nil *memo-cache*)


;;----------------------------------------------------------------
;; IO record & playback, tags, 2 and more stages
;;----------------------------------------------------------------

(define fetch-tbl {A:123})

;; Assert: Many IO function arguments are supported and correctly passed
;; during record and playback modes. [TODO: i j]
(define (fetch a b c d e f g h i j)
  (log "fetch")
  (expect 2345678910 (concat b c d e f g h i j))
  (dict-get a fetch-tbl))

(define (fa a b)
  (log "fa")
  (concat a "="
          (while (lambda (name) (filter "A B C" name))
                 (lambda (name) (memo-io (native-name fetch) name 2 3 4 5 6 7 8 9 10))
                 a)))

;; Assert: memo-io works outside of memo context, and does not disturb
;; cache.
(expect nil *memo-on*)
(expect "A=123" (fa "A" 1))
(expect nil *memo-on*)


(memo-session
 (begin
   (expect "A=123" (memo-call (native-name fa) "A" 1))
   (expect 1 (log-count "fa"))
   (expect 1 (log-count "fetch"))

   ;; Assert: After a function call is recorded within this session, a
   ;; matching call returns a result without without IO replay or
   ;; re-invocation.
   (expect "A=123" (memo-call (native-name fa) "A" 1))
   (expect 1 (log-count "fa"))
   (expect 1 (log-count "fetch"))

   ;; Assert: When function arguments do not match a cached value, it is not
   ;; returned.
   (expect "A=123" (memo-call (native-name fa) "A" 2))
   (expect 2 (log-count "fa"))
   (expect 2 (log-count "fetch"))))


(memo-session
 (begin
   ;; Assert: In subsequent session with same external state, playback
   ;; succeeds with IO replay and no re-invocation.
   (expect "A=123" (memo-call (native-name fa) "A" 1))
   (expect 0 (log-count "fa"))
   (expect 1 (log-count "fetch"))

   ;; Assert: After successful playback in this session, a matching call
   ;; returns a result without IO replay or re-invocation. (Ephemeral)
   (expect "A=123" (memo-call (native-name fa) "A" 1))
   (expect 0 (log-count "fa"))
   (expect 1 (log-count "fetch"))))


(memo-session
 (let-global ((fetch-tbl {A:"B", B:"C", C:789}))
   ;; Assert: Functions with multiple consecutive IO operations are
   ;; supported.

   ;; Assert: In subsequent session with different external state, playback
   ;; will fail on IO replay and the function will be re-invoked.
   ;; (triggering another IO op).
   (expect "A=789" (memo-call (native-name fa) "A" 1))
   (expect 1 (log-count "fa"))
   (expect 4 (log-count "fetch"))))


;; Assert: DB can represent two possible outcomes based on different
;; external state.  Only IO replay is required, not re-invocation.
(memo-session
 (begin
   (expect "A=123" (memo-call (native-name fa) "A" 1))
   (expect 0 (log-count "fa"))
   (expect 1 (log-count "fetch"))))
(memo-session
 (let-global ((fetch-tbl {A:"B", B:"C", C:789}))
   (expect "A=789" (memo-call (native-name fa) "A" 1))
   (expect 0 (log-count "fa"))
   (expect 3 (log-count "fetch"))))


;;----------------------------------------------------------------
;; Nested memo calls
;;----------------------------------------------------------------

(define *lookup* "X Y Z A Z")
(define (lookup n)
  (log "lookup")
  (word n *lookup*))

(define (inner n)
  (log "inner")
  (memo-io (native-name lookup) 5)
  (memo-io (native-name lookup) n))

(define (outer a b)
  (log "outer")
  (concat (memo-io (native-name lookup) a)
          (memo-call (native-name inner) b)))

(reset-cache)
(memo-session
 (begin
   ;; Assert: Initial call of nested memo-call returns correct value with
   ;; one invocation of each function.
   (expect "XY" (memo-call (native-name outer) 1 2))
   (expect 1 (log-count "outer"))
   (expect 1 (log-count "inner"))
   (expect 3 (log-count "lookup"))

   ;; Assert: (Ephemeral caching of outer) When outer function is passed the
   ;; same arguments in the same session, cached value is returned without
   ;; any IO or reinvocation.
   (expect "XY" (memo-call (native-name outer) 1 2))
   (expect 1 (log-count "outer"))
   (expect 1 (log-count "inner"))
   (expect 3 (log-count "lookup"))

   ;; Assert: (Ephemeral caching of inner) Re-invocation of outer function
   ;; does not cause re-invocation or IO replay of inner function (if its
   ;; inputs are unchanged).
   (expect "AY" (memo-call (native-name outer) 4 2))
   (expect 2 (log-count "outer"))
   (expect 1 (log-count "inner"))
   (expect 4 (log-count "lookup"))))

(memo-session
 (begin
   ;; Assert: In a subsequent session, if the outer function is re-invoked
   ;; but passes the same arguments to the inner function, the inner
   ;; function is replayed but not re-invoked.
   (expect "ZY" (memo-call (native-name outer) 3 2))
   (expect 1 (log-count "outer"))
   (expect 0 (log-count "inner"))
   (expect 3 (log-count "lookup"))))

(memo-session
 (let-global ((*lookup* "X 2 Z A Z"))
   ;; Assert: When the outer function is recorded and the inner function is
   ;; not (due to playback success), the inner function will still be
   ;; treated as a dependency of the outer.

   ;; The previous "ZY" result recorded the outer function while the inner
   ;; was played back, but now the inner function is invalid.
   (expect "Z2" (memo-call (native-name outer) 3 2))
   (expect 1 (log-count "outer"))
   (expect 1 (log-count "inner"))
   (expect 6 (log-count "lookup"))))

(memo-session
 (let-global ((*lookup* "X 3 Z A Z"))
   ;; Assert: When the outer function is recorded and the inner function
   ;; is not (due to ephemeral caching), the inner function will still be
   ;; treated as a dependency of the outer.

   ;; The previous "AY" result recorded the outer function while the inner
   ;; was played back, but now the inner function is invalid.
   (expect "A3" (memo-call (native-name outer) 4 2))
   (expect 1 (log-count "outer"))
   (expect 1 (log-count "inner"))
   (expect 6 (log-count "lookup"))))

(memo-session
 (let-global ((*lookup* "X Y Z A B"))
   ;; Assert: In a subsequent session, if inner function must be re-invoked
   ;; but it returns the same value, the outer function will not be
   ;; re-invoked.
   (expect "XY" (memo-call (native-name outer) 1 2))
   (expect 0 (log-count "outer"))
   (expect 1 (log-count "inner"))
   ;; replay of outer IO (1) + first inner IO (1) + record of inner (2)
   (expect 4 (log-count "lookup"))))


;; Assert: Playback does not log transactions.  [memo-playback validates
;; existing DB entries by re-issuing sub-operations (IOs and memo-call
;; dependencies of the played-back function).  These sub-operations
;; should not be logged as dependencies.]

(define (f0 a) a)
(define (f1 a b) (memo-call (native-name f0) a))
(define (f2 a b c) (memo-call (native-name f1) a b))

(reset-cache)

(memo-session
 (begin
   (expect 5 (memo-call (native-name f2) 5 6 7))
   (expect 5 (words *memo-db*))

   ;; Record new invocation f2, reusing the EPHEMERAL cached results of
   ;; f1 & f2.
   (expect 5 (memo-call (native-name f2) 5 6 70))
   (expect 7 (words *memo-db*))

   ;; Record new invocation f2, reusing the non-EPHEMERAL cached results
   ;; of f1 & f2.
   (set *memo-cache* nil)
   (expect 5 (memo-call (native-name f2) 5 6 700))
   (expect 9 (words *memo-db*))))


;;----------------------------------------------------------------
;; memo-drop
;;----------------------------------------------------------------

;; Assert: memo-drop prevents persistent caching but does not prevent
;; ephemeral caching.

(define (fn-drop flags)
  (log "fn-drop")
  (if (findstring 2 flags)
      (memo-io (native-name lookup) 1))
  (memo-drop)
  (if (findstring 3 flags)
      (memo-io (native-name lookup) 1))
  flags)


(reset-cache)
(memo-session
 (begin
   ;; Drop without IO
   (expect 1 (memo-call (native-name fn-drop) 1))
   (expect 1 (memo-call (native-name fn-drop) 1))
   (expect 1 (log-count "fn-drop"))
   (expect 0 (words *memo-db*))
   ;; Drop preceded by IO
   (expect 2 (memo-call (native-name fn-drop) 2))
   (expect 2 (memo-call (native-name fn-drop) 2))
   (expect 2 (log-count "fn-drop"))
   (expect 0 (words *memo-db*))
   ;; Drop followed by IO
   (expect 3 (memo-call (native-name fn-drop) 3))
   (expect 3 (memo-call (native-name fn-drop) 3))
   (expect 3 (log-count "fn-drop"))
   (expect 0 (words *memo-db*))))


;;----------------------------------------------------------------
;; File IO
;;----------------------------------------------------------------

(define xyz-file (concat TMPDIR "xyz"))

;; memo-hash-file

(write-file xyz-file "xyz")
(define xyz (hash-file xyz-file))

(define (hash-two-files-test)
   (expect xyz (memo-hash-file xyz-file))
   (memo-hash-file db-file))

;; Assert: Calls to memo-hash-file outside a memo session are NOT cached.
(memo-hash-file xyz-file)
(expect nil *memo-hashes*)

;; Assert: Calls to memo-hash-file within a session are cached.
(reset-cache)
(memo-session
 (begin
   ;; caching
   (expect 0 (words *memo-hashes*))
   ;; Call memo-hash-file within a memo-call to create (IO ... "hash-file"
   ;; ...) records (used below)
   (memo-call (native-name hash-two-files-test))
   (expect 2 (words *memo-hashes*))
   (expect xyz (let-global ((hash-file (lambda (f) (assert nil))))
                 (memo-hash-file xyz-file)))))

;; Assert: Cached hash values are flushed on session end.
(expect nil *memo-hashes*)

;; Assert: The first memo-hash-file call within a session will hash all files
;; previously hashed AND the requested file.
(memo-session
 (begin
   ;; hash-batch
   (memo-hash-file (current-file))
   (expect 3 (words *memo-hashes*))
   (expect xyz (let-global ((hash-file (lambda (f) (assert nil))))
                 (memo-hash-file xyz-file)))))


;; memo-read-file

(define abc-file (concat TMPDIR "abc"))
(write-file abc-file "A B C")

(define (file-words name)
  (log "file-words")
  (words (memo-read-file name)))

(reset-cache)

(memo-session
 (begin
   ;; Assert: memo-read-file functions like read-file.
   (expect 3 (memo-call (native-name file-words) abc-file))
   (expect 1 (log-count "file-words"))
   ;; Assert: memo-read-file does not trigger re-evaluate when file does not
   ;; change.
   (expect 3 (memo-call (native-name file-words) abc-file))
   (expect 1 (log-count "file-words"))))

 (memo-session
  (begin
    ;; Assert: memo-read-file *does* trigger re-evaluate when file changes
    ;; (in a subsequent session).
    (write-file abc-file "A B")
    (expect 2 (memo-call (native-name file-words) abc-file))
    (expect 1 (log-count "file-words"))))

;; memo-write-file

(define out-file (concat TMPDIR "out"))
(define (copy-file in out)
  (log "copy-file")
  (memo-write-file out (memo-read-file in)))

(memo-session
 (begin
   ;; Assert: memo-write-file functions like write-file.
   (expect nil (memo-call (native-name copy-file) abc-file out-file))
   (expect 1 (log-count "copy-file"))
   (expect "A B" (read-file out-file))
   ;; Assert: memo-write-file does not trigger re-evaluate when the output
   ;; file has not changed.
   (expect nil (memo-call (native-name copy-file) abc-file out-file))
   (expect 1 (log-count "copy-file"))))

 (memo-session
  (begin
    ;; Assert: When output file has changed, memo-write-file playback
    ;; will restore the intended value without re-invoking.
    (write-file out-file "different")
    (expect nil (memo-call (native-name copy-file) abc-file out-file))
    (expect 0 (log-count "copy-file"))
    (expect "A B" (strip (read-file out-file)))))
