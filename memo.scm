;;--------------------------------
;; Persistent memoization
;;--------------------------------

;; This module implements memoization (caching of function results) that
;; suports IO operations and persists across program invocations.
;;
;; The `memo-on` macro enters a context within which memoized functions
;; may be called.  It loads previously cached results (if any) from a
;; specified file, evaluates an expression, and then stores the cache back
;; to the file.  Nested calls to memo-on are no-ops; only the final exit
;; from memo-on will save the cache.
;;
;; Functions that perform IO operations can be memoized when those
;; operations are replayable: that is, modification of external state is
;; idempotent.  In order to perform IO, memoized functions may call
;; operations provided herein (memo-read-file, memo-write-file), or
;; construct more custom IO using `memo-io`.


(require "core.scm")
(require "io.scm")
(require "string.scm")


(define *memo-on* nil)        ;; are we in a session?
(define *memo-cache* nil)     ;; ephemeral (per-session) cache
(define *memo-db* nil)        ;; persistent DB
(define *memo-db-file* nil)   ;; currently-loaded file
(define *memo-db-disk* nil)   ;; version currently in file
(define *memo-key* nil)       ;; next key to assign (when recording)
(define *memo-tag* nil)       ;; next tag to use for DB entries
(define *memo-log* nil)       ;; DB entries for currently-recording function
(define *memo-hashes* nil)    ;; hash results during the current session


;; The database key to use for a call initiation
(define `(call-key fname args)
  (concat fname " " args))


(data PMDB
  ;; VALUE is the cached response from the recorded function.
  (Result value)

  ;; TAG tokenizes all previous inputs to the recorded function; it is
  ;;   incorporated into the key for the next lookup
  ;; (OP ...ARGS) returns an additional input used by the recorded function
  ;;   Note: If OP begins with ":", the remainder of OP is a function name
  ;;   called via memo-call.
  (IO tag op &list args))


(declare (memo-do-apply fname args))


(define (memo-playback key)
  (let ((o (dict-get key (concat *memo-cache* " " *memo-db*))))
    (case o
      ((Result v) o)

      ((IO tag op args)
       (define `result (if (filter ":%" op)
                           (memo-do-apply (patsubst ":%" "%" op) args)
                           (name-apply op args)))
       (memo-playback [tag result]))

      (else nil))))



(define (memo-add-entry rec)
  (set *memo-log* (append *memo-log* {=*memo-key*: rec})))


(define (memo-record key fname args)
  (let-global ((*memo-key* key)
               (*memo-log* nil))
    (let ((value (name-apply fname args))
          (key key))
      ;; Save the final DB entry for this recording session.
      (if *memo-key*
          (begin
            (memo-add-entry (Result value))
            (set *memo-db* (append *memo-db* *memo-log*))))
      value)))


(define (memo-drop)
  &public
  (set *memo-key* nil))


;; Re-use an existing IO record, but ensure that it matches fname & args.
;; Since all inputs to the recorded function have matched up to this point,
;; the IO requested by the function must match (or else memoization is
;; invalid).
(define (get-io-tag io-record fname args)
  (define `io-tag
    (case io-record ((IO t _ _) t)))

  (if io-record
      (begin
        (expect io-record (IO io-tag fname args))
        io-tag)))


(define (memo-log-io fname args result)
  (define `prior-record
    (dict-get *memo-key* *memo-db*))
  (define `tag
    (or (get-io-tag prior-record fname args)
        (begin
          (memo-add-entry (IO *memo-tag* fname args))
          (set *memo-tag* (1+ *memo-tag*) *memo-tag*))))

  (if *memo-key*
      (set *memo-key* [tag result]))
  result)


(define `(memo-log-call fname args result)
  (memo-log-io (concat ":" fname) args result))


;; Perform an IO operation.  Log the IO as an additional input to the
;; function being recorded (if there is one).
;;
(define (memo-io fname ...args)
  &public
  (memo-log-io fname args (name-apply fname args)))


(define (memo-do-apply fname args)
  (define `(playback-or-record fname args)
    (let ((o (memo-playback (call-key fname args)))
          (fname fname)
          (args args))
      (case o
        ((Result v) v)
        (else (memo-record (call-key fname args) fname args)))))

  (let ((pair (dict-find (call-key fname args) *memo-cache*))
        (fname fname)
        (args args))
    (if pair
        (dict-value pair)
        (let ((value (playback-or-record fname args))
              (key (call-key fname args)))
          (set *memo-cache* (append {=key: value} *memo-cache*))
          value))))


;; Call (FNAME ...ARGS), or return cached results.
;;
(define (memo-apply fname args)
  &public
  (if *memo-on*
      (memo-log-call fname args (memo-do-apply fname args))
      (begin
        ;; TODO: allow or forbid, don't warn
        (print "WARNING: memo-not-init: (" fname " " args ")")
        (name-apply fname args))))


;; Call (FNAME ...ARGS), or return cached results.
;;
(define (memo-call fname ...args)
  &public
  (memo-apply fname args))


;;---------------------------------------------------------------
;; Session management
;;---------------------------------------------------------------


(define `(memo-db-encode data)
  (concat (subst "\n" "!n" " " "\n" "!0" " " data) "\n"))

(define `(memo-db-decode data)
  (strip-vec (subst " " "!0" "\n" " " "!n" "\n" data)))


(define `(memo-read-db dbfile)
  (set *memo-db-file* dbfile)
  (set *memo-db* (memo-db-decode (read-file dbfile)))
  (set *memo-tag* (or (word 1 *memo-db*) 0))
  (set *memo-db* (nth-rest 2 *memo-db*))
  (set *memo-db-disk* *memo-db*))


(define `(memo-write-db)
  (define `memo-file-data
    (concat *memo-tag* "\n"
            (memo-db-encode *memo-db*)))

  (expect nil (write-file *memo-db-file* memo-file-data))
  (set *memo-db-disk* *memo-db*))


(define (memo-start-session dbfile)
  (set *memo-on* 1)
  (if (not (eq? *memo-db-file* dbfile))
      ;; load data from new (or initial) DB file
      (memo-read-db dbfile)))


(define `(memo-end-session)
  (set *memo-on* nil)
  (set *memo-cache* nil)
  (set *memo-hashes* nil)
  (if (not (eq? *memo-db* *memo-db-disk*))
      (memo-write-db)))


(define `(memo-plus dbfile)
  (if *memo-on*
      nil
      (begin
        (memo-start-session dbfile)
        1)))


(define (memo-minus end-session? value)
  (if end-session?
      (memo-end-session))
  value)


;; Evaluate EXPR with memoization initialized.  If a session is not active,
;; begin one using DBFILE as the database.  DBFILE will be evaluated only
;; when a session is initiated.
;;
(define `(memo-on dbfile expr)
  &public
  (memo-minus (memo-plus dbfile) expr))


;;---------------------------------------------------------------
;; File IO
;;---------------------------------------------------------------

(define (hash-add pairs)
  (set *memo-hashes* (append *memo-hashes* pairs)))


(declare (do-memo-hash-file))


;; Get vector of all filenames hashed in IO ops in the DB
;;
(define `files-in-db
  (foreach pair *memo-db*
           (case (dict-value pair)
             ((IO tag op args)
              (if (eq? op (native-name do-memo-hash-file))
                  (word 1 args))))))


(define (do-memo-hash-file filename)
  (if *memo-on*
      (begin
        (dict-value
         (or (dict-find filename *memo-hashes*)
             (begin
               (define `names
                 (sort (cons filename (if *memo-hashes* nil files-in-db))))
               (hash-add (hash-files names))
               (dict-find filename *memo-hashes*)))))
      (hash-file filename)))


;; Return hash of file contents, logging the IO transaction for playback.
;;
(define (memo-hash-file filename)
  &public
  (memo-io (native-name do-memo-hash-file) filename))


;; Read data from FILENAME, logging the IO transaction for playback.
;;
(define (memo-read-file filename)
  &public
  (memo-hash-file filename)
  (read-file filename))


;; Write data to FILENAME, logging the IO transaction for playback.
;;
(define (memo-write-file filename data)
  &public
  ;; Remove any previously-read hash.
  (set *memo-hashes* (dict-remove filename *memo-hashes*))
  (let ((result (write-file filename data))
        (filename filename))
    (memo-hash-file filename)
    result))
