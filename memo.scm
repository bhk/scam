;; # memo: Persistent Memoization
;;
;; The `memo` module implements persistent memoization, which caches the
;; results of function calls to avoid re-computing them.  "Persistent" means
;; that results are available not just during the invocation of a program,
;; but during future invocations as well.  Also, persistent memoization can
;; be applied to functions that perform IO.  SCAM uses persistent
;; memoization to rebuild SCAM programs accurately with with minimal
;; repeated work.  [The `memo` module functionality is not to be confused
;; with the `memoize` function in the `core` library.]
;;
;; `memo-call` and `memo-apply` perform "memoized" invocations of functions.
;; They take a function and a set of arguments, and either invoke the
;; function with the arguments or return a result cached from a previous
;; invocation.  When a cached result is returned, it must also be the case
;; that any side effects (e.g. file writes) from the previous invocation are
;; in effect.
;;
;; In order to meet these requirements, any IO operations performed by the
;; memoized functions must adhere to these requirements:
;;
;; * The IO operation must be logged using a mechanism provided by this
;;   module. The logging mechanisms provided include `memo-io`, which wraps
;;   arbitrary IO operations, or `memo-write-file` and `memo-read-file`,
;;   which provide ready-to-use logged and replayable read and write
;;   operations.
;;
;; * IO operations must be replayable.  Performing the operation two or more
;;   times will result in the same external state as performing it once
;;   does, and will return the same values each time.
;;
;; * IO operations must not be mutually-conflicting during a
;;   [session](#sessions).  One IO operation may not have an effect on
;;   external state that would un-do the effect of another IO operation, or
;;   that would modify the return value of a previously-executed function
;;
;; ## Sessions
;;
;; Memoized functions must be called within the context of a **session*.
;; The `memo-on` function initiates a **session**, evaluates a given
;; expression within the context of that session, and then terminates the
;; session.  (If called during a session, `memo-on` is a no-op.)  At the
;; beginning of a session, previously cached results are read from a
;; specified DB file.  At the end of the session, cached results are written
;; to the DB file.
;;
;; It is assumed that any external state that affects the program will not
;; change during the session.  Using the compiler as an example, when source
;; files are modified during compilation, the compiler cannot guarantee
;; valid results.  This assumption means that each unique IO operation only
;; needs to be performed once during a session.
;;
;; It is assumed that external state *may* change *between* sessions.  When
;; cached results from one session apply to a memoized call in a subsequent
;; session, and the results depend upon IO, those IO operations will be
;; replayed to check the validity of the cached results.  (When external
;; side effects are involved, replaying the IO serves the purpose of
;; re-effecting the change -- so replay does more than just validation.)
;;
;; If a memoized function is called when a session is not active, it is a
;; fatal error.
;;


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
(define *memo-dir* nil)       ;; a directory for BLOBs, if it has been created

(declare (memo-save-session))


;; Return dir containing DB, creating it if necessary.
;;
(define (memo-dir)
  (if (not (eq? *memo-dir* (dir *memo-db-file*)))
      (begin
        (set *memo-dir* (dir *memo-db-file*))
        (mkdir-p *memo-dir*)))
  *memo-dir*)


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


;; Discard memoization results from the current session, preventing them
;; from being persisted.
;;
(define (memo-drop)
  &public
  (set *memo-key* nil)
  (memo-save-session))


;; Re-use an existing IO record, but ensure that it matches fname & args.
;; Since all inputs to the recorded function have matched up to this point,
;; the IO requested by the function must match (or else memoization is
;; invalid).
;;
(define (get-io-tag io-record fname args)
  (define `io-tag
    (case io-record ((IO t _ _) t)))

  (if io-record
        ;; We expect: io-record == (IO io-tag fname args)
        io-tag))


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


;; Call `(FNAME ...ARGS)`, or return cached results.
;;
(define (memo-apply fname args)
  &public
  ;; (assert *memo-on*)
  (memo-log-call fname args (memo-do-apply fname args)))


;; Call `(FNAME ...ARGS)`, or return cached results.
;;
(define (memo-call fname ...args)
  &public
  (memo-apply fname args))


;; Store the result of (FNAME ...ARGS) in a BLOB and return the BLOB name.
;;
(define (blobify fname ...args)
  (save-blob (memo-dir) (name-apply fname args)))


(define (read-blob name)
  (read-file name))

(memoize (native-name read-blob))


;; Memoize a function call that might return a large amount of data.  The
;; return value is stored as an blob in the memo DB directory instead of
;; being stored directly in the memo DB file.  We assume the blobs are
;; retained as long as the DB file.
;;
(define (memo-blob-call fname ...args)
  &public
  (if *memo-on*
      (read-blob (memo-apply (native-name blobify) (cons fname args)))
      (name-apply fname args)))


;;---------------------------------------------------------------
;; Session management
;;---------------------------------------------------------------


(define `(memo-db-encode data)
  (concat (subst "\n" "!n" " " "\n" "!0" " " data) "\n"))

(define `(memo-db-decode data)
  (strip (subst " " "!0" "\n" " " "!n" "\n" data)))


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

  ;; create memo dir if it has not been already
  (memo-dir)
  (expect nil (write-file *memo-db-file* memo-file-data))
  (set *memo-db-disk* *memo-db*))


(define (memo-save-session)
  (if (not (eq? *memo-db* *memo-db-disk*))
      (memo-write-db)))


(define (memo-start-session dbfile)
  (set *memo-on* 1)
  (if (not (eq? *memo-db-file* dbfile))
      ;; load data from new (or initial) DB file
      (memo-read-db dbfile)))


(define `(memo-end-session)
  (set *memo-on* nil)
  (set *memo-cache* nil)
  (set *memo-hashes* nil)
  (memo-save-session))


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


(declare (do-hash-file))
(declare (do-write-blob))


;; Get vector of all filenames hashed in IO ops in the DB
;;
(define `files-in-db
  (foreach pair *memo-db*
           (case (dict-value pair)
             ((IO tag op args)
              (if (filter op (concat (native-name do-hash-file) " "
                                     (native-name do-write-blob)))
                  (word 1 args))))))


(define (do-hash-file filename)
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
  (memo-io (native-name do-hash-file) filename))


;; Read data from FILENAME, logging the IO transaction for playback.
;;
(define (memo-read-file filename)
  &public
  (memo-hash-file filename)
  (read-file filename))


;; Copy a BLOB onto a given file, unless the file is already known to match
;; the BLOB.  Return nil on success.
;;
;; Note: DST comes first to simplify files-in-db.
;;
(define (do-write-blob dst blob)
  (define `hash (notdir blob))

  ;; Remove any cached hash for this file.  Memoized code must not write a
  ;; file after reading/hashing it, but the cache may have been populated
  ;; speculatively (see above), so it might hold a match for FILENAME.
  (if (not (eq? hash (dict-get dst *memo-hashes*)))
      (begin
        (set *memo-hashes* (dict-remove dst *memo-hashes*))
        (cp-file blob dst 1))))


;; Write DATA to FILENAME, logging the IO transaction for playback.  The
;; limit on the size of DATA is system-specific, but at least 60KB for
;; any data and 100KB for text files.
;;
(define (memo-write-file filename data)
  &public
  (if *memo-on*
      (let ((blob (save-blob (memo-dir) data)))
        (memo-io (native-name do-write-blob) filename blob))
      ;; not in a memo session
      (write-file filename data)))


;; Call `chmod-file`, logging the IO as a dependency.  MODE is formatted as
;; per the `chmod` command.
;;
(define (memo-chmod-file filename mode)
  &public
  (memo-io (native-name chmod-file) filename mode))
