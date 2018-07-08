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
(define *memo-db* nil)        ;; persistent DB
(define *memo-db-file* nil)   ;; currently-loaded file
(define *memo-db-disk* nil)   ;; version currently in file
(define *memo-tag* nil)       ;; next tag to use for DB entries
(define *memo-key* nil)       ;; next key to assign (when recording)
(define *memo-cache* nil)     ;; ephemeral (per-session) cache
(define *memo-hashes* nil)


(define (memo-db-add entry)
  (set *memo-db* (append *memo-db* entry)))


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


(declare (memo-apply fname args))


(define (memo-playback key)
  (let ((o (dict-get key (append *memo-cache* *memo-db*))))
    (case o
      ((Result v) o)

      ((IO tag op args)
       (define `result (if (filter ":%" op)
                           (memo-apply (patsubst ":%" "%" op) args)
                           (name-apply op args)))
       (memo-playback [tag result]))

      (else nil))))


(define (memo-record key fname args)
  (let-global ((*memo-key* key))
    (let ((value (name-apply fname args))
          (key key))
      ;; Save the final DB entry for this recording session.
      (memo-db-add {=*memo-key*: (Result value)})
      value)))


(define (memo-log-io fname args result)
  (if *memo-key*
      (let ((result result)
            (prior-entry (dict-get *memo-key* *memo-db*))
            (args args))
        (define `tag
          (if prior-entry
              (let ((prior-tag (case prior-entry ((IO t _ _) t))))
                ;; any existing entry should match
                (if (not (eq? prior-entry (IO prior-tag fname args)))
                    (begin
                      (printf "prior entry = %q" prior-entry)
                      (printf "    current = %q" (IO prior-tag fname args))
                      (assert nil)))
                prior-tag)
              (begin
                (define `entry {=*memo-key*: (IO *memo-tag* fname args)})
                (memo-db-add entry)
                (set *memo-tag* (1+ *memo-tag*) *memo-tag*))))
        (set *memo-key* [tag result])))
  result)


(define `(memo-log-call fname args result)
  (memo-log-io (concat ":" fname) args result))


;; Perform an IO operation.  Log the IO as an additional input to the
;; function being recorded (if there is one).
;;
(define (memo-io fname ...args)
  &public
  (memo-log-io fname args (name-apply fname args)))


(define (memo-apply fname args)
  (define `(playback-or-record fname args)
    (let ((o (memo-playback (call-key fname args)))
          (fname fname)
          (args args))
      (case o
        ((Result v) v)
        (else (memo-record (call-key fname args) fname args)))))

  (memo-log-call
   fname args
   (let ((pair (dict-find (call-key fname args) *memo-cache*))
         (fname fname)
         (args args))
     (if pair
         (dict-value pair)
         (let ((value (playback-or-record fname args))
               (key (call-key fname args)))
           (set *memo-cache* (append {=key: value} *memo-cache*))
           value)))))

;; Call a function *or* return cached results.
;;
(define (memo-call fname ...args)
  &public
  (if *memo-on*
      (memo-apply fname args)
      (begin
        ;; TODO: allow or forbid, don't warn
        (print "WARNING: memo-not-init: (" fname " " args ")")
        (name-apply fname args))))


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

(define *hash-cmd* nil)

(define (hash-cmd)
  (or *hash-cmd*
      (begin
        (define `cmd
          (notdir (or (word 1 (shell "which md5 sha1sum shasum"))
                      (error "no md5, shasum, or sha1sum"))))
        (set *hash-cmd* (subst "md5" "md5 -r" cmd))
        *hash-cmd*)))


;; Write DATA to a file in OBJ-DIR whose name is a function of DATA.
;; Returns the path to the new file.
;;
(define (save-object obj-dir data)
  (define `templ (quote-sh-arg (concat obj-dir "objtmp.XXXXXXXX")))
  (define `cmd
    (concat "( t=$(mktemp " templ ") && "
            (echo-command data) " > \"$t\" && "
            "h=$(" (hash-cmd) " \"$t\") && "
            "n=\"${h:0:16}\" && "
            "mv -f \"$t\" " (quote-sh-arg obj-dir) "\"$n\" && "
            "echo \"$n\""
            ") 2>/dev/null"))
  (addprefix obj-dir (logshell cmd)))


;; Get the first 16 bytes of a hash of FILENAME.
;;
(define (do-hash-file filename)
  (define `cmd
    (concat (hash-cmd) " " (quote-sh-arg filename) " 2>/dev/null"))
  (string-slice 1 16 (word 1 (shell cmd))))


(define (hash-add pairs)
  (set *memo-hashes* (append *memo-hashes* pairs)))

(declare (hash-file))

(define (hash-batch filename)
  (define `hashed-files
    (foreach pair *memo-db*
             (case (dict-value pair)
               ((IO tag op args)
                (if (eq? op (native-name hash-file))
                    (word 1 args))))))
  (define `files
    (sort (append filename hashed-files)))
  (define `cmd
    (concat (hash-cmd) " " files " 2>/dev/null"
            " | sed 's/!/!1/g;s/ /!0/g;s/\t/!+/g'"))

  (set *memo-hashes*
       (foreach dline (shell cmd)
                {(rest (promote dline)): (string-slice 1 16 dline)})))


(define (hash-file filename)
  &public
  (if *memo-on*
      (or (if (not *memo-hashes*)
              (hash-batch filename))
          (dict-get filename *memo-hashes*)
          (foreach hash (do-hash-file filename)
                   (hash-add {=filename: hash})
                   hash))
      (do-hash-file filename)))


;; Read data from FILENAME, logging the IO transaction for playback.
;;
(define (memo-read-file filename)
  &public
  (memo-io (native-name hash-file) filename)
  (read-file filename))


;; Write data to FILENAME, logging the IO transaction for playback.
;;
(define (memo-write-file filename data)
  &public
  ;; Remove any previously-read hash.
  (set *memo-hashes* (dict-remove filename *memo-hashes*))
  (let ((result (write-file filename data)))
    (memo-io (native-name hash-file) filename)
    result))
