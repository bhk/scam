;;--------------------------------
;; Persistent memoization
;;--------------------------------

(require "'core")
(require "'io")


;; Call FUNC-NAME with arguments in vector ARGV.
;;
(define (name-apply func-name argv)
  (define `call-expr
    (concat "$(call " func-name
            (subst " ," ","
                   (foreach n (wordlist 1 (words argv) "1 2 3 4 5 6 7 8")
                            (concat ",$(call ^n," n ",$2)")))
            (if (word 9 argv)
                (concat "," (lambda (_ a) (nth-rest 9 a))))
            ")"))
  (call "if" nil nil call-expr))


(define *memo-db* nil)
(define *memo-key* nil)
(define *memo-tag* 0)


(data PMDB
  ;; VALUE is the cached response from the recorded function.
  (Result value)

  ;; TAG tokenizes all previous inputs to the recorded function; it is
  ;;   incorporated into the key for the next lookup
  ;; (OP ...ARGS) returns an additional input used by the recorded function
  ;;   Note: If OP begins with ":", the remainder of OP is a function name
  ;;   called via memo-call.
  (IO tag op &list args))


(declare (memo-call-v fname args))


(define (memo-playback key)
  (let ((o (dict-get key *memo-db*)))
    (case o
      ((Result v) o)

      ((IO tag op args)
       (define `result (if (filter ":%" op)
                           (memo-call-v (patsubst ":%" "%" op) args)
                           (name-apply op args)))
       (memo-playback [tag result]))

      (else nil))))


(define (memo-record key fname args)
  (let-global ((*memo-key* key))
    (let ((value (name-apply fname args)))
      (set *memo-db* (append {=*memo-key*: (Result value)} *memo-db*))
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
                (expect prior-entry (IO prior-tag fname args))
                prior-tag)
              (begin
                (define `entry {=*memo-key*: (IO *memo-tag* fname args)})
                (set *memo-db* (append entry *memo-db*))
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


(define (memo-call-v fname args)
  (define `key (concat fname " " args))
  (memo-log-call
   fname args
   (let ((o (memo-playback key))
         (key key)
         (args args))
     (case o
       ((Result v) v)
       (else (memo-record key fname args))))))


;; Call a function *or* return cached results.
;;
(define (memo-call fname ...args)
  &public
  (memo-call-v fname args))


;; Return DB in a form suitable for saving directly to a file.
;; [Ends in a newline, and newlines separate entries.]
(define (memo-get-cache)
  (concat (subst "\n" "!n" " " "\n" *memo-db*)
          " "
          {tag: *memo-tag*}
          "\n"))

(define (memo-set-cache data)
  (set *memo-db* (strip-vec (subst "\n" " " "!n" "\n" data)))
  (set *memo-tag* (dict-get "tag" *memo-db* 0))
  (set *memo-db* (filter-out {tag: *memo-tag*} *memo-db*)))


;; File IO

(define (hex-split str)
  (subst "0" "0 " "1" "1 " "2" "2 " "3" "3 " "4" "4 " "5" "5 "
         "6" "6 " "7" "7 " "8" "8 " "9" "9 " "a" "a " "b" "b "
         "c" "c " "d" "d " "e" "e " "f" "f " str))

;; Get hashes of file names matching GLOB-PATTERN.
;; Return { FILENAME: HASH, ... } dictionary.
;;
(define (hash-file filename)
  (define `cmd
    (concat "shasum " (quote-sh-arg filename) " 2>/dev/null"))
  (define `(initial n str)
    (subst " " "" (wordlist 1 n (hex-split str))))
  (initial 12 (word 1 (shell cmd))))


(define (memo-read-file filename)
  (memo-io (global-name hash-file) filename)
  (read-file filename))


(define (memo-write-file filename data)
  (let ((result (write-file filename data)))
    (memo-io (global-name hash-file) filename)
    result))
