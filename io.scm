;;--------------------------------------------------------------
;; io : I/O and shell interaction
;;--------------------------------------------------------------

(require "core")
(declare SCAM_DEBUG &global)


(define `(logshell cmd)
  (if (filter "S" SCAM_DEBUG)
      (print "shell: " cmd))
  (shell cmd))


(define (shellc ...all)
  (logshell (concat-vec all)))


;; quote argument for POSIX shells
;;
(define (quote-sh-arg arg)
  &public
  (concat "'" (subst "'" "'\''" arg) "'"))


;; Construct a command line to echo `str`
;;
(define (echo-command str)
  &public
  (concat "printf '%b' " (quote-sh-arg (subst "\\" "\\\\" "\n" "\\n" str))))


;; Write to console without a trailing newline.  We redirect to stderr
;; because `shell` captures stdout, and writing to stdin fails on Cygwin.
;;
(define (printn ...strings)
  &public
  (shellc (echo-command (concat-vec strings)) " >&2"))


;; Execute command, returning data written to stdout.  (Unlike `shell`,
;; which trims trailing newlines and then converts newlines to spaces.)
;;
(define (shell! cmd)
  &public
   (subst " " "" "!n" "\n" "!0" " " "!1" "!"
          (logshell (concat cmd " | sed -e 's/!/!1/g;s/ /!0/g;s/$/!n/g'"))))


;; Read one line from stdin.
;;
;; Note: On MacOS, input lines longer than 1023 characters will cause bad
;; things to happen.
;;
(define (getline prompt)
  &public
  (if prompt
      (printn prompt))
  (shell! "head -1"))


;; write-file creates a temporary file and, on success, copies it to the
;; destination.  If interrupted in the middle of processing, the temporary
;; file might be partially constructed.
;;
(define (write-file filename data)
  &public
  (define `prefile (concat filename ".pre"))

  (if filename
      ;; write line-by-line to avoid command line length limit
      (if (not (shellc (echo-command "") " > " prefile " || echo ERROR"))
          ;; initial write succeeded
          (begin
            (for line (subst " " "\n " (split "\n" data))
                 (if line
                     (shellc (echo-command line) " >> " prefile)))
            (shellc "mv " prefile " " filename)))
      (print "error: write-file: nil filename")))


(define (read-file fname)
  &public
  (if fname
      (shell! (concat "cat < " (quote-sh-arg fname)))
      (print "error: read-file: nil filename")))


;; Read lines from a file into a vector.
;;
(define (read-lines fname ?start ?end)
  &public
  (define `command
    (concat "sed -E '"
            (if start
                (concat start "," end "!d;"))
            "s/!/!1/g;s/\t/!+/g;s/ /!0/g;s/$/!n/g' "
            fname))
  (if fname
      (subst " !n" " !." "!n" "" (logshell command))
      (print "error: read-lines: nil filename")))


(define (file-exists? fname)
  &public
  ;; `wildcard` is cached by Make and will not reflect files created/deleted
  ;; when the program is running.
  (if
   (shellc "ls " (quote-sh-arg fname) " 2> /dev/null")
   1))
