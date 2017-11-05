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


;; Quote argument ARG for POSIX shells.
(define (quote-sh-arg arg)
  &public
  (concat "'" (subst "'" "'\''" arg) "'"))


;; Construct a command line to echo STR.
(define (echo-command str)
  &public
  (concat "printf '%b' " (quote-sh-arg (subst "\\" "\\\\" "\n" "\\n" str))))


;; Write STRINGS to the console without a trailing newline.  This function
;; redirects its output to stderr because `shell` captures stdout, and
;; because writing to stdin fails on Cygwin.
(define (printn ...strings)
  &public
  (shellc (echo-command (concat-vec strings)) " >&2"))


;; Execute command CMD, returning data written to stdout.  (Unlike `shell`,
;; which trims trailing newlines and then converts newlines to spaces.)
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


;; Write DATA to file FILENAME.
;;
;; In order to handle large values, the data is written line-by-line, using
;; multiple shell invocations, to a temporary file that is moved to FILENAME
;; only on success, so that if the operation is interrupted (e.g. our
;; process is terminated) then FILENAME will not be left with partial data.
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


;; Read the contents of file FILENAME and return it as a string.
(define (read-file filename)
  &public
  (if filename
      (shell! (concat "cat < " (quote-sh-arg filename)))
      (print "error: read-file: nil filename")))


;; Read contents of file FILENAME and return a vector of lines.
(define (read-lines filename ?start ?end)
  &public
  (define `command
    (concat "sed -E '"
            (if start
                (concat start "," end "!d;"))
            "s/!/!1/g;s/\t/!+/g;s/ /!0/g;s/$/!n/g' "
            filename))
  (if filename
      (subst " !n" " !." "!n" "" (logshell command))
      (print "error: read-lines: nil filename")))


;; Return 1 if file FILENAME exists.  The `wildcard` built-in function
;; is a faster alternative, but it caches results and will not reflect
;; files created/deleted when the program is running.
(define (file-exists? filename)
  &public
  (if
   (shellc "ls " (quote-sh-arg filename) " 2> /dev/null")
   1))
