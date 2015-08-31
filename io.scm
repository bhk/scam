;;--------------------------------------------------------------
;; io : I/O and shell interaction
;;--------------------------------------------------------------

(require "core")
(declare SCAM_DEBUG &global)


(define (xshell str)
  (if (filter "S" SCAM_DEBUG)
      (print "shell: " str))
  (shell str))


;; quote argument for POSIX shells
;;
(define (quote-sh-arg arg)
  (concat "'" (subst "'" "'\''" arg) "'"))


;; Construct a command line to echo `str`
;;
(define (echo-command str)
  (concat "printf '%b' " (quote-sh-arg (subst "\\" "\\\\" "\n" "\\n" str))))


;; Write to console without a trailing newline.  We redirect to stderr
;; because `shell` captures stdout, and writing to stdin fails on Cygwin.
;;
(define (printn ...)
  (xshell (concat (echo-command (concat-vec *args*)) " >&2")))


;; Execute command, returning data written to stdout.  (Unlike `shell`,
;; which trims trailing newlins and then converts newlines to spaces.)
;;
(define (shell! cmd)
   (subst " " "" "!n" "\n" "!0" " " "!1" "!"
          (xshell (concat cmd " | sed -e 's/!/!1/g;s/ /!0/g;s/$/!n/g'"))))


;; Read one line from stdin.
;;
;; Note: On MacOS, input lines longer than 1023 characters will cause bad
;; things to happen.
;;
(define (getline prompt)
  (if prompt
      (printn prompt))
  (shell! "head -1"))


(define (write-file filename data)
  (if filename
      (xshell (concat (echo-command data) " > " filename))
      (print "error: write-file: nil filename")))


(define (read-file fname)
  (if fname
      (shell! (concat "cat < " (quote-sh-arg fname)))
      (print "error: read-file: nil filename")))


(define (read-lines fname start end)
  (define `command
    (concat "sed -E '"
            (if start
                (concat start "," end "!d;"))
            "s/!/!1/g;s/\t/!+/g;s/ /!0/g;s/$/!n/g' "
            fname))
  (if fname
      (subst " !n" " !." "!n" "" (xshell command))
      (print "error: read-lines: nil filename")))


(define (file-exists? fname)
  ;; `wildcard` is cached by Make and will not reflect files created/deleted
  ;; when the program is running.
  (if
   (shell (concat "ls " (quote-sh-arg fname) " 2> /dev/null"))
   1))
