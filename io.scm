;;--------------------------------------------------------------
;; io : I/O and shell interaction
;;--------------------------------------------------------------

(require "core")
(declare SCAM_DEBUG)


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
  (concat-vec
   (subst "!n" "\n"
          (xshell (concat cmd " | sed -e 's/!/!1/g;s/ /!0/g;s/$/!n/g'")))))


;; Read one line from stdin.
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
