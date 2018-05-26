;;--------------------------------------------------------------
;; io : File I/O and shell interaction
;;--------------------------------------------------------------

(require "core")
(declare SCAM_DEBUG &global)


(define (logshell cmd)
  (if (filter "S" SCAM_DEBUG)
      (print "shell: " cmd))
  (shell cmd))


;; Quote argument ARG for POSIX shells.
(define (quote-sh-arg arg)
  &public
  (concat "'" (subst "'" "'\\''" arg) "'"))


;; A sed command that converts text to a vector of lines as encoded by SCAM.
;;
(define `(wrap-filter ?start ?end)
  (concat "sed -e '"
          (if start
              (concat start "," end "!d;"))
          "s/!/!1/g;s/ /!0/g;s/\t/!+/g;s/^$/!./'"))


;; Convert a sequence of lines to text.
;;
(define (unwrap-text o)
  (if o
      (concat (concat-vec o "\n") "\n")))


;; Execute command CMD, returning data written to stdout.
;;
;; Unlike `shell`, which trims trailing newlines and then converts newlines
;; to spaces, `shell!` preserves newline and space characters, but does not
;; guarantee complete fidelity: NUL characters will not be preserved, and
;; the last line of output will be terminated with a newline (whether it was
;; present or not in the command out).
;;
(define (shell! cmd)
  &public
  (unwrap-text (logshell (concat "( " cmd " ) | " (wrap-filter)))))


;; Construct a command line to echo STR.
(define (echo-command str)
  &public
  (concat "printf '%b' " (quote-sh-arg (subst "\\" "\\\\" "\n" "\\n" str))))


;; Write STRINGS to the console without a trailing newline.  This function
;; redirects its output to stderr because `shell` captures stdout, and
;; because writing to stdin fails on Cygwin.
;;
(define (printn ...strings)
  &public
  (logshell (concat (echo-command (concat-vec strings)) " >&2")))


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
;; On success, "OK" is returned.
;;
(define (write-file filename data)
  &public
  (define `prefile (quote-sh-arg (concat filename ".pre")))

  (and filename
       (not (logshell (concat "2>&1 >" prefile)))
       ;; initial write succeeded
       (begin
         (for line (subst "\n" "\n " [data])
              (logshell (concat (echo-command line) " >> " prefile)))
         (logshell (concat "mv " prefile " " (quote-sh-arg filename)))
         "OK")))


;; Read contents of file FILENAME and return a vector of lines.
;;
(define (read-lines filename ?start ?end)
  &public
  (if filename
      (logshell (concat (wrap-filter start end) " "
                        (quote-sh-arg filename) " 2>/dev/null"))
      (print "error: read-lines: nil filename")))


;; Read the contents of file FILENAME and return it as a string.
;;
(define (read-file filename)
  &public
  (if filename
      (unwrap-text (read-lines filename))
      (print "error: read-file: nil filename")))


;; Return FILENAME if file FILENAME exists.  The `wildcard` built-in
;; function is a faster alternative, but it caches results and will not
;; reflect files created/deleted when the program is running.
;;
(define (file-exists? filename)
  &public
  (if (logshell (concat "[[ -f " (quote-sh-arg filename) " ]] && echo t"))
      filename))


(define (mkdir-p dir)
  &public
  (logshell (concat "mkdir -p " (quote-sh-arg dir))))


;; clean-path-x: Helper for clean-path
;; PATH is a word list of path elements remaining to be visited
;; STK is a word list of non-".." path elements
;;
(define (clean-path-x path ?stk)
  ;; next path element
  (define `e (word 1 path))
  ;; emit ".." if path element is ".." and stack is empty
  (define `emit (filter ".." (concat e (word 1 stk))))
  ;; remove last path element when E is "..", otherwise append E
  (define `next-stk
    (subst "/" " " (filter-out "%/.." (concat stk "/" e))))

  (if path
      (concat emit " " (clean-path-x (rest path) next-stk))
      stk))


;; Remove redundant "." and ".." path elements and repeated "/" characters
;; from an absolute or relative path.  PATH may include whitespace.
;;
(define (clean-path path)
  &public
  (define `prefix (if (filter "/%" path) "/"))
  (define `suffix (if (filter "%/" path) "/"))
  (define `elems (filter-out "." (subst "/" " " [path])))
  (define `o (subst " " "/" (strip-vec (clean-path-x elems))))

  (promote (patsubst "/./" "/" (concat prefix (or o ".") suffix))))


;; Combine a directory name and a path (relative or absolute).
;;
(define (resolve-path dir path)
  &public
  (clean-path (if (filter "/%" path)
                  path
                  (concat dir "/" path))))


;; Escape a relative path or absolute path, so that the result is:
;;  - safe as a sub-directory (has no "..", and is not an absolute path)
;;  - a word (contains no spaces)
;;  - directly usable with `filter` (contains no "%")
;;  - safe to include in an "include ..." directive (no globbing chars)
;;  - safe to use in a make rule without escaping
;;  - unique (the encoding can be reversed)
;;
;; Encoding summarized:
;;    +   ! # \ $ : ; = % ~ * ? |  "\t"  "\n"  ".."  "/"
;;    2 0 1 H B D C S E P T A Q V   -     _     .     /
;;
(define (escape-path path)
  &public
  (define `a
    (subst "+" "+2" " " "+0" "!" "+1" "#" "+H" "\\" "+B" "$" "+D"
           ":" "+C" ";" "+S" "=" "+E" "%" "+P" "~" "+T" "*" "+A"
           "?" "+Q" "|" "+V" "\t" "+-" "\n" "+_" "/.." "/+." path))
   (patsubst "/%" "+/%" a))

;; Undo `escape-path`
;;
(define (unescape-path loc)
  &public
  (subst "+/" "/" "/+." "/.." "+_" "\n" "+-" "\t" "+V" "|" "+Q" "?"
         "+A" "*" "+T" "~" "+P" "%" "+E" "=" "+S" ";" "+C" ":"
         "+D" "$" "+B" "\\" "+H" "#" "+1" "!" "+0" " " "+2" "+" loc))
