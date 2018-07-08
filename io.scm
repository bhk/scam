;;--------------------------------------------------------------
;; io : File I/O and shell interaction
;;--------------------------------------------------------------

(require "core.scm")
(declare SCAM_DEBUG &native)


(define (logshell cmd)
  &public
  (if (filter "S" SCAM_DEBUG)
      (print "shell: " cmd))
  (shell cmd))


;; Quote argument ARG for POSIX shells.
;;
(define (quote-sh-arg arg)
  &public
  (concat "'" (subst "'" "'\\''" arg) "'"))


;; Quote FILENAME for POSIX shells and ensure it does not begin with '-'
;;
(define (quote-sh-file filename)
  &public
  (quote-sh-arg (concat (if (filter "-%" [filename]) "./") filename)))


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


;; Write data to a file descriptor.  Since `shell` captures stdout for the
;; command it invokes, we replace 1 with 9, which has been redirected to
;; *actual* stdout (see the prologue in build.scm).
;;
;; We redirect stderr to stdout, so that `shell` will capture error
;; messages. Special care must be taken when fd is 2.
;;
;; Returns `nil` on success; non-nil if the file descriptor is bad.
;;
(define (write fd data)
  &public
  (rest
   (logshell (concat (echo-command data)
                     (if (filter 2 fd)
                         ;; swap 1 and 2
                         " 3>&2 2>&1 1>&3 3>&-"
                         ;; swap 1 and 2
                         (concat " 2>&1 >&" (patsubst 1 9 fd)))))))


;; Format string and write to a file.  Unlike printf: (A) no trailing
;; newline is appended, and (B) a file descriptor is specified.
;;
(define (fprintf fd format ...values)
  &public
  (write fd (vsprintf format values)))


;; Read one line from stdin.
;;
;; Note: On MacOS, input lines longer than 1023 characters will cause bad
;; things to happen.
;;
(define (getline prompt)
  &public
  (if prompt
      (write 1 prompt))
  (shell! "head -1"))


;; Concatenate elements in VEC in groups of SIZE.
;;
(define (concat-groups vec size)
  (let ((group-dots (patsubst "%" "!." (wordlist 1 size vec)))
        (all-dots (patsubst "%" "!." vec))
        (vec vec))
    (define `groups
      (subst group-dots (concat group-dots "!.") all-dots))
    (subst "!. " "" "!." " " (join vec groups))))


;; Write DATA to file FILENAME.
;;
;; In order to handle large values, the data is written line-by-line, using
;; multiple shell invocations, to a temporary file that is moved to FILENAME
;; only on success, so that if the operation is interrupted (e.g. our
;; process is terminated) then FILENAME will not be left with partial data.
;;
;; On success, nil is returned.  Otherwise, an error description is returned.
;;
(define (write-file filename data)
  &public
  (define `file-arg (quote-sh-file filename))
  (define `temp-arg (quote-sh-file (concat filename "_[tmp]")))

  (or
   ;; ensure filename is not a directory and create empty tmpfile
   (rest (logshell (concat "rm -f " file-arg " 2>&1 && 2>&1 > " temp-arg)))

   ;; initial write succeeded
   (begin
     (for line (concat-groups (subst "\n" "\n " [data]) 50)
          (logshell (concat (echo-command line) " >> " temp-arg)))
     (rest (logshell (concat "mv " temp-arg " " file-arg " 2>&1"
                             " || rm " temp-arg " 2>&1"))))))


;; Read contents of file FILENAME and return a vector of lines.
;;
(define (read-lines filename ?start ?end)
  &public
  (if filename
      (logshell (concat (wrap-filter start end) " "
                        (quote-sh-file filename) " 2>/dev/null"))
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
  (if (logshell (concat "[[ -f " (quote-sh-file filename) " ]] && echo t"))
      filename))


(define (mkdir-p dir)
  &public
  (logshell (concat "mkdir -p " (quote-sh-file dir) " 2>&1")))


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
           "?" "+Q" "|" "+V" "\t" "+-" "\n" "+_" ".." "+."
           path))
  (patsubst "/%" "+/%" a))

;; Undo `escape-path`
;;
(define (unescape-path loc)
  &public
  (subst "+/" "/" "+." ".." "+_" "\n" "+-" "\t" "+V" "|" "+Q" "?"
         "+A" "*" "+T" "~" "+P" "%" "+E" "=" "+S" ";" "+C" ":"
         "+D" "$" "+B" "\\" "+H" "#" "+1" "!" "+0" " " "+2" "+" loc))
