;; # io: File I/O and Shell Interaction

(require "core.scm")


(declare SCAM_DEBUG &native)


;; Perform a shell command CMD, logging results if `S` appears in SCAM_DEBUG.
;;
(define (ioshell cmd)
  &public
  (if (filter "S" SCAM_DEBUG)
      (print "shell: " cmd))
  (shell cmd))


;; Quote argument ARG for POSIX shells.
;;
(define (quote-sh-arg arg)
  &public
  (concat "'" (subst "'" "'\\''" arg) "'"))


;; Quote FILENAME for POSIX shells and ensure it does not begin with '-'.
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
          "s/!/!1/g;s/ /!0/g;s/\t/!+/g;s/\x0d/!r/g;s/^$/!./'"))


;; Execute command CMD, returning data written to `stdout`.
;;
;; Unlike `shell`, which trims trailing newlines and then converts newlines
;; to spaces, `shell!` preserves newline and space characters.  It does
;; guarantee complete fidelity, however: NUL characters will not be
;; preserved, and the last line of output will be terminated with a newline
;; (whether it was present or not in the command output).
;;
(define (shell! cmd)
  &public
  (define `raw-lines (ioshell (concat "( " cmd " ) | " (wrap-filter))))
  (concat-vec (subst "!r" "\x0d" (addsuffix "\n" raw-lines))))


;; Construct a command line that will echo STR.
;;
(define (echo-command str)
  &public
  (concat "printf '%b' " (quote-sh-arg (subst "\\" "\\\\" "\n" "\\n" str))))


;; Write DATA to a file descriptor FD, 0 through 8.
;;
;; Result is `nil` on success; non-nil if the file descriptor is bad.
;;
(define (write fd data)
  &public
  ;; Writing to 1 or 2 (stdout, stderr) is tricky.  First, `shell` captures
  ;; the output of the command it executes, making our program's stdout
  ;; unavailable.  To get around this, we redirect 9 to 1 in the SCAM
  ;; program prologue when running make, so fd 9 is available as the real
  ;; stdout.  Second, we want to capture error messages from the executed
  ;; command, so we redirect 2.
  (rest
   (ioshell (concat (echo-command data)
                     (if (filter 2 fd)
                         ;; use 9 to save the "real" stderr
                         " 9>&2 2>&1 >&9"
                         ;; use 9 for the "real" stdout
                         (concat " 2>&1 >&" (patsubst 1 9 fd)))))))


;; Format text and write to a file.  See `vsprintf` for handling of FORMAT
;; and VALUES.  Unlike `printf`, no trailing newline is appended.
;;
(define (fprintf fd format ...values)
  &public
  (write fd (vsprintf format values)))


;; Read one line from `stdin`.
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
;; only on success, so that if the operation is interrupted (e.g. the SCAM
;; process is killed) then FILENAME will not be left with partial data.
;;
;; On success, nil is returned.  Otherwise, an error description is returned.
;;
(define (write-file filename data)
  &public
  (define `file-arg (quote-sh-file filename))
  (define `temp-arg (quote-sh-file (concat filename "_[tmp]")))

  (or
   ;; ensure filename is not a directory and create empty tmpfile
   (rest (ioshell (concat "rm -f " file-arg " 2>&1 && 2>&1 > " temp-arg)))

   ;; initial write succeeded
   (begin
     (for line (concat-groups (subst "\n" "\n " [data]) 50)
          (ioshell (concat (echo-command line) " >> " temp-arg)))
     (rest (ioshell (concat "mv " temp-arg " " file-arg " 2>&1"
                             " || rm " temp-arg " 2>&1"))))))


;; Modify file mode.  Return nil on success, description on error.
;; MODE is as defined by the `chmod` command.
;;
(define (chmod-file filename mode)
  &public
  (ioshell (concat "chmod " (quote-sh-arg mode)
                   " " (quote-sh-file filename) " 2>&1")))


;; Read contents of file FILENAME and return a vector of lines.  The number
;; of elements in the resulting vector is one more than the number of
;; newlines in the file.
;;
;; Return `nil` if the file is not readable.
;;
(define (read-lines filename ?start ?end)
  &public
  (subst "!r" "\x0d"
         (ioshell (concat "(( cat " (quote-sh-file filename) " && echo )"
                          " | " (wrap-filter start end) " ) 2>/dev/null"))))


;; Read the contents of file FILENAME and return it as a string.
;;
(define (read-file filename)
  &public
  (if filename
      (concat-vec (read-lines filename) "\n")
      (print "error: read-file: nil filename")))


;; Copy file SRC to DST.  Return nil on success, description on error.
;;
(define (cp-file src dst ?make-dst-dir)
  &public
  (ioshell (concat
            (if make-dst-dir
                (concat "mkdir -p " (quote-sh-file (dir dst)) " 2>&1 && "))
            "cp " (quote-sh-file src) " " (quote-sh-file dst) " 2>&1")))


;; Return FILENAME if file FILENAME exists.  The `wildcard` built-in
;; function is a faster alternative, but it caches results and will not
;; reflect files created/deleted when the program is running.
;;
(define (file-exists? filename)
  &public
  (if (ioshell (concat "[[ -f " (quote-sh-file filename) " ]] && echo t"))
      filename))


;; Create directory DIR and parent directories, if necessary.
;;
(define (mkdir-p dir)
  &public
  (ioshell (concat "mkdir -p " (quote-sh-file dir) " 2>&1")))


(define *hash-cmd*
  nil)

(define (hash-cmd)
  (or *hash-cmd*
      (begin
        (define `cmd
          ;; GNU which is noisy => errors to /dev/null
          (or (notdir (word 1 (shell "which md5 sha1sum shasum 2>/dev/null")))
              (error "no md5, shasum, or sha1sum in path")))
        (set *hash-cmd* (subst "md5" "md5 -r" cmd))
        *hash-cmd*)))


;; Hash multiple files, returning a dictionary mapping file names to hash
;; values.  Hash values are 16 bytes long.  The selection of which hash
;; algorithm to use depends on what is available in PATH; it is guaranteed
;; to remain the same for the duration of the program's execution.
;;
(define (hash-files filenames)
  &public
  (define `quoted-names
    (concat-for f filenames " "
                (quote-sh-file f)))

  ;; Limit the first word on each line (the hash) to 16 bytes
  (define `cmd
    (concat (hash-cmd) " " quoted-names " 2>/dev/null"
            " | sed 's/\\(^................\\)[^ ]*/\\1/;s/!/!1/g;s/ /!0/g;s/\t/!+/g'"))

  ;; Output is one line per file containing HASH and FILENAME seperated
  ;; by one space (md5 -r) or two spaces (all others).
  (define `extra (if (filter "s%" (hash-cmd)) "!0"))

  (foreach dline (ioshell cmd)
           (foreach hash (word 1 (subst "!0" " " dline))
                    (define `dfile
                      (patsubst (concat hash "!0" extra "%") "%" dline))
                    {(promote dfile): hash})))


;; Return the hash of one file (see `hash-files`).
;;
(define (hash-file filename)
  &public
  (dict-value (hash-files [filename])))


;; Execute shell command CMD, has what it writes to `stdout`, and return the
;; hash.
;;
(define (hash-output cmd)
  &public
  (define `hashpipe
    (if (filter "md5" (basename (hash-cmd)))
        "md5 -q"
        (concat (hash-cmd) " -")))
  (ioshell (concat "( " cmd " ) | " hashpipe
                   " | sed 's/\\(^................\\).*/\\1/'")))


;; Write DATA to a file whose name is a hash of DATA, in directory DIR-NAME.
;; Return the path to the new file.
;;
(define (save-blob dir-name data)
  &public
  (define `templ (quote-sh-file (concat dir-name "objtmp.XXXXXXXX")))
  (define `cmd
    (concat "( t=$(mktemp " templ ") && "
            (echo-command data) " > \"$t\" && "
            "h=$(" (hash-cmd) " \"$t\") && "
            "n=\"${h:0:16}\" && "
            "mv -f \"$t\" " (quote-sh-file dir-name) "\"$n\" && "
            "echo \"$n\""
            ") 2>/dev/null"))
  (addprefix dir-name (ioshell cmd)))


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
  (define `o (subst " " "/" (strip (clean-path-x elems))))

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
(define (escape-path path)
  &public
  ;; Encoding summarized:
  ;;     +   ! # \ $ : ; = % ~ * ? |  "\t"  "\n"  ".."  "/"
  ;;     2 0 1 H B D C S E P T A Q V   -     _     .     /
  (define `a
    (subst "+" "+2" " " "+0" "!" "+1" "#" "+H" "\\" "+B" "$" "+D"
           ":" "+C" ";" "+S" "=" "+E" "%" "+P" "~" "+T" "*" "+A"
           "?" "+Q" "|" "+V" "\t" "+-" "\n" "+_" ".." "+."
           path))
  (patsubst "/%" "+/%" a))


;; Undo `escape-path`.
;;
(define (unescape-path loc)
  &public
  (subst "+/" "/" "+." ".." "+_" "\n" "+-" "\t" "+V" "|" "+Q" "?"
         "+A" "*" "+T" "~" "+P" "%" "+E" "=" "+S" ";" "+C" ":"
         "+D" "$" "+B" "\\" "+H" "#" "+1" "!" "+0" " " "+2" "+" loc))
