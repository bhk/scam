;; # io: File I/O and Shell Interaction

(require "core.scm")
(require "string.scm")


(declare SCAM_DEBUG &native)


;; Invoke `(shell CMD)`, logging results if `S` appears in SCAM_DEBUG.
;;
(define (ioshell cmd)
  (if (filter "S" SCAM_DEBUG)
      (printf "shell: %q" cmd))
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


(define `sed-esc-chars
  "s/!/!1/g;s/ /!0/g;s/\t/!+/g;s/\x0d/!r/g")


;; Return a vector of lines output by CMD, optionally starting/stopping at
;; START/END.
;;
(define (shell-wrap cmd ?start ?end)
  (define `shell-cmd
    (concat "( " cmd " ) | sed -e '"
            (if start
                (concat start "," end "!d;"))
            sed-esc-chars ";s/^$/!./'"))
  (subst "!r" "\x0d" (ioshell shell-cmd)))


;; Execute command CMD, returning data written to `stdout` as a vector of
;; lines, split at "\n" characters.  To obtain the original output as one
;; string, do the following:
;;
;;     (concat-vec RESULT "\n")
;;
;; Note: Zero bytes in the output may result in truncated lines.
;;
(define (shell-lines cmd)
  &public
  (shell-wrap (concat cmd " ; echo ")))


;; Execute CMD, providing STDIN as input, capturing `stdout` and `stderr`.
;; Return the exit status and output.  The output is returned unmolested,
;; except that zero bytes may result in truncated lines.
;;
;; Result = [STATUS STDOUT STDERR]
;;
(define (pipe cmd ?stdin)
  &public
  (define `(quote-printf-arg str)
    (quote-sh-arg (subst "\\" "\\\\" "\n" "\\n" str)))

  (define `(label n)
    (concat "| sed 's/^/" n "/;" sed-esc-chars "'"))

  ;; Label each line with "1" or "2"
  (define `label-cmd
    (concat (if stdin
                (concat "printf '%b' " (quote-printf-arg stdin) " | ")
                "cat /dev/null | ")
            "( ( ( " cmd " ; echo 1$? >&3 ; echo ; echo >&2 ) " (label 2) " ) "
            "3>&2 2>&1 1>&3 " (label 3) " ) 2>&1"))

  (let ((lines (subst "!r" "\x0d" (ioshell label-cmd))))
    (foreach fd [1 2 3]
             (or (subst " " "\n" (filtersub (concat fd "%") "%" lines))
                 "!."))))


;; Some (all?) Linuxes limit command line length to 2^18
;;
(define MAX-ARG-1     100000)
(define MAX-ARG-REST  100001)   ;; MAX-ARG-1 + 1


;; Construct a BYTES argument for `echo-bytes` that outputs STR.
;;
(define `(get-echo-bytes str)
  (subst "\\" "\\ \\"    ;; encode for printf
         "\n" "\\ n"     ;; encode for printf
         "'" "' \\ ' '"  ;; encode for shell
         (string-to-bytes str)))


;; Output BYTES to file FILE-NAME.
;;
;; Return `nil` on success; error message otherwise.
;;
(define (echo-small bytes suffix file is-append)
  (ioshell
   (concat "printf '%b' '" (concat-vec bytes) "' "
           (subst "{>}" (if is-append ">>" ">") suffix)
           file)))


(declare (echo-bytes bytes suffix file is-append))


(define (echo-split b-first b-rest suffix file is-append)
  ;; "'" should only appear within "' \\ ' '"
  (if (filter "' \\" (lastword (subst "\\ \\" nil "' \\ ' '" nil b-first)))
      ;; get one more character
      (echo-split (concat b-first " " (word 1 b-rest))
                  (rest b-rest)
                  suffix file is-append)
      (or (echo-small b-first suffix file is-append)
          (echo-bytes b-rest suffix file 1))))


;; BYTES = vector of single-byte strings (gotten from `get-echo-bytes`)
;; SUFFIX = string appended to echo command, in which "{>}" will be replaced
;;      with either ">" or ">>".
;; FILE = string to be appended to SUFFIX.
;;
(define (echo-bytes bytes suffix file ?is-append)
  (if (word MAX-ARG-REST bytes)
      (echo-split (wordlist 1 MAX-ARG-1 bytes)
                  (nth-rest MAX-ARG-REST bytes)
                  suffix file is-append)
      (echo-small bytes suffix file is-append)))


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
  (define `redirs
    (if (filter 2 fd)
        "9>&2 2>&1 >&9"
        (concat "2>&1 >&" (patsubst 1 9 fd))))

  (echo-bytes (get-echo-bytes data) redirs nil))


;; Write DATA to file FILENAME.
;;
;; On success, nil is returned.  Otherwise, an error description is returned.
;;
(define (write-file file-name data)
  &public
  (echo-bytes (get-echo-bytes data) "2>&1 {>} " (quote-sh-file file-name)))


;; Format text and write to a file descriptor, 0 through 8.  See `vsprintf`
;; for handling of FORMAT and VALUES.  Unlike `printf`, no trailing newline
;; is appended.
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
  (concat-vec (shell-lines "head -1") "\n"))


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
  (shell-wrap
   (concat "cat " (quote-sh-file filename) " 2>/dev/null && echo")
   start end))


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
          ;; GNU `which` is noisy => errors to /dev/null
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
            " | sed 's/\\(^................\\)[^ ]*/\\1/;"
            "s/!/!1/g;s/ /!0/g;s/\t/!+/g'"))

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


;; Execute shell command CMD, hash what it writes to `stdout`, and return the
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


(define (mktemp dir-name)
  (ioshell
   (concat "mktemp " (quote-sh-file (concat dir-name "blob.XXXX")))))


(define (write-blob file data)
  (define `dir-arg
    (quote-sh-file (dir file)))

  (define `file-arg
    (quote-sh-file file))

  (define `hash
    (if (echo-bytes (get-echo-bytes data) "2>&1 {>} " file-arg)
        nil
        (ioshell
         (concat "( o=" file-arg
                 " && h=$(" (hash-cmd) " \"$o\")"
                 " && mv -f \"$o\" " dir-arg "\"${h:0:16}\""
                 " && echo \"${h:0:16}\" ) 2>/dev/null"))))

  (addprefix (dir file) hash))


;; Write DATA to a file whose name is a hash of DATA, in directory DIR-NAME.
;; Return the path to the new file.
;;
(define (save-blob dir-name data)
  &public
  (write-blob (mktemp dir-name) data))


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


;; Obtain a directory underneath the build directory, or, if not running
;; in the context of a build (as "-q.scm" files do) then return ".scam/".
;; If TMPL is given, a new directory is created under the build directory
;; and returned, using TMPL as a template for the `mktemp` command.
;;
;; Result ends in "/".
;;
(define (get-tmp-dir ?tmpl)
  &public
  (define `tmp
    (or (value "SCAM_DIR") (value "SCAM_TMP") ".scam/"))

  (if tmpl
      (let ((o (pipe (concat "mktemp -d " (quote-sh-file (concat tmp tmpl))))))
        (or (first (word 2 (subst "\n" " " o)))
            (error (concat "get-tmp-dir failed: " (nth 2 o)))))
      tmp))
