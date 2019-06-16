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
  (.. "'" (subst "'" "'\\''" arg) "'"))


;; Quote FILENAME for POSIX shells and ensure it does not begin with '-'.
;;
(define (quote-sh-file filename)
  &public
  (quote-sh-arg (.. (if (filter "-%" [filename]) "./") filename)))


;; Format a string, similarly to vsprintf, but with the following format
;; sequences supported:
;;    `%s` : the argument is output literally
;;    `%A` : the argument is quoted for a POSIX shell
;;    `%V` : the argument is treated as a vector of strings, each to be
;;           quoted as an argument to a POSIX shell
;;    `%F` : the argument is quoted for a POSIX shell using `quote-sh-file`.
;;
(define (io-vsprintf fmt args)
  (define `(shell-fmt code v)
    (cond ((filter "A" code) (quote-sh-arg v))
          ((filter "F" code) (quote-sh-file v))
          ((filter "V" code) (subst " " [" "]
                                    (foreach f (promote v)
                                             (quote-sh-arg f))))
          (else v)))

  (vsprintfx fmt args "s A F V" shell-fmt))


;; [See `io-vsprintf`.]
;;
(define (io-sprintf fmt ...args)
  &public
  (io-vsprintf fmt args))


;; Format a command using `io-vsprintf` and execute it using `shell`.
;;
(define (shellf cmd-fmt ...args)
  &public
  (ioshell (io-vsprintf cmd-fmt args)))


(define `sed-esc-chars
  "s/!/!1/g;s/ /!0/g;s/\t/!+/g;s/\x0d/!r/g")


;; Return a vector of lines output by CMD, optionally starting/stopping at
;; START/END.
;;
(define (shell-vwrap cmd-fmt args ?start ?end)
  (define `shell-cmd
    (.. "( " (io-vsprintf cmd-fmt args) " ) | sed -e '"
        (if start
            (.. start "," end "!d;"))
        sed-esc-chars ";s/^$/!./'"))
  (subst "!r" "\x0d" (ioshell shell-cmd)))


;; Execute command, returning data written to `stdout` as a vector of
;; lines, split at "\n" characters.  To obtain the original output as one
;; string, do the following:
;;
;;     (concat-vec RESULT "\n")
;;
;; CMD-FMT = format string as per `io-vsprintf`
;; ARGS = arguments references by CMD-FMT
;;
;; Note: Zero bytes in the output may result in truncated lines.
;;
(define (shell-lines cmd-fmt ...args)
  &public
  (shell-vwrap (.. cmd-fmt " ; echo ") args))


;; Execute command, capturing STDERR and STDOUT, return exit code
;;
;; CMD-FMT = format string as per `io-vsprintf`
;; ARGS = arguments references by CMD-FMT
;;
;; Result = [CODE LINES...]
;;
(define (shell-ok cmd-fmt ...args)
  (let ((o (shell-vwrap (.. cmd-fmt " 2>&1 ; echo $?") args)))
    (._. (lastword o) (butlast o))))


;; Execute a command, providing STDIN as input, capturing `stdout` and `stderr`.
;; Return the exit status and output.  The output is returned unmolested,
;; except that NUL bytes may result in truncated lines.
;;
;; STDIN = bytes to provide as input to the command.  If nil, /dev/null is
;;    supplied.  The size of STDIN may be limited by the maximum command size.
;; FMT ...ARGS = arguments passed to `io-vsprintf` to construct the command.
;;
;; Result = [STATUS STDOUT STDERR]
;;
(define (pipe stdin fmt ...args)
  &public
  (define `cmd
    (io-vsprintf fmt args))

  (define `(quote-printf-arg str)
    (quote-sh-arg (subst "\\" "\\\\" "\n" "\\n" str)))

  (define `(label n)
    (.. "| sed 's/^/" n "/;" sed-esc-chars "'"))

  ;; Label each line with "1" or "2"
  (define `label-cmd
    (.. (if stdin
            (.. "printf '%b' " (quote-printf-arg stdin) " | ")
            "cat /dev/null | ")
        "( ( ( " cmd " ; echo 1$? >&3 ; echo ; echo >&2 ) " (label 2) " ) "
        "3>&2 2>&1 1>&3 " (label 3) " ) 2>&1"))

  (let ((lines (subst "!r" "\x0d" (ioshell label-cmd))))
    (foreach fd [1 2 3]
             (or (subst " " "\n" (filtersub (.. fd "%") "%" lines))
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
(define (echo-small bytes suffix-fmt suffix-arg is-append)
  (ioshell
   (.. "printf '%b' '" (concat-vec bytes) "' "
       (io-vsprintf (if is-append
                        (patsubst ">%F" ">>%F" suffix-fmt)
                        suffix-fmt)
                    [suffix-arg]))))


(declare (echo-bytes bytes suffix-fmt suffix-arg is-append))


(define (echo-split b-first b-rest suffix-fmt suffix-arg is-append)
  ;; Do not split in the middle of an escape sequence: "\\" or "'\''"
  (if (filter "' \\" (lastword (subst "\\ \\" nil "' \\ ' '" nil b-first)))
      ;; get one more character
      (echo-split (.. b-first " " (word 1 b-rest))
                  (rest b-rest)
                  suffix-fmt suffix-arg is-append)
      (or (echo-small b-first suffix-fmt suffix-arg is-append)
          (echo-bytes b-rest suffix-fmt suffix-arg 1))))


;; BYTES = vector of single-byte strings (gotten from `get-echo-bytes`)
;; SUFFIX-FMT = format string to construct a suffix to be appended to each
;;    command (for the purpose of redirection).  Within this format string,
;;    when multiple commands are issued, ">%F" will be replaced with ">>%F"
;;    in all but the first command.
;; SUFFIX-ARG = argument passed to `io-sprintf` with SUFFIX-FMT
;;
(define (echo-bytes bytes suffix-fmt suffix-arg ?is-append)
  (if (word MAX-ARG-REST bytes)
      (echo-split (wordlist 1 MAX-ARG-1 bytes)
                  (nth-rest MAX-ARG-REST bytes)
                  suffix-fmt suffix-arg is-append)
      (echo-small bytes suffix-fmt suffix-arg is-append)))


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
        (.. "2>&1 >&" (patsubst 1 9 fd))))

  (echo-bytes (get-echo-bytes data) redirs nil))


;; Write DATA to file FILENAME.
;;
;; On success, return nil.  On failure, return an error description.
;;
(define (write-file filename data)
  &public
  (echo-bytes (get-echo-bytes data) "2>&1 >%F" filename))


;; Move file FROM to TO.
;;
;; On success, return nil.  On failure, return an error description.
;;
(define (mv-file from to)
  &public
  (shellf "mv -f %F %F 2>&1" from to))


(define (write-file-atomic file-name data)
  &public
  (let ((o (shell-ok "mktemp %F" (.. file-name ".tmp.XXXX"))))
    (define `tmp-name (nth 2 o))
    (if (filter-out 0 (word 1 o))
        (nth 2 o)
        (or (write-file tmp-name data)
            (mv-file tmp-name file-name)))))


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
  (shellf "chmod %s %F 2>&1" mode filename))


;; Read contents of file FILENAME and return a vector of lines.  The number
;; of elements in the resulting vector is one more than the number of
;; newlines in the file.
;;
;; Return `nil` if the file is not readable.
;;
(define (read-lines filename ?start ?end)
  &public
  (shell-vwrap "cat %F 2>/dev/null && echo" [filename] start end))


;; Read the contents of file FILENAME and return it as a string.
;;
(define (read-file filename)
  &public
  (if filename
      (concat-vec (read-lines filename) "\n")
      (print "error: read-file: nil filename")))


;; Create directory DIR and parent directories, if necessary.
;;
(define (mkdir-p dir)
  &public
  (shellf "mkdir -p %F 2>&1" dir))


;; Copy file SRC to DST.  Return nil on success, description on error.
;;
(define (cp-file src dst ?make-dst-dir)
  &public
  (shellf "%s cp %F %F 2>&1"
          (if make-dst-dir
              (io-sprintf "mkdir -p %F 2>&1 &&" (dir dst)))
          src dst))


;; Copy file SRC to DST.  Return nil on success, description on error.
;;
;; Operation is atomic on POSIX file systems -- that is, if DST is opened
;; and read by another process, it will either see the previous contents of
;; DST or an entire copy of SRC (never a partial copy).  A temporary file
;; (in the same directory as DST, and with a name based on DST) will be used
;; for this purpose.
;;
(define (cp-file-atomic src dst ?make-dst-dir)
  &public
  (shellf
   "(%s a=$(mktemp %F) && (cp %F \"$a\" && mv -f \"$a\" %F) || rm \"$a\") 2>&1"
   (if make-dst-dir
       (io-sprintf "mkdir -p %F &&" (dir dst)))
   (.. dst ".tmp.XXXX")
   src
   dst))


;; Return FILENAME if file FILENAME exists.  The `wildcard` built-in
;; function is a faster alternative, but it caches results and will not
;; reflect files created/deleted when the program is running.
;;
(define (file-exists? filename)
  &public
  (if (shellf "[[ -f %F ]] && echo t" filename)
      filename))


(define *hash-cmd*
  nil)

(define (get-hash-cmd)
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
  ;; Limit the first word on each line (the hash) to 16 bytes
  (define `hash-out
    (shellf (.. "%s -- %V 2>/dev/null | "
                "sed 's/\\(^................\\)[^ ]*/\\1/;"
                "s/!/!1/g;s/ /!0/g;s/\t/!+/g'")
            (get-hash-cmd) filenames))

  ;; Output is one line per file containing HASH and FILENAME seperated
  ;; by one space (md5 -r) or two spaces (all others).
  (foreach
      delim (if (filter "s%" (get-hash-cmd))
                "!0!0"
                "!0")
      (foreach
          dline hash-out
          (foreach
              hash (word 1 (subst "!0" " " dline))
              (define `dfile
                (patsubst (.. hash delim "%") "%" dline))
              {(promote dfile): hash}))))


;; Return the hash of one file (see `hash-files`).
;;
(define (hash-file filename)
  &public
  (dict-value (hash-files [filename])))


;; Execute shell command, hash what it writes to `stdout`, and return the
;; hash.
;;
;; CMD-FMT = format string as per `io-vsprintf`
;; ARGS = arguments references by CMD-FMT
;;
(define (hash-output cmd-fmt ...args)
  &public
  (define `hashpipe
    (if (filter "md5" (basename (get-hash-cmd)))
        "md5 -q"
        (.. (get-hash-cmd) " -")))
  (ioshell (.. "( " (io-vsprintf cmd-fmt args) " ) | " hashpipe
               " | sed 's/\\(^................\\).*/\\1/'")))


;; Write DATA to a temporary file, and then rename it to the hash of DATA.
;; Return the hash, or NIL on failure.
;;
(define (write-blob file data)
  (define `hash
    (if (echo-bytes (get-echo-bytes data) "2>&1 >%F" file)
        nil
        (shellf (.. "( o=%F && h=$(%s \"$o\") && "
                    "mv -f \"$o\" %A\"${h:0:16}\" && "
                    "echo \"${h:0:16}\" ) 2>/dev/null")
                file (get-hash-cmd) (dir file))))

  (addprefix (dir file) hash))


;; Write DATA to a file whose name is a hash of DATA, in directory DIR-NAME.
;; Return the path to the new file.
;;
(define (save-blob dir-name data)
  &public
  (write-blob (shellf "mktemp %F" (.. dir-name "blob.XXXX"))
              data))


;; clean-path-x: Helper for clean-path
;; PATH is a word list of path elements remaining to be visited
;; STK is a word list of non-".." path elements
;;
(define (clean-path-x path ?stk)
  ;; next path element
  (define `e (word 1 path))
  ;; emit ".." if path element is ".." and stack is empty
  (define `emit (filter ".." (.. e (word 1 stk))))
  ;; remove last path element when E is "..", otherwise append E
  (define `next-stk
    (subst "/" " " (filter-out "%/.." (.. stk "/" e))))

  (if path
      (.. emit " " (clean-path-x (rest path) next-stk))
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

  (promote (patsubst "/./" "/" (.. prefix (or o ".") suffix))))


;; Combine a directory name and a path (relative or absolute).
;;
(define (resolve-path dir path)
  &public
  (clean-path (if (filter "/%" path)
                  path
                  (.. dir "/" path))))


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
    (or (value "SCAM_TMP") ".scam/"))

  (if tmpl
      (let ((o (pipe nil "mktemp -d %F" (.. tmp tmpl))))
        (or (first (word 2 (subst "\n" " " o)))
            (error (.. "get-tmp-dir failed: " (nth 2 o)))))
      tmp))
