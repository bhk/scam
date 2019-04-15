;; # scamdoc: Documentation Generator
;;
;; Scamdoc scans SCAM source files for documentation and generates Markdown
;; documents.
;;
;; Extracted documentation must appear in comment blocks: sequential lines
;; that begin with one or more ";" characters.  Initial ";" characters and
;; one space are removed from each line to yield a text block -- it is
;; assumed that this text is in Markdown format.
;;
;; When the text in a comment block begins with "#" it is treated as a
;; section of documentation for the module itself.
;;
;; Otherwise, if a comment block is immediately followed by a `&public`
;; function or macro definition or declaration (with no intervening
;; non-blank lines) it is treated as the documentation for that symbol.


;; TODO:
;;  - Generate links to functions referenced as text:
;;      Like `sprintf` ...  =>  Like [`sprintf`](#sprintf-???) ...
;;  - Support exported variables, symbol macros, and data types.


(require "core")
(require "string")
(require "io")
(require "peg")
(require "getopts")

;;--------------------------------
;; parsing
;;--------------------------------

;; Include all tokens that may delimit identifiers in addition to the
;; tokens we will be explicitly naming in the parser.
;;
(define lexify
  (gen-lex [" " "\n" ";" "(" ")" "`" "{" "}" "\""]))


(define matchSpace
  (peg-* (peg-or (peg-p [" " "\t" "\n"])
                 (peg-and (peg-p ";") (peg-* (peg-p "%"))))))


(define (tok pat)
  (peg-and (peg-p pat) matchSpace))


(define matchDecl
  (peg-and matchSpace
           (tok "(")
           (peg-or (tok "define")
                   (tok "declare"))
           (peg-? (peg-p "`" nil {is-macro: 1}))
           (tok "(")
           (peg-c "proto" (peg-* (peg-p "%" ")")))
           (tok ")")
           (tok "&public")))


(expect (matchDecl (lexify "(define (foo a b) &public nil)") 1)
        (append 14 {proto: ["foo" " " "a" " " "b"]}))

(expect (matchDecl (lexify "(define `(foo) &public nil)") 1)
        (append 11 {is-macro: 1, proto: ["foo"]}))


(define (dump name value)
  (define `ns
    (string-repeat " " (string-len name)))
  (printf "%s: [ ‹%s› ]" name
          (subst "!0" "·" "\n" "␤" " " (concat "›\n" ns "    ‹") value)))


;; - Combine consecutive comment lines into a single word.
;; - Remove all other comments.
;; - Group lines that begin with space with preceding non-comment lines.
;;
(define (pre-process text)
  (define `a (subst "\n" "\n " [text]))

  ;; Distinguish line endings, remove comments in non-comment lines, convert
  ;; spaces to "!s" in non-comment lines.
  (define `b
    (foreach line a
             (if (filter ";%" line)
                 (concat line "!.")
                 (word 1 (subst ";" " " line)))))      ;;(subst "!0" "!s" ...)

  ;; Combine comment lines
  (define `c (subst "\n!. ;" "\n;" "!." nil b))

  ;; Remove blank lines
  (define `d (filter-out "\n" c))

  ;; Combine non-comment lines
  ;;  (define `e (subst "\n ;" " ;" "\n " "!0" d))
  (define `e (subst "\n !0" "\n!0" d))

  e)


(define (get-comment-text comments)
  (define `lines
    (foreach
        line (subst "\n" "\n " comments)
        (patsubst [" %"] "%"
                  (patsubst ";%" "%"
                            (patsubst ";%" "%" line)))))

  ;; Trim trailing newlines to exactly one
  (if comments
      (promote
       (concat
        (subst " " "\n"
               (nth-rest 1 (subst "\n" nil lines)))
        "\n"))))



(data Defns
  (Mod name filename exports sections))


;; Return a vector of records:
;;   { export: <name>, proto: <tokens>, doc: <text>, is-macro: <1|nil> }
;;   { text: <text> }
;;
(define (extract-defns text)
  (define `(get-export line text)
    (let ((v (rest (matchDecl (lexify (promote line)) 1))))
      (if v
          [ (append {export: (first (dict-get "proto" v))}
                    v
                    {doc: text}) ])))

  (declare comment-text)

  (strip
   (foreach line (pre-process text)
            (if (filter ";%" line)
                (begin
                  (set comment-text (get-comment-text line))
                  (if (filter "# ## ###" (word 1 comment-text))
                      (set comment-text nil
                           ;; and return...
                           [ { text: comment-text } ])))
                (set comment-text nil
                     ;; and return...
                     (get-export line comment-text))))))


(define `test1
  (concat ";; # Header\n"
          "\n"
          ";; Comment\n"
          "\n"
          "(define (f a) &public) nil)\n"
          "(define (g a)) nil)\n"
          "(define (h x)\n"
          "   &public) nil)\n"))

(expect (extract-defns test1)
        [ {text: "# Header\n"}
          {export: "f", proto: ["f" " " "a"], doc: "Comment\n" }
          {export: "h", proto: ["h" " " "x"], doc: nil } ])


;;--------------------------------
;; Generate Documentation
;;--------------------------------

(define (expand-template t funcs ...args)
  (define `vsubst
    (foreach
        w (subst "{" " {" "}" " " [t])
        (if (filter "{%" w)
            (begin
              (define `fn (or (dict-get (subst "{" nil  w) funcs) "?"))
              [(apply fn args)])
            w)))
  (promote (subst " " nil vsubst)))


(expect (expand-template "a {f} c" {f: (lambda (a1 a2) (concat a2 a1))} "A" "B")
        "a BA c")

;;--------------------------------

;; Construct the anchor name generated by Markdown for a header.
;; (md-anchor CONTENT) -> ANCHOR
;;
(define md-anchor
  (gen-polysub
   "? . ~ ! @ # $ % ^ & * ( ) = + [ ] \\ { } | ; : ' \" , / < >"
   nil
   (lambda (text) (subst " " "-" (string-lower text)))))


;;(define SPECIALS
;;  "` * & [ ]")

(define (md-funcname text)
  ;; Back-tick escaping quotes all special characters
  (concat "`" text "`"))


;; Generate "| MODULE | EXPORTS |" rows for an index of exports.
;;
(define (fmt-index mod-defns)
  ;; The anchor that will be assigned to the module documentation
  (define `(mod-anchor sections)
    (define `h1
      (first (split "\n" (dict-get "text" (first sections)))))
    (md-anchor (nth-rest 1 (subst "#" nil h1))))

  ;; Vector of function names linked to their descriptions.
  (define `(fmt-exports exports)
    (concat-for
        d exports " "
        (concat "[" (md-funcname (first (dict-get "proto" d))) "]"
                "(#"
                (md-anchor (concat-vec (dict-get "proto" d)))
                ")")))

  (define `(fmt-row name exports sections)
    (sprintf "| [%s](#%s) | %s |\n"
             name (mod-anchor sections) (fmt-exports exports)))

  (concat-for m mod-defns nil
              (case m
                ((Mod name _ exports sections)
                 (fmt-row name exports sections)))))


;; Generate sections for each module including module comments and
;; documentation for each export.
;;
(define (fmt-modules mod-defns)
  (define `(fmt-proto proto)
    (concat (first proto)
            (concat-vec (string-upper (rest proto)))))

  (define `(fmt-module name exports sections)
    (concat
     ;; Module-level documentation
     "\n\n"
     (concat-for e sections nil
                 (concat (dict-get "text" e) "\n"))
     "## Exports\n\n"
     ;; Exports and export documentation
     (concat-for d exports "\n\n"
                 (concat "##### `(" (fmt-proto (dict-get "proto" d)) ")`\n\n"
                         (dict-get "doc" d)))))

   (concat-for m mod-defns nil
               (case m
                 ((Mod name _ exports sections)
                  (fmt-module name exports sections)))))


;; Construct a markdown document documenting one or more modules.
;;
;; MOD-DEFNS = vector of Mod records
;;
(define (fmt-doc template mod-defns)
  (expand-template template
                   {index: fmt-index, modules: fmt-modules}
                   mod-defns))


;;--------------------------------
;; main
;;--------------------------------


(define (perror fmt ...args)
  (print (vsprintf fmt args))
  1)


;; Return 1 on error; `nil` otherwise.
;;
(define (warn-undef name filename exports sections)
  (vec-or
   (append

    (if (not sections)
        (perror "%s:1: ERROR: no module documentation" filename))

    (for d exports
         (if (not (dict-get "doc" d))
             (perror "%s: ERROR: no documentation for (%s)"
                     filename
                     (concat-vec (dict-get "proto" d))))))))


(define template
  (concat
   "# SCAM Libraries\n"
   "\n"
   "| Module | Exports |\n"
   "| :-- | :-- |\n"
   "{index}\n"
   "{modules}\n"))


;; Return nil on success; 1 on failure.
;;
(define (generate infiles outfile)

  (define `mod-defns
    (for file infiles
         (let ((defns (extract-defns (read-file file))))
           (Mod (basename (notdir file))
                file
                (sort (filter "export%" defns))
                (filter "text%" defns)))))

  (let ((md (sort mod-defns)))
    (or
     ;; Report errors; exit if an error was detected.
     (vec-or
      (for e md
           (case e
             ((Mod n f e s) (warn-undef n f e s)))))

     (let ((err (write-file outfile (fmt-doc template md))))
       (if err
           (perror "scamdoc: error writing file: %s" err))))))


(define (main argv)

  (let ((omap (getopts argv "-o=")))
    (define `(opt name)
      (dict-get name omap))

    (cond
     ((not (opt "o"))
      (perror "scamdoc: no output file provided"))

     ((not (opt "*"))
      (perror "scamdoc: no input files provided"))

     (else
      (generate (opt "*") (last (opt "o")))))))
