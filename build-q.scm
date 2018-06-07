(require "gen")
(require "core")
(require "io")
(require "build" &private)

;; strip-comments

(expect "a\nb\n" (strip-comments "#Comment\n\n# comment 2\na\nb\n"))
(expect "" (strip-comments ""))

;; traverse-graph

(define nodemap
   { a: "b c",
     a: "b c",
     b: "c d e",
     c: "f",
     d: "g" })

(expect "a c b f d e g"
        (traverse-graph "a c"
                        (lambda (node) (dict-get node nodemap))
                        (lambda (node) (concat "{" node "}"))))


;; scan-source

(expect [ ["req/a!1" "req/b"]
          ["use/a" "use/b"] ]
        (scan-source "test/build-q.txt"))

;; scan-object


(set-global "[mod-'builtin-test]"
            (concat "# comment\n"
                    "# Requires: 'core 'io\n"
                    "# Uses: 'a 'b\n"
                    "# comment\n"))

(expect [ ["'core" "'io"]
          ["'a" "'b"] ]
        (scan-builtin "'builtin-test"))


;;----------------------------------------------------------------
;; compile-rule
;;----------------------------------------------------------------

(set *is-boot* nil)

(let ((rule (compile-rule "p"
                          "p.src"
                          ["p.obj"]
                          ["a-q.ok"]
                          ["m1" "m2"]
                          nil)))
  (let ((lines (split "\n" rule)))
    (expect (subst "SELF" (firstword MAKEFILE_LIST)
                   "p: p.src p.obj SELF | a-q.ok")
            (first lines))
    (expect 1 (see "\t@: " (nth 2 lines)))
    (expect 1 (see "compile" (nth 2 lines)))))

;;----------------------------------------------------------------
;; scan-modules
;;----------------------------------------------------------------

(define `(scan sources is-boot mmap-in ?env)
  (set *is-boot* is-boot)
  (set *obj-dir* "out/")
  (scan-modules env sources mmap-in))

(define `(sexpect a b)
  (expect-x (sort a) (sort b) (current-file-line)))


(declare *files*)
(define (test-scan-deps file)
  (rest (assoc file *files*)))

(define (test-file-exists? file)
  (if (assoc file *files*) file))

(let-global ((scan-deps test-scan-deps)
             (file-exists?   test-file-exists?))

  ;; Assert: detects `-q` tests
  ;; Assert: follows dependencies (including paths)
  ;; Assert: does not use bundles when `rebundle` is nil
  ;; Assert: MMAP records are correct for found sources & not-found files

  (set *files* [; filename     requires      uses
                ["a.scm"       "x/b.scm"             ]
                ["x/b.scm"     "x/r.scm"     "'core" ]
                ["x/b-q.scm"                         ]
                ["x/r.scm"                           ]
                ["'core"                             ] ])

;;  (sexpect [ ["a"    "a.scm"      "out/a.min"    nil    "b"   nil  1]
;;             ["b"    "x/b.scm"    "out/b.min"    "b-q"  "r"   "u"  1]
;;             ["b-q"  "x/b-q.scm"  "out/b-q.min"  nil    nil   nil  1]
;;             ["r"    "x/r.scm"    "out/r.min"    nil    nil   nil  1]
;;             ["u"    nil          "x/u.scm"      nil    nil   nil  1] ]
;;           (scan ["a.scm"] nil 1 nil))
  (sexpect [ ["a.scm"      nil          "x/b.scm"  nil      "RC"]
             ["x/b.scm"    "x/b-q.scm"  "x/r.scm"  "'core"  "RC"]
             ["x/b-q.scm"  nil           nil       nil      "RC"]
             ["x/r.scm"    nil           nil       nil      "RC"]
             ["'core"      nil           nil       nil      "RC"] ]
           (scan ["a.scm"] nil nil))

  ;; Assert: `rt` and `ct` are heeded.

  (set *files* [["a.scm"]
                ["b.scm"] ])

  (set-global "[mod-'xx]" "# nothing")

  (define mm0 (scan ["a.scm"] nil nil { rt: "'xx" }))
  (sexpect [ ["a.scm" nil "'xx" nil "C"] ]
           mm0)

  (define mm0 (scan ["b.scm"] nil nil { rt: "'rt", ct: "'ct" }))
  (sexpect [ ["b.scm" nil "'rt" "'ct" ""] ]
           mm0))
