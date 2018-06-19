(require "gen")
(require "core")
(require "io")
(require "compile")
(require "build" &private)

(define SOURCE_DIR (dir (current-file)))

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

(expect ["req/a!1" "req/a2" "req/b"]
        (scan-source (concat SOURCE_DIR "test/build-q.txt")))

;; scan-object


(set-global "[mod-'builtin-test]"
            (concat "# comment\n"
                    "# Requires: 'core 'io\n"
                    "# comment\n"))

(expect ["'core" "'io"]
        (scan-builtin "'builtin-test"))


;;----------------------------------------------------------------
;; compile-rule
;;----------------------------------------------------------------

(let ((rule (compile-rule "p"
                          "p.src"
                          ["p.obj"]
                          ["a-q.ok"]
                          ["m1" "m2"]
                          nil)))
  (let ((lines (split "\n" rule)))
    (let-global ((*is-boot* nil))
      (expect (subst "SELF" (firstword MAKEFILE_LIST)
                     "p: p.src p.obj SELF | a-q.ok")
              (first lines))
      (expect 1 (see "\t@: " (nth 2 lines)))
      (expect 1 (see "compile" (nth 2 lines))))))

;;----------------------------------------------------------------
;; scan-modules
;;----------------------------------------------------------------

(define `(sort-expect a b)
  (expect-x (sort a) (sort b) (current-file-line)))

(declare *file-deps*)

(define (mock-scan-deps file)
  (nth 2 (assoc file *file-deps*)))

(define (mock-file-exists? file)
  (if (assoc file *file-deps*) file))

(define (scan env sources ?mmap ?is-boot)
  (let-global ((*is-boot* is-boot)
               (*obj-dir* "out/")
               (scan-deps mock-scan-deps)
               (file-exists? mock-file-exists?))
    (scan-modules env sources mmap)))


;; Assert: detects `-q` tests
;; Assert: follows dependencies (including paths)
;; Assert: does not use bundles when `rebundle` is nil
;; Assert: MMAP records are correct for found sources & not-found files

(set *file-deps*
     [; filename     requires
      ["a.scm"       "x/b.scm"       ]
      ["x/b.scm"     "x/r.scm 'core" ]
      ["x/b-q.scm"                   ]
      ["x/r.scm"                     ]
      ["'core"                       ] ])

(sort-expect [ ["a.scm"      nil          "x/b.scm"        "RC"]
               ["x/b.scm"    "x/b-q.scm"  "x/r.scm 'core"  "RC"]
               ["x/b-q.scm"  nil           nil             "RC"]
               ["x/r.scm"    nil           nil             "RC"]
               ["'core"      nil           nil             "RC"] ]
             (scan nil ["a.scm"]))

;; Assert: `rt` and `ct` are heeded.

(set *file-deps* [ ["a.scm" nil]
                   ["b.scm" nil] ])

(set-global "[mod-'xx]" "# nothing")
(sort-expect [ ["a.scm" nil "'xx" "C"] ]
             (scan {rt: "'xx" }
                   ["a.scm"]))

(sort-expect [ ["b.scm" nil "'rt 'ct" nil] ]
             (scan { rt: "'rt", ct: "'ct" }
                   ["b.scm"]))
