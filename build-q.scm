(require "core")
(require "build" &private)

;; traverse-graph

(define nodemap
  (dict-bind "a" "b c"
  (dict-bind "b" "c d e"
  (dict-bind "c" "f"
  (dict-bind "d" "g")))))

(expect "a c b f d e g"
        (traverse-graph "a c"
                        (lambda (node) (dict-get node nodemap))
                        (lambda (node) (concat "{" node "}"))))


;; scan-source, scan-object, scan-deps

(expect [ ["test/req/a!1.txt" "test/req/b.txt"]
          ["test/use/a.txt" "test/use/b.txt"] ]
        (scan-source "test/build-q.txt"))

(expect [ ["test/a/!1b.txt" "test/c.txt"]
          ["test/x/y.txt"] ]
        (scan-object "test/build-q.txt"))

(define ///build-test-mod.min
  &global
  "#Exports: ...\n# Requires: r s\n# Uses: u v\n")
(expect ["///r.min ///s.min" "///u.min ///v.min"]
        (scan-object "///build-test-mod.min"))


;;----------------------------------------------------------------
;; scan-modules
;;----------------------------------------------------------------

(define `(scan sources rebundle is-boot mmap-in)
  (scan-modules (append (dict-bind "odir" "out/")
                        (dict-bind "rebundle" rebundle)
                        (dict-bind "boot" is-boot))
                sources mmap-in))

(define `(sexpect a b)
  (expect-x (sort a) (sort b) (current-file-line)))


(declare *files*)
(define (test-scan-file file)  (rest (assoc file *files*)))
(define (test-if-exists file)  (if (assoc file *files*) file))
(define (test-if-bundled file) (test-if-exists (bundle-var (bundle-path file))))


(let-global ((scan-source test-scan-file)
             (scan-object test-scan-file)
             (if-exists   test-if-exists)
             (if-bundled  test-if-bundled))

  ;; Assert: detects `-q` tests
  ;; Assert: follows dependencies (including paths)
  ;; Assert: does not use bundles when `rebundle` is nil
  ;; Assert: MMAP records are correct for found sources & not-found files

  (set *files* [; filename     requires      uses
                ["a.scm"       "x/b.scm"               ]
                ["x/b.scm"     "x/r.scm"     "x/u.scm" ]
                ["x/b-q.scm"                           ]
                ["x/r.scm"                             ]
                ["///u.scm"                            ] ])

  (sexpect [ ["a"    "a.scm"      "out/a.min"    nil    "b"   nil  1]
             ["b"    "x/b.scm"    "out/b.min"    "b-q"  "r"   "u"  1]
             ["b-q"  "x/b-q.scm"  "out/b-q.min"  nil    nil   nil  1]
             ["r"    "x/r.scm"    "out/r.min"    nil    nil   nil  1]
             ["u"    nil          "x/u.scm"      nil    nil   nil  1] ]
           (scan ["a.scm"] nil 1 nil))

  ;; Assert: use bundles when `rebundle` is 1

  (set *files* [["///runtime.min"]
                ["///scam-ct.min"] ])

  (define mm0 (scan ["runtime.scm" "scam-ct.scm"] 1 1 nil))

  (sexpect [ ["runtime" nil "///runtime.min"  nil nil nil 1]
             ["scam-ct" nil "///scam-ct.min"  nil nil nil 1] ]
           mm0)

  ;; Assert: MMAP records are correct for bundles and MIN files
  ;; Assert: implicit `require` and `use` are added when is-boot=nil
  ;; Assert: dependency satisfied by MIN file

  (set *files* [; filename     requires      uses
                ["a.scm"       "b.scm c.scm"         ]
                ["///b.min"                          ]
                ["c.min"                             ] ])

  (sexpect
   (append [["a"    "a.scm"   "out/a.min"  nil  "b c runtime" "scam-ct" nil]
            ["b"    nil       "///b.min"   nil  "runtime"     "scam-ct" nil]
            ["c"    nil       "c.min"      nil  "runtime"     "scam-ct" nil]]
           mm0)
   (scan ["a.scm"] 1 nil mm0)))
