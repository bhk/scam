(require "core.scm")
(require "gen.scm")
(require "io.scm")
(require "memo.scm")
(require "compile.scm" &private)
(require "gen-testutils.scm" &private)


;; strip-comments

(expect ["a" "" "b"] (skip-comments ["#Comment" "" "# comment 2" "a" "" "b"] nil))
(expect ["# comment 2" "a" "" "b"] (skip-comments ["#Comment" "" "# comment 2" "a" "" "b"] ["# c%"]))

;; descendants

(define `map {1:[2 3 4], 2:[5 4 3 6], 3: [4 7]})

(expect [1 2 3 4 5 6 7]
        (descendants (lambda (a) (dict-get a map)) [1]))

;;----------------------------------------------------------------
;; Env importing/exporting
;;----------------------------------------------------------------

;; A has no imported bindings
(define mod-A-env
  { g: (EFunc "x" "F" 1),
    q: (EFunc "p" "Q" 1) })

;; env-compress

(for s [",.;[]\\!1!0!11!10!021!10 !. !@#$%^&*()_+=|}{\\][/.,?><';\":`~,i x!=F!0x v!=V!0x"]
     (expect s (env-expand (env-compress s))))

;; tokenize-key, detokenize-key:

(define `(tok-test env)
  (eq? env (detokenize-key (tokenize-key env))))

(expect 1 (tok-test {a: "a"}))
(expect 1 (tok-test {a: "a%a!p!P!%"}))
(expect 1 (tok-test {"%": "a%a!p!P!%"}))

;; exporting and importing bindings

(define (export-round-trip env all)
  (env-parse (split "\n"
                    (concat "# comment\n" (env-export-lines env) "# f f f\n"))
             all))

(define e1
  { f: (EFunc "x" "f" 2),
       x: (EVar "x" "X"),
       g: (EFunc "p" "g" 1),  ;; private
       g: (EFunc "i" "g" 1),  ;; imported, shadowed
       z: (EFunc "i" "z" 1),  ;; imported
       m: (EIL "x" "" nil),
       "a:n\n,x": (EVar "x" "xyz") })

;; import public members
(fexpect (export-round-trip e1 nil)
         { f: (EFunc "i" "f" 2),
           x: (EVar "i" "X"),
           m: (EIL "i" "" nil),
           "a:n\n,x": (EVar "i" "xyz")})

;; import public AND private members
;;   Only ony definition for `g` is retained because the dictionary is compacted.
;;   Public members preceded private members.
;;   All entries are marked scope="i"
(fexpect (export-round-trip e1 1)
         { f: (EFunc "i" "f" 2),
           x: (EVar "i" "X"),
           m: (EIL "i" "" nil),
           "a:n\n,x": (EVar "i" "xyz"),
           g: (EFunc "i" "g" 1)})


;;--------------------------------------------------------------
;; Module Management
;;--------------------------------------------------------------

;; module-id

(let-global ((*is-boot* 1))
  (expect "core" (module-id "core.scm")))

(let-global ((*is-boot* nil)
             (*obj-dir* ".obj/"))
  (expect ".obj/a+0b.scm" (module-id "a b.scm"))
  (expect ".obj/+./b.scm" (module-id "../b.scm")))


;; modid-deps & modid-read-lines
(set-native "[mod-cqtx]" "# Requires: a!0b var\n# xyz")
(define test-dir (get-tmp-dir))
(write-file (concat test-dir "cqtx.o") "# Requires: a!0b boot-file\n# xyz\n")
(write-file (concat test-dir "cqtx.scm.o") "# Requires: .tmp/a!0b .tmp/file\n# xyz\n")
(write-file (concat test-dir "nil.scm.o") "# Requires: \n")


(let-global ((*is-boot* nil)
             (*obj-dir* test-dir))
  (expect "runtime" (runtime-module-name nil))

  (expect (modid-deps (module-id "cqtx.scm"))
          [".tmp/a b" ".tmp/file"])
  (expect (modid-deps-all (module-id "nil.scm"))
          [(concat test-dir "nil.scm") "runtime"]))


(let-global ((*is-boot* 1)
             (*obj-dir* test-dir))
  (expect ["a b" "boot-file"] (modid-deps (module-id "cqtx"))))



;; locate-source

(declare SCAM_LIBPATH &native)

;; Assert: Source file is found in base directory.
(expect "compile.scm"
        (let-global ((SCAM_LIBPATH nil))
          (locate-source "./" "compile.scm")))

;; Assert: Source file is found in a SCAM_LIBPATH directory.
(expect "test/run.scm"
        (let-global ((SCAM_LIBPATH "test:x/y/z"))
          (locate-source "a/b/c/" "run.scm")))


;;--------------------------------
;; (require MOD)
;;--------------------------------


(let-global ((get-module (lambda () (ModError "no worky"))))

  ;; too many arguments
  (expect (c0-ser "(require \"mod\" foo)")
          "!(PError 8 'too many arguments to require')")

  ;; non-string
  (expect (c0-ser "(require MOD)")
          (concat "!(PError 4 'invalid STRING in (require STRING); "
                  "expected a literal string')"))

  ;; get-module failure
  (expect (c0-ser "(require \"mod\")")
          "!(PError 4 'require: no worky')"))


(define (mock-get-module name base private)
  (ModSuccess (subst ".scm" "" name)
              {f: (EVar "i" "f")}))

(let-global ((get-module mock-get-module))
  ;; get-module success
  (expect (c0-ser "(require \"mod.scm\")")
          "(^R mod!(ICrumb 'require' 'mod'))"))


;;--------------------------------------------------------------
;; parse-and-gen
;;--------------------------------------------------------------

;; for file
(let ((o (parse-and-gen "(define a &native 1)" "" "(test)" "test.tmp")))
  (expect "" (dict-get "errors" o))
  (expect "a := 1\n" (dict-get "code" o))
  (expect (dict-get "a" (dict-get "env" o))
          (EVar "p" "a")))

;; for eval
(let ((o (parse-and-gen "(define a &native 1)" "" "(test)" "")))
  (expect "" (dict-get "errors" o))
  (expect "$(call ^set,a,1)" (dict-get "code" o))
  (expect { a: (EVar "p" "a") }
          (dict-get "env" o)))


(expect "a := 1\nb := 2\n"
        (dict-get "code"
                  (parse-and-gen (concat "(define a &native 1) "
                                        "(define b &native 2)")
                                "" "(test)" "test.tmp")))
