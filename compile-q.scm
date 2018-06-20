(require "core")
(require "gen")
(require "io")
(require "compile" &private)

;; strip-comments

(expect ["a" "" "b"] (skip-comments ["#Comment" "" "# comment 2" "a" "" "b"]))

;;----------------------------------------------------------------
;; Env importing/exporting
;;----------------------------------------------------------------

;; A has no imported bindings
(define mod-A-env
  { g: (EFunc "F" "x" 1 nil),
    q: (EFunc "Q" "p" 1 nil) })

;; no imports to trim
(expect (env-filter-scope mod-A-env "x p")
        mod-A-env)

;; reconstruct A (include)
(expect (env-strip-imports mod-A-env 1)
        mod-A-env)

;; get public bindinds (import)
(fexpect (env-strip-imports mod-A-env nil)
         {g: (EFunc "F" "i" 1 nil)})


;; env-compress

(for s [",.;[]\\!1!0!11!10!021!10 !. !@#$%^&*()_+=|}{\\][/.,?><';\":`~,i x!=F!0x v!=V!0x"]
     (expect s (env-expand (env-compress s))))


;; tokenize-key, detokenize-key:

(define `(tok-test env)
  (eq? env (detokenize-key (tokenize-key env))))

(expect 1 (tok-test {a: "a"}))
(expect 1 (tok-test {a: "a%a!p!P!%"}))
(expect 1 (tok-test {"%": "a%a!p!P!%"}))


;; import only public members

(fexpect (import-binding "f" (EFunc "F" "x" 2 nil))
         {f: (EFunc "F" "i" 2 nil)})

(fexpect (import-binding "f" (EFunc "F" "p" 2 nil))
         nil)


(define (export-round-trip env flag)
  (env-strip-imports
   (env-parse [ "# comment"
                (subst "\n" "" (env-export-line env "p x"))
                "# F F F F F F"])
   flag))


(fexpect (export-round-trip
          { f: (EFunc "f" "x" 2 nil),
            x: (EVar "X" "x"),
            a: (EFunc "fa" "x" 2 ["a b" (IVar "a")]),
            g: (EFunc "g" "p" 1 nil),  ;; private
            g: (EFunc "g" "i" 1 nil),  ;; imported
            m: (EIL "" "x" NoOp),
            "a:n\n,x": (EVar "xyz" "x") }
          nil)

         { f: (EFunc "f" "i" 2 nil),
           x: (EVar "X" "i"),
           a: (EFunc "fa" "i" 2 ["a b" (IVar "a")]),
           m: (EIL "" "i" NoOp),
           "a:n\n,x": (EVar "xyz" "i")} )


;; import public AND private members

(define mod-C-env
  { f: (EFunc "f" "x" 2 nil),
       x: (EVar "X" "x"),
       ;; other definitions in this module
       g: (EFunc "g" "p" 1 nil),  ;; private
       "a:n\n,x": (EVar "xyz" "x")
       })

(fexpect (export-round-trip mod-C-env 1)
         mod-C-env)


;;--------------------------------------------------------------
;; Module Management
;;--------------------------------------------------------------

;; module-source-deps

(expect ["req/a!1" "req/a2" "req/b"]
        (module-source-deps (concat (dir (current-file))
                                    "test/build-q.txt")))

;; scan-object

(set-global "[mod-'builtin-test]"
            (concat "# comment\n"
                    "# Requires: 'core 'io\n"
                    "# comment\n"))

(expect ["'core" "'io"]
        (module-builtin-deps "'builtin-test"))


;;--------------------------------------------------------------
;; compile-text
;;--------------------------------------------------------------

;; for file
(let ((o (compile-text "(define a &global 1)" "" "(test)" "test.tmp")))
  (expect "" (dict-get "errors" o))
  (expect "a := 1\n" (dict-get "code" o))
  (expect (dict-get "a" (dict-get "env" o))
          (EVar "a" "p")))

;; for eval
(let ((o (compile-text "(define a &global 1)" "" "(test)" "")))
  (expect "" (dict-get "errors" o))
  (expect "$(call ^set,a,1)" (dict-get "code" o))
  (expect { a: (EVar "a" "p") }
          (dict-get "env" o)))


(expect "a := 1\nb := 2\n"
        (dict-get "code"
                  (compile-text (concat "(define a &global 1) "
                                        "(define b &global 2)")
                                "" "(test)" "test.tmp")))


;; require
(declare ^require &global)

(let-global ((module-locate (lambda (f name) (concat "'" name)))
             (^require (lambda () nil))
             (module-import (lambda () nil)))
  (let ((o (compile-text "(require \"r\")" "" "(test)" "test.tmp")))
    (expect "" (dict-get "errors" o))
    (expect "'r" (dict-get "require" o))
    (expect 1 (see "$(call ^require,'r)\n"
                   (dict-get "code" o)))))


(define *written* "")
(define `wname (first *written*))
(define `wtext (nth 2 *written*))


;; compile-file

(define harness-FS
  { "foo.scm": "(define x 1)",
    ".scam/runtime.min": "# empty" })

(define (harness-read-file name)
  (dict-get name harness-FS))

(define (harness-read-lines name)
  (split "\n" (harness-read-file name)))

(let-global ((write-file (lambda (name data) (set *written* [name data]) nil))
             (read-file harness-read-file)
             (read-lines harness-read-lines))

  (compile-file "foo.scm" ".out/foo.min" "rt" "C")
  (expect ".out/foo.min" wname)
  (expect 1 (see "# Exports: x" wtext))
  (expect 1 (see "x := 1" wtext)))
