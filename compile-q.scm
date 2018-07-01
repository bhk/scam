(require "core")
(require "gen")
(require "io")
(require "compile" &private)

;; strip-comments

(expect ["a" "" "b"] (skip-comments ["#Comment" "" "# comment 2" "a" "" "b"]))


;; literal-filter-out

(expect "a d" (literal-filter-out "b c" "a b c d"))
(expect "a d" (literal-filter-out "b c %" "a b c % d"))

;; descendants

(define `map {1:[2 3 4], 2:[5 4 3 6], 3: [4 7]})

(expect [1 2 3 4 5 6 7]
        (descendants (lambda (a) (dict-get a map)) [1]))

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

;; scan-object
(let-global ((modid-read-lines (lambda (id)
                                 (expect id "'test")
                                 [ "# comment"
                                   "# Requires: 'core 'io"
                                   "# comment" ])))

  (expect ["'core" "'io"]
          (modid-deps "'test")))


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
             (modid-import (lambda () nil)))
  (let ((o (compile-text "(require \"r\")" "" "(test)" "test.tmp")))
    (expect "" (dict-get "errors" o))
    (expect "'r" (dict-get "require" o))
    (expect 1 (see "$(call ^require,'r)\n"
                   (dict-get "code" o)))))




;; compile-module

(define harness-FS
  { "foo.scm": "(define x 1)",
    ".scam/runtime.min": "# empty" })

(define (harness-read-file name)
  (dict-get name harness-FS))

(define (harness-read-lines name)
  (split "\n" (harness-read-file name)))

(define *written* "")
(define (harness-write-file name data)
  (set *written* (append *written* {=name: data}))
  nil)

(let-global ((write-file harness-write-file)
             (read-file harness-read-file)
             (read-lines harness-read-lines)
             (*is-quiet* 1)
             (*obj-dir* ".out/"))

  (compile-module "foo.scm")
  (let ((min (dict-get ".out/foo.min" *written*)))
    (expect 1 (see "# Exports: x" min))
    (expect 1 (see "x := 1" min))))
