;; runtime-q

;; It is unusual to require "runtime", and not ordinarily supported, since
;; it must be loaded before `require` can be called.  runtime-q needs
;; private symbols, so it requires runtime explicitly.  We set *require* to
;; prevent runtime from being eval'ed again.
(declare *required*)
(set *required* "runtime")
(require "runtime.scm" &private)


;; Many of the runtime functions are tested by calling the "manifest
;; functions" that expose their functionality.  For example, "set-native" makes
;; use of `^set`.

(define (expect-x o i file-line)
  (if (findstring (concat o 1) (findstring (concat i 1) (concat o 1)))
      ""
      (error
       (print file-line ": error: assertion failed"
              "\nA: '" o "'"
              "\nB: '" i "'\n"))))

(define `(expect o i)
  (expect-x o i (current-file-line)))


;; ^u
;; ^d

(expect "a !b!0\t\nc" (promote (word 1 (demote "a !b!0\t\nc"))))

;; ^n

(expect "a b" (nth 2 (concat "1 " (demote "a b") " 3")))

;; ^set

(set-native ".v" "$$")
(expect (value ".v") "$$")
(expect (call ".v") "$$")

;; ^fset

(set-native-fn ".f" "$$")
(expect (value ".f") "$$")
(expect (call ".f") "$")

;; ...

(expect ( (lambda (...x) x) 1 2 "" "3 4" "\n" "")
        [1 2 "" "3 4" "\n"] )
(expect ( (lambda (...x) x) 1 2 3 4 5 6 7 8 9 10 11 "")
        [1 2 3 4 5 6 7 8 9 10 11])
(expect (.foreach "N" 2 (call "^v" 1 2 3))
        [2 3])

;; apply

(define (rev a b c d e f g h i j k)
  (concat k j i h g f e d c b a))

(expect "321" (apply rev [1 2 3]))
(expect "11 10 321" (apply rev [ 1 2 3 "" "" "" "" "" "" "10 " "11 "]))

(define (indexarg n ...args)
  (nth n args))

(expect "x" (apply indexarg
                   "24 a b c d e f g h i j k l m n o p q r s t u v w x y z"))

;; name-apply

(expect "11 10 321" (name-apply (native-name rev)
                                [ 1 2 3 "" "" "" "" "" "" "10 " "11 "]))


;; esc-LHS

(declare (esc-LHS str))
(expect (esc-LHS "a= c ")
        "$(if ,,a= c )")
(expect (esc-LHS "a\nb")
        "$(if ,,a$'b)")
(expect (esc-LHS ")$(")
        "$(if ,,$]$$$[)")


;; ^E

(expect (^E "$,)")
        "$(if ,,$`,$])")

(expect (^E "$" "`")
        "$`(if ,,$``)")

(define `(TE str)
  (expect ((^E str))
          str)
  ;; escape twice, expand twice
  (expect (((^E str "`")))
          str))

(TE " ")
(TE "$,)(")
(TE "a")
(TE "a b")
(TE "a$")
(TE "a$1")
(TE "a$2")
(TE ",")
(TE "x\ny")
(TE "$(")
(TE "$)")
(TE "a,b")
(TE "x), (a")
(TE " a ")

;; misc. macros and functions

(expect "" nil)

(expect "1" (not nil))
(expect nil (not "x"))

(expect nil (bound? "_xya13"))
(expect "1" (bound? (native-name ^R)))

(expect "4 5" (nth-rest 4 "1 2 3 4 5"))

(expect "! 1" (first ["! 1" 2]))

(expect "b c" (rest "a   b c  "))

(expect "3 4" (rrest "1 2 3 4"))

(expect "bbc bhi" (filtersub "a%" "b%" "abc def ahi"))


;; atexits

(define at-exit-worked nil)
(at-exit (lambda () (set at-exit-worked 1)))
(run-at-exits)
(expect 1 at-exit-worked)


;;------------------------------------------------------------------------
;; tracing
;;------------------------------------------------------------------------

;; test harness utilities

(define (concat-vec v delim)
  (promote (subst " " [delim] v)))

(define *log* nil)

(define (log str)
  (set *log* (concat *log* str)))
(define (logln str)
  (set *log* (concat *log* str "\n")))

(define (hijack-info fnbody)
  (subst "$(info " (concat "$(call " (native-name logln) " ,") fnbody))

(define (hook-trace-info a ?b ?c ?d)
  (log (concat a b c d "\n")))


;; trace-digits

(expect ":0" (trace-digits "/"))
(expect ":::::::0" (trace-digits "///////"))
(expect ":::::130" (trace-digits "/////1/111/"))
(expect "::::0" (trace-digits "////"))

;; trace-words

(expect "1 1 1 " (trace-words 3))


;;-------- trace-body


;; FX is a target function for get-body test.
;;
(define (fx a b c d e f g h i j k)
  (logln "fx")
  (if b
      (concat a (fx b c d e f g h i j k nil))
      a))

(define initial-fx fx)

(let-global ((*log* nil))
  (expect "123456789ab" (fx 1 2 3 4 5 6 7 8 9 "a" "b"))
  (expect 11 (words (subst "\n" " " *log*))))


;;-------- trace-body c


(set fx (trace-body "c" "FX" "ID" initial-fx))
(expect "///////" (value (count-var "ID")))
(expect "123456789ab" (fx 1 2 3 4 5 6 7 8 9 "a" "b"))
(expect "//////1/1" (value (count-var "ID")))
(fx 1 2 3 4 5 6 7 8 9 "a" "b")
(expect "//////11/11" (value (count-var "ID")))


;;-------- trace-body x

(set-native-fn (save-var "ID") initial-fx)
(set fx (trace-body "x" "FX" "ID" initial-fx))

(let-global ((*log* nil))
  (expect "123456789ab" (fx 1 2 3 4 5 6 7 8 9 "a" "b"))
  (expect 121 (words (subst "\n" " " *log*))))

(let-global ((*log* nil))
  (set fx (trace-body "x1" "FX" "ID" initial-fx))
  (expect "123456789ab" (fx 1 2 3 4 5 6 7 8 9 "a" "b"))
  (expect 11 (words (subst "\n" " " *log*))))


;;-------- trace-body p

(set fx (trace-body (concat "p" [(lambda () (log "P"))])
                        "FX" "ID" initial-fx))

(let-global ((*log* nil))
  (expect "123456789ab" (fx 1 2 3 4 5 6 7 8 9 "a" "b"))
  (expect  "Pfx\nPfx\nPfx\nPfx\nPfx\nPfx\nPfx\nPfx\nPfx\nPfx\nPfx\n" *log*))


;;-------- trace-body t

(declare (^tp name value) &native)

(set fx (hijack-info (trace-body "t" "FX" "ID" initial-fx)))

(let-global ((^tp (lambda (name value)
                    (log (concat name " " value "\n"))
                    value))
             (*log* nil))
  (expect "123" (fx 1 2 3 nil nil nil nil nil nil nil nil))
  (expect *log*
          (concat-vec [ "--> (FX \"1\" \"2\" \"3\")"
                        "fx"
                        " --> (FX \"2\" \"3\" \"\")"
                        "fx"
                        "  --> (FX \"3\" \"\" \"\")"
                        "fx"
                        "  <-- FX: 3"
                        " <-- FX: 23"
                        "<-- FX: 123"
                        "" ]
                      "\n")))

;;-------- trace-body f

(let-global ((^tp (lambda (name value)
                    (log (concat name " " value "\n"))
                    value))
             (fx (hijack-info (trace-body "f" "FX" "ID" initial-fx)))
             (*log* nil))

  (expect "12" (fx 1 2 nil nil nil nil nil nil nil nil nil))
  (expect *log*
          (concat-vec [ "--> FX"
                        "fx"
                        " --> FX"
                        "fx"
                        " <-- FX"
                        "<-- FX"
                        "" ]
                      "\n")))

;;-------- trace-match

(expect "'ab 'ac" (trace-match "%" "ab 'ab 'ac `c `az"))
(expect "`az" (trace-match "`a%" "az 'ab 'ac `c `az"))
(expect "`c `az" (trace-match "`%" "ab ac `c `az"))
(expect "ab" (trace-match "\"ab" "ab 'ab `ab ^ab"))
(expect "^ab" (trace-match "\"^ab" "ab 'ab `ab ^ab"))

;;-------- *trace-ids* & related functions

(expect nil (trace-id "&&&"))
(expect 0 (trace-id "name1" 1))
(expect 1 (trace-id "name2" 1))
(expect 0 (trace-id "name1" 1))
(expect "name1 name2" known-names)

;;-------- trace-ext

(set fx initial-fx)
(define `fx-name (native-name fx))
(define `fx-id (trace-id fx-name))
(define `fx-save (value (save-var fx-id)))
(define `fx-count (subst ":" "" (trace-digits (value (count-var fx-id)))))

;; do not trace non-functions
(define data nil)
(expect nil (trace (native-name data)))

;; *do-not-trace*
(let-global ((*do-not-trace* (concat *do-not-trace* " " fx-name)))
  (expect nil (trace (concat fx-name ":" "p" [(lambda () (log "P"))])))
  (expect fx initial-fx))

;; trace instruments and backs up
(set *log* nil)
(expect fx-name (trace (concat fx-name ":" "p" [(lambda () (log "P"))])))
(expect initial-fx fx-save)
(expect "123" (fx 1 2 3 nil nil nil nil nil nil nil nil))
(expect "Pfx\nPfx\nPfx\n" *log*)

;; trace `c` replaces previous intrumentation
(set *log* nil)
(expect fx-name (trace (concat fx-name ":c")))
(expect 0 fx-count)
(expect initial-fx fx-save)
(expect "123" (fx 1 2 3 nil nil nil nil nil nil nil nil))
(expect "fx\nfx\nfx\n" *log*)
(expect 3 fx-count)
(define fx-c fx)

;; trace `x:-` removes `x` from instrumentation
(expect nil (trace (concat fx-name " " fx-name ":-")))
(expect nil (trace (concat fx-name " %:-")))
(expect fx-name (trace (concat fx-name " foo:-")))
(expect 3 fx-count)

;; repeated trace `c` does not re-zero counts and does not change instrumentation
(expect fx-name (trace (concat fx-name ":c")))
(expect 3 fx-count)
(expect fx-c fx)

;; trace with default mode replaces instrumented version
(set *log* nil)
(expect fx-name (trace fx-name))
(expect initial-fx fx-save)
(expect "-->" (findstring "-->" fx))
(expect 3 fx-count)

(let-global ((trace-info hook-trace-info)
             (*log* nil))

  ;; untrace restores original behavior, logs counts, and resets counts
  (set *log* nil)
  (untrace fx-name)
  (expect fx initial-fx)
  (expect 0 fx-count)
  (expect *log* (subst "FX" fx-name "       3 FX\n")))


;; prevent spurious logging on exit

(set *trace-ids* nil)
