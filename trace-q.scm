;;------------------------------------------------------------------------
;; tests
;;------------------------------------------------------------------------
(require "core")
(require "trace" &private)


;; test harness utilities

(define *log* nil)

(define (log str)
  (set *log* (concat *log* str)))
(define (logln str)
  (set *log* (concat *log* str "\n")))

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

(define old-fx fx)

(let-global ((*log* nil))
  (expect "123456789ab" (fx 1 2 3 4 5 6 7 8 9 "a" "b"))
  (expect 11 (words (strip *log*))))


;;-------- trace-body c


(set fx (trace-body "c" "FX" "ID" old-fx))
(expect "///////" (value (count-var "ID")))
(expect "123456789ab" (fx 1 2 3 4 5 6 7 8 9 "a" "b"))
(expect "//////1/1" (value (count-var "ID")))
(fx 1 2 3 4 5 6 7 8 9 "a" "b")
(expect "//////11/11" (value (count-var "ID")))


;;-------- trace-body x

(set-rglobal (save-var "ID") old-fx)
(set fx (trace-body "x" "FX" "ID" old-fx))

(let-global ((*log* nil))
  (expect "123456789ab" (fx 1 2 3 4 5 6 7 8 9 "a" "b"))
  (expect 121 (words (strip *log*))))

(let-global ((*log* nil))
  (set fx (trace-body "x1" "FX" "ID" old-fx))
  (expect "123456789ab" (fx 1 2 3 4 5 6 7 8 9 "a" "b"))
  (expect 11 (words (strip *log*))))


;;-------- trace-body p

(set fx (trace-body (concat "p" [(lambda () (log "P"))])
                        "FX" "ID" old-fx))

(let-global ((*log* nil))
  (expect "123456789ab" (fx 1 2 3 4 5 6 7 8 9 "a" "b"))
  (expect  "Pfx\nPfx\nPfx\nPfx\nPfx\nPfx\nPfx\nPfx\nPfx\nPfx\nPfx\n" *log*))


;;-------- trace-body t

(declare (^tp name value) &global)

(set fx (subst "info " (concat "call " (global-name logln) " ,")
               (trace-body "t" "FX" "ID" old-fx)))

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


;;-------- trace-match

(expect "ab ac" (trace-match "%" "ab ac ~c ~az"))
(expect "~az" (trace-match "~a%" "ab ac ~c ~az"))
(expect "~c ~az" (trace-match "~%" "ab ac ~c ~az"))

;;-------- *trace-ids* & related functions

(expect nil (trace-id "&&&"))
(expect 0 (trace-id "name1" 1))
(expect 1 (trace-id "name2" 1))
(expect 0 (trace-id "name1" 1))
(expect "name1 name2" known-names)

;;-------- trace-action

(let-global ((trace-info hook-trace-info)
             (*log* nil))

  (expect "t" (trace-action "v" "tfunc"))  ;; default to "t" and log

  (define `p-action
    (concat "p" [(lambda () (info "HI"))]))
  (expect p-action
          (trace-action (concat "v" p-action) "pfunc"))

  (expect *log*
          (concat "[t] tfunc\n"
                  "[p$(info!0HI)] pfunc\n")))


;;-------- trace-ext

(define `FX (global-name fx))
(define `FXVAL (value (save-var (trace-id FX))))
(set fx old-fx)

;; ignore-vars
(expect nil (trace-ext (concat FX ":" "p" [(lambda () (log "P"))]) FX))
(expect fx old-fx)

;; trace instruments and backs up
(set *log* nil)
(expect FX (trace-ext (concat FX ":" "p" [(lambda () (log "P"))]) nil))
(expect old-fx FXVAL)
(expect "123" (fx 1 2 3 nil nil nil nil nil nil nil nil))
(expect "Pfx\nPfx\nPfx\n" *log*)

;; trace `c` is additive
(set *log* nil)
(expect FX (trace-ext (concat FX ":c") nil))
(define `FXC
  (subst ":" "" (trace-digits (value (count-var (trace-id FX))))))
(expect 0 FXC)
(expect old-fx FXVAL)
(expect "123" (fx 1 2 3 nil nil nil nil nil nil nil nil))
(expect "Pfx\nPfx\nPfx\n" *log*)
(expect 3 FXC)

;; trace `x` replaces instrumented version
(set *log* nil)
(expect FX (trace-ext (concat FX ":x2") nil))
(expect old-fx FXVAL)
(expect "12" (fx 1 2 nil nil nil nil nil nil nil nil nil))
(expect "fx\nfx\nfx\nfx\n" *log*)
(expect 3 FXC)

;; trace with default action replaces instrumented version
(set *log* nil)
(expect FX (trace-ext FX nil))
(expect old-fx FXVAL)
(expect "-->" (findstring "-->" fx))

(let-global ((trace-info hook-trace-info)
             (*log* nil))

  ;; untrace restores original behavior, logs counts, and resets counts
  (set *log* nil)
  (untrace-ext FX)
  (expect fx old-fx)
  (expect 0 FXC)
  (expect *log* (subst "FX" FX "       3 FX\n")))


;; prevent spurious logging on exit

(set *trace-ids* nil)
