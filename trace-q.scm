;; trace-q

(require "core.scm")
(require "trace.scm" &private)

;; test harness utilities

(define *log* nil)

(define (log str)
  (set *log* (.. *log* str)))
(define (logln str)
  (set *log* (.. *log* str "\n")))

(define (hijack-info fnbody)
  (subst "(info " (.. "(call " (native-name logln) ",") fnbody))

(define (lines str)
  (words (subst "\n" "\n " [str])))

(set tf-pv (hijack-info tf-pv))
(set trace-match (hijack-info trace-match))
(set trace (hijack-info trace))
(set untrace (hijack-info untrace))

;;-------- trace-prefix, trace-unprefix

(expect "'abc" (trace-prefix "abc"))
(expect "`abc" (trace-prefix "`abc"))
(expect "abc" (trace-prefix "\"abc\""))
(expect nil (trace-prefix "\"\""))

(expect "abc" (trace-unprefix "'abc"))
(expect "`abc" (trace-unprefix "`abc"))
(expect "\"abc\"" (trace-unprefix "abc"))


;;-------- E?, R?

(expect "" (TI++))
(expect " " (TI++))
(expect "  " (TI++))
(expect "  " (--TI))
(expect " " (--TI))
(expect "" (--TI))


;;-------- E?, R?

;; In order to run in .out/a/scam, we do not make assumptions about the
;; runtime we are running under... so we use this simulation of the runtime
;; that will be used in the completed compiler.
(define (_t ...) nil)
(set _t (.. "$(" (native-name E?) ")"
            "$(call " (native-name R?) ",$1,$(call $1,$2,$3,$4,$5,$6,$7,$8,"
            "$(call ^n,1,$9),$(wordlist 2,9999,$9)))"))

(declare (E? ...))
(declare (R? ...))
(set E? (hijack-info E?))
(set R? (hijack-info R?))

(let-global ((*log* nil))
  (define (test-? a b c d e f g h i j) j)
  (define `upName (trace-unprefix (native-name test-?)))

  (expect "a b" (_t (native-name test-?) 1 2 3 4 5 6 7 8 9 "a b"))
  (expect *log* (.. "--> (" upName " 1 2 3 4 5 6 7 8 9 \"a b\")\n"
                    "<-- " upName ": \"a b\"\n")))


;;-------- tally-X


(expect 0 (tally-decimal tally-zero))
(expect 1 (tally-decimal (tally-1+ tally-zero)))
(expect 100 (tally-decimal (foldl tally-1+ tally-zero (urange 1 100))))


;;-------- *trace-ids* & related functions

(expect nil (trace-id "&&&"))
(expect 0 (trace-id "name1" 1))
(expect 1 (trace-id "name2" 1))
(expect 0 (trace-id "name1" 1))
(expect "name1 name2" (dict-keys *trace-ids*))


;;-------- trace-body


;; FX is a target function for get-body test.
(define (fx a ...other)
  (logln a)
  (if a
      (.. a (apply fx other))
      "!"))

(let-global ((*log* nil))
  (expect "123456789a b!" (fx 1 2 3 4 5 6 7 8 9 "a b"))
  (expect 11 (lines *log*))
  (expect "a b2!" (fx "a b" 2)))

;; set save-var (referenced by trace-body result)
(define fxID (trace-id (native-name fx) 1))
(set-native-fn (save-var fxID) fx)


;; trace-body t

;; ASSERT: arg lengths 1 .. 11 are handled
;; ASSERT: proper argument formatting
;; ASSERT: result is unchanged
;; ASSERT: proper indentation


(let-global ((fx (hijack-info (trace-body "t" "'fx" fxID fx)))
             (*log* nil))
  (expect "a b2!" (fx "a b" 2))
  (expect *log*
          (concat-vec [ "--> (fx \"a b\" 2)"
                        "a b"
                        " --> (fx 2)"
                        "2"
                        "  --> (fx)"
                        ""
                        "  <-- fx: \"!\""
                        " <-- fx: \"2!\""
                        "<-- fx: \"a b2!\""
                        "" ]
                      "\n")))


;; trace-body f

(let-global ((fx (hijack-info (trace-body "f" "FX" fxID fx)))
             (*log* nil))

  (expect "1!" (fx 1))
  (expect *log*
          (concat-vec [ "--> \"FX\""
                        "1"
                        " --> \"FX\""
                        ""
                        " <-- \"FX\""
                        "<-- \"FX\""
                        "" ]
                      "\n")))


;; trace-body c

;; ASSERT: proper counting & proper results
(let-global ((*log* nil)
             (fx (trace-body "c" "FX" fxID fx)))
  (define `(C name)
    (tally-decimal (native-value (tally-var name))))

  (expect 0 (C fxID))
  (expect "12345678ab!" (fx 1 2 3 4 5 6 7 8 "a" "b"))
  (expect 11 (C fxID))
  (fx 1 2 3 4 5 6 7 8 "a" "b")
  (expect 22 (C fxID)))


;; trace-body x


;; TODO: why?
(expect fx (native-value (save-var fxID)))
(set-native-fn (save-var fxID) fx)

;; ASSERT: proper X1 execution
(let-global ((*log* nil)
             (fx (trace-body "x1" "FX" fxID fx)))
  (expect "12345678a bc!" (fx 1 2 3 4 5 6 7 8 "a b" "c"))
  (expect 11 (lines *log*)))

;; ASSERT: proper multiplication of effort with recursive function
(let-global ((*log* nil)
             (fx (trace-body "x3" "FX" fxID fx)))
  (expect "12345678a bc!" (fx 1 2 3 4 5 6 7 8 "a b" "c"))
  (expect 33 (lines *log*)))


;; trace-body p  (undocumented)

(let-global ((*log* nil)
             (fx (trace-body (.. "p" [(lambda () (log "P"))])
                             "FX" fxID fx)))

  (expect "12345678a bc!" (fx 1 2 3 4 5 6 7 8 "a b" "c"))
  (expect  "P1.P2.P3.P4.P5.P6.P7.P8.Pa b.Pc.P." (subst "\n" "." *log*)))


;;-------- trace-match

(define (zxcv a) a)
;; trace matching assumes user namespace; create one there even if we are
;; running in --boot mode
(if (not (eq? (native-name zxcv) "'zxcv"))
    (set-native-fn "'zxcv" zxcv))
(define (call-zxcv a) (native-call "'zxcv" a))
(define `zxcv-id (trace-id "'zxcv"))
(define `zxcv-tally (native-value (tally-var zxcv-id)))

(expect "" (trace-match "\"\""))
(expect *log* "scam: trace: empty name in trace spec!\n")
(set *log* nil)
(expect "'zxcv" (trace-match "zxcv"))
(expect "" (trace-match "xz"))
(expect *log* nil)
(expect "" (trace-match "xz%"))
(expect *log* nil)


;;-------- spec-name, spec-mode

(expect "foo" (spec-name "foo:x:1"))
(expect "t" (spec-mode "foo"))
(expect "f" (spec-mode "foo:f"))
(expect "a:b" (spec-mode "foo:a:b"))


;;-------- trace, untrace

(set *trace-ids* nil)
(define (bkup-zxcv) nil)

(let-global ((*trace-ids* nil)
             (bkup-zxcv zxcv)
             (*log* nil))
  ;; `-` mode excludes files matching other SPECs
  (expect "" (trace "zxc% zxcv:-"))

  ;; save-var
  (expect "'zxcv" (trace "zxc%"))
  (expect *log* "scam: tracing zxcv [mode=t] ...\n")
  (expect bkup-zxcv (native-value (save-var zxcv-id)))

  ;; restore after untrace
  (set *log* "")
  (untrace "'zxcv" nil)
  (expect bkup-zxcv zxcv)
  (expect *log* nil)

  ;; install counts
  (expect "'zxcv" (trace "zxc%:c"))
  (expect *log* "scam: tracing zxcv [mode=c] ...\n")
  (expect "////////" zxcv-tally)
  (call-zxcv 1)
  (call-zxcv 2)
  (expect 2 (tally-decimal zxcv-tally))

  ;; re-iterated ":c" does not reset counts or report changes
  (set *log* "")
  (expect "'zxcv" (trace "zxc%:c"))
  (expect *log* nil)
  (expect 2 (tally-decimal zxcv-tally))

  ;; report counts
  (set *log* "")
  (untrace "'zxcv" nil)
  (expect bkup-zxcv zxcv)
  (expect *log* "scam: counts: zxcv = 2\n")
  (expect nil zxcv-tally)

  ;; ASSERT: counts were cleared
  (set *log* "")
  (untrace "'zxcv" nil)
  (expect *log* nil))
