;;--------------------------------
;; scam-ct.scm
;;--------------------------------

;; This file is an implicit "use" dependency of ordinary SCAM programs.

;; This is needed only to pull in runtime exports.  (Both scam-ct.scm and
;; runtime.scm are given no implicit dependencies.)  Unfortunately, this
;; will re-execute runtime.min, unless we re-define require.
(declare (^require f) &global)
(define r.sav ^require)
(set ^require (lambda (f) ""))

(require "runtime")

;; (when COND EXPR...)
;;
(defmacro (when args)
  (define `cond (first args))
  (define `rest (rest args))

  `(if ,(first args) (begin ,@rest)))


;; (unless COND EXPR...)
;;
(defmacro (unless args)
  (define `cond (first args))
  (define `rest (rest args))

  `(if ,cond nil (begin ,@rest)))

;;--------------------------------
(set ^require r.sav)
;;--------------------------------
