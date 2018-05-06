;;--------------------------------
;; scam-ct.scm
;;--------------------------------

;; This file is an implicit "use" dependency of ordinary SCAM programs.

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
