(require "runtime")


;; (when COND EXPR...)
;;
(defmacro (when form)
  (define `cond (nth 3 form))
  (define `rest (nth-rest 4 form))

  `(if ,cond (begin ,@rest)))


;; (unless COND EXPR...)
;;
(defmacro (unless form)
  (define `cond (nth 3 form))
  (define `rest (nth-rest 4 form))

  `(if ,cond nil (begin ,@rest)))
