;;--------------------------------
;; utilities for testing compilation
;;--------------------------------

(require "core")
(require "parse")
(require "gen")
(require "gen0")


;; Serialize IL to a compact, readable format.  This transformation is not
;; reliably reversible, but the output should be suitable for assertions
;; when the inputs are crafted to avoid ambiguities.
;;
(define (il-ser node)
  &public
  (define `(call-ser pre args)
    (concat "(" pre " " (concat-for a args "," (il-ser a)) ")"))

  (case node
    ((IString value) value)
    ((IVar name) (concat "{" name "}"))
    ((IBuiltin name args) (call-ser (concat "." name) args))
    ((ICall name args) (call-ser name args))
    ((ILocal ndx level) (concat "{" ndx (filter-out "^0" (concat "^" level)) "}"))
    ((IFuncall nodes) (call-ser "^Y" nodes))
    ((IConcat values) (concat-for v values "" (il-ser v)))
    ((IBlock nodes) (if (word 2 nodes)
                       (call-ser "IBlock" nodes)
                       (il-ser (first nodes))))
    ((ILambda code) (concat "`" (il-ser code)))
    (else (if node
              ;; convert (") to (') to simplify assertions
              (subst "\"" "'" (sprintf "!%q" node))))))


;; Default env for c0-ser.  Use "real" names different from local names to
;; make sure they don't get confused.
;;
(define default-env
  &public
  { a: (EArg "1"),
    v: (EVar "V" "."),
    f: (EFunc "F" "." 2 nil),
    ;; names that an extra promote/demote will corrupt...
    "f!0!": (EFunc "F!0!" "." 2 nil),
    "d!0!": (EVar "D!0!" ".") })


;; Compile one or more forms to serialized IL.
(define (c0-ser text ?env)
  &public
  (il-ser (c0-block (parse-text text) (or env default-env))))


;; Parse a vector of forms from text, normalizing positions to 0.
;;
(define (pN text)
  &public
  (for f (parse-text text)
       (form-set-indices 0 f)))

;; Parse *one* form from text.
;;
(define (p1 text)
  &public
  (let ((o (parse-text text)))
    (if (word 2 o)
        (concat "EXTRA NODES: " o))
    (first o)))

;; Compile one or more expressions a a block, calling k with `sil` and `env`
;; values, where `env` is the resulting environment, and `sil` is the
;; serialized IL node.
;;
(define (p1-block-cc text k)
  &public
  (c0-block-cc nil (pN text)
               (lambda (env nodes)
                 (k env (il-ser (IBlock nodes))))))

;; Display and return value.
;;
(define (DUMP name val)
  &public
  (if (findstring "D" SCAM_DEBUG)
      (print name ": " (format val)))
  val)

;; Translate "~" to local namespace prefix in str.
(define (xns str)
  &public
  (subst "~" (gen-global-name "" nil) str))
