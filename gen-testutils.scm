;;--------------------------------
;; utilities for testing compilation
;;--------------------------------

(require "core.scm")
(require "parse.scm")
(require "gen.scm")
(require "gen0.scm" &private)

(declare SCAM_DEBUG &native)

;; Set all form positions to POS.
;;
(define (form-set-indices pos form)
  &public
  (define `(recur f)
    (form-set-indices pos f))

  (if form
      (case form
        ((PString  n v) (PString pos v))
        ((PSymbol  n v) (PSymbol pos v))
        ((PError   n v) (PError pos v))
        ((PList    n subs) (PList pos (for (f subs) (recur f))))
        ((PQuote   n sub) (PQuote pos (recur sub)))
        ((PQQuote  n sub) (PQQuote pos (recur sub)))
        ((PUnquote n sub) (PUnquote pos (recur sub)))
        ((PSplice  n sub) (PSplice pos (recur sub)))
        (else (.. "ERROR:form-set-indices(" form ")")))))


;; form-set-indices
(fexpect (PList 0 [ (PString 0 1) (PSymbol 0 2) ])
         (form-set-indices 0 (PList 9 [ (PString 8 1) (PSymbol 7 2) ])))


;; Serialize IL to a compact, readable format.  This transformation is not
;; reliably reversible, but the output should be suitable for assertions
;; when the inputs are crafted to avoid ambiguities.
;;
(define (il-ser node)
  &public
  (define `(call-ser pre args)
    (.. "(" pre " " (concat-for (a args ",") (il-ser a)) ")"))

  (case node
    ((IString value) value)
    ((IVar name) (.. "{" name "}"))
    ((IBuiltin name args) (call-ser (.. "." name) args))
    ((IFor name list body) (call-ser ".foreach" [(IString  name) list body]))
    ((ICall name args) (call-ser name args))
    ((IArg ndx ups) (.. "{" (if ups (patsubst ".%" "%" ups) "!")
                        (patsubst "=%" "%" ndx) "}"))
    ((IFuncall nodes) (call-ser "^Y" nodes))
    ((IConcat values) (concat-for (v values "") (il-ser v)))
    ((IBlock nodes) (if (word 2 nodes)
                       (call-ser "IBlock" nodes)
                       (il-ser (first nodes))))
    ((ILambda code) (.. "`" (il-ser code)))
    (else (if node
              ;; convert (") to (') to simplify assertions
              (subst "\"" "'" (sprintf "!%q" node))))))


;; Default env for c0-ser.  Use "real" names different from local names to
;; make sure they don't get confused.
;;
(define default-env
  &public
  (append
   (depth-marker ".")
   { a: (EDefn.arg 1 "."),
     v: (EVar "p" "V"),
     f: (EFunc "p" "F" 2),
     ;; names that an extra promote/demote will corrupt...
     "f!0!": (EFunc "p" "F!0!" 2),
     "d!0!": (EVar "p" "D!0!") }))


;; Compile one or more forms to serialized IL.
(define (c0-ser text ?env)
  &public
  (il-ser (c0-block (parse-text text) (or env default-env))))


;; Parse a vector of forms from text, normalizing positions to 0.
;;
(define (pN text)
  &public
  (for (f (parse-text text))
    (form-set-indices 0 f)))

;; Parse *one* form from text.
;;
(define (p1 text)
  &public
  (let ((o (parse-text text)))
    (if (word 2 o)
        (.. "EXTRA NODES: " o))
    (first o)))

;; Compile one or more expressions a a block, calling k with `sil` and `env`
;; values, where `env` is the resulting environment, and `sil` is the
;; serialized IL node.
;;
(define (p1-block-cc text k)
  &public
  (c0-block-cc nil (parse-text text)
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
  (subst "~" (gen-native-name "" nil) str))
