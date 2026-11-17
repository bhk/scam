;;--------------------------------------------------------------
;; gen1: compiler back-end
;;--------------------------------------------------------------

(require "core.scm")
(require "escape.scm")
(require "parse.scm")
(require "gen.scm")

;; File vs. Function Syntax
;; ----------------
;;
;; Compilation may generate code for different syntactic contexts in Make.
;; "File" code can appear as a line in a Makefile and is suitable for
;; passing to Make's `eval` builtin.  "Function" code can appear within a
;; function body, is suitable for invoking directly or binding to a function
;; variable.
;;
;;     SCAM source:     (set-native "x" 1)    (+ 1 2)
;;     Function Code:   $(call ^S,x,1)        $(call +,1,2)
;;     File Code:       x = 1                 $(if $(call +,1,2),)
;;
;; Most of the functions in this module compile to function syntax.  File
;; syntax is handled by `c1-file-xxx` functions.  A few constructs are
;; handled specially in file syntax, but in most cases the code is first
;; compiled to function syntax and then wrapped and/or transformed.
;;
;; Lambda Values, Captures, and Lambda-escaping
;; ----------------
;;
;; Function code yields a value after *expansion*, so literal occurrences of
;; "$" are represented by "$`".  (The runtime sets the variable "`" to "$".)
;;
;;   (define (f x) (.. "$" x))
;;   -->  f = $`$1
;;
;; When function code expands to an anonymous function, two levels of
;; escaping are necessary. Consider:
;;
;;   (define (f x) (lambda (y) "$")
;;   -->  f = $``
;;
;; When there is a *capture*, the runtime function `^E` is used to encode
;; that captured value as part of the anonymous function:
;;
;;   (define (f x) (lambda (y) (.. "$" y x)))
;;   -->  f = $``$`1$(call ^E,$1)
;;
;; Note that `x` (an upwards funarg) compiles to `$(call ^E,$1)`.  The steps
;; can be summarized as:
;;
;;           IL             (c1 IL)           (c1-Lambda (c1 IL)
;;   "$" = (IString "$")    $`                $``
;;    y  = (IArg 1 ".")     $1                $`1
;;    x  = (IArg 1 "..")    $-(call ^E,$-1)   $(call ^E,$1)
;;
;; Lambda-escaping performs the following:
;;
;;    "$." -> "$."  (marker)
;;    "$-" -> "$"   (negative-escape)
;;    "$"  -> "$`"  (other instances of "$")


(declare (c1 node))


;; "FILE" or "FILE:LINE" as given by POS and current file.
;;
(define (c1-Where pos)
  (define `lnum
    (get-subject-line pos *compile-subject*))
  (escape (.. *compile-file* (if pos (.. ":" lnum)))))


;; Lambda-escape CODE
;;
(define (c1-Lambda body)
  (subst "$" "$`"
         "$`-" "$"
         "$`." "$."
         (c1 body)))


;; Crumbs are name/value pairs that can be embedded in generated code,
;; allowing c1-level functions, which typically return only code, to return
;; error information and other OOB data.  Crumbs survive lambda-escaping,
;; protect-XXX, and other transformations performed during code generation.
;; Before `gen1` returns, it removes all crumbs from the code and collates
;; their names & values into {name: vector} pairs.

(define `(crumb-encode str)
  (subst "~" "~1" "(" "~L" "," "~C" ")" "~R" "$" "~S" "\n" "~N" str))

(define `(crumb-decode str)
  (subst "~N" "\n" "~S" "$" "~R" ")" "~C" "," "~L" "(" "~1" "~" str))


;; Construct a crumb.
;;
(define (crumb key value)
  (.. "$.{" (crumb-encode {=key: value}) "$.}"))


;; Split code at crumb boundaries.
;;
(define (split-crumbs code)
  (subst "$.{" " $.{" "$.}" "$.} " [code]))


(define `(get-error-crumbs code)
  (if (findstring "$.{errors!" code)
      (concat-vec (filter "$.{errors!%" (split-crumbs code)))))


;; Construct a node that expands NODE but returns nil.
;;
(define (c1-void node)
  (define `(use-if-void name args node)
    (and (filter "error eval info ^R ^at ^S ^SF" name)
         (if (filter "^S ^SF" name)
             (not (word 3 args))
             1)
         node))

  (c1 (or (case node
            ((IBuiltin name args) (use-if-void name args node))
            ((ICallX name args _) (use-if-void name args node))
            ((IString _) (IString nil))
            ((ICrumb _ _) node))
          (if node
              (IBuiltin "if" [node (IString "")])))))


;; These nodes generate code with balanced parens and without leading or
;; trailing whitespace.  This saves a trip through `protect-arg`.
;;
(define `(is-balanced? node)
  (case node
    ((ICallX _ _ _) 1)
    ((IVar _)       1)
    ((IBuiltin _ _) 1)
    ((IFor _ _ _)   1)
    ((IArg _ _)   1)))


(define (c1-arg node)
  (if (is-balanced? node)
      (c1 node)
      (protect-arg (c1 node))))


;; Protect an expression from having leading/trailing whitespace trimmed (as
;; in `and` and `or` expressions)
;;
(define (c1-arg-trim node)
  (if (is-balanced? node)
      (c1 node)
      (protect-trim (protect-arg (c1 node)))))


;; c1-vec: compile multiple expressions
;;
(define (c1-vec args delim quotefn)
  (concat-for (a args delim)
    (native-call quotefn a)))


;; Compile an array of arguments (IL nodes) into at most 9 positional arguments
;;
(define (c1-args9 nodes)
  (define `arg-nodes
    (if (word 9 nodes)
        (conj (wordlist 1 8 nodes) (il-vector (nth-rest 9 nodes)))
        nodes))
  (c1-vec arg-nodes "," (native-name c1-arg)))


(define `one-char-names
  (._. "a b c d e f g h i j k l m n o p q r s t u v w x y z"
       "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z _"
       "1 2 3 4 5 6 7 8 9 0"))


(define (c1-var name)
  ;; don't treat names with with spaces or % as one-char
  (.. "$" (or (findstring name (filter (native-strip name) one-char-names))
              (.. "(" name ")"))))


;; Fold an `nth-rest` call with a containing word/wordlist/^n call.
;; Indices for inner and outer calls must all be constant.
;;
(define (c1-xwordlist ctor name index-node list-node end-node)
  (define `(node-is-9s node)
    (filter "99999999" (case node ((IString n) n))))

  (define `(fold-indices a b)
    (words (.. (repeat-words 1 a) (repeat-words 1 b))))

  ;; validate outer call
  (case index-node
    ((IString outer-n)
     (if (or (filter-out "wordlist" name)
             (node-is-9s end-node))
         ;; validate inner expression
         (case list-node
           ((IBuiltin inner-name args)
            (and (filter "wordlist" inner-name)
                 (node-is-9s (nth 2 args))
                 (case (nth 1 args)
                   ((IString inner-n)
                    ;; construct folded expression
                    (define `args-out
                      (append [ (IString (fold-indices outer-n inner-n)) ]
                              (nth-rest (if (filter "wordlist" name) 2 3)
                                        args)))
                    (c1 (ctor name args-out)))))))))))


;; Optimize if CA1 is const.  We could be more thorough and treat more
;; uses of `$` as const, but it's probably not worth it.
;;
;; CA1 = (c1-arg (first args))
;;
(define (c1-if ca1 args)
  (cond
   ;; (if CONST A B)  -->  either A or B
   ((not (findstring "$" (subst "$`" nil ca1)))
    (.. (c1 (nth (if ca1 2 3) args))
        (get-error-crumbs (.. ca1 (c1 (nth (if ca1 3 2) args))))))

   ;; (if (not COND) A B)  -->  (if COND B A)
   ((and (filter "%,,1)" (lastword ca1))
         (filter [(IBuiltin "if" "%")] (word 1 args)))
    (case (first args)
      ((IBuiltin _ not-args)
       (c1-if (c1-arg (first not-args))
          (._. (word 1 not-args)
               (or (word 3 args)
                   [(IString nil)])
               (word 2 args))))
      (else (error "c1-if"))))

   (else
    (.. "$(if " (protect-ltrim ca1) "," (c1-args9 (rest args)) ")"))))


;; Call built-in function
(define (c1-Builtin name args)
  (define `default
     (.. "$(" name
         " " ;; this space is necessary even when there are no arguments
         (protect-ltrim (c1-vec args ","
                                (if (filter "and or" name)
                                    (native-name c1-arg-trim)
                                    (native-name c1-arg))))
         ")"))

  ;; nil when does not match (support `native-var`)
  (define `var-ref
    (if (filter "var" name)
        (c1-var (c1-arg (first args)))))

  ;; nil when does not match (optimize nested wordlist)
  (define `wordlist-fold
   (and (filter "wordlist word" name)
        (findstring "99999999" args)
        (c1-xwordlist IBuiltin name (first args) (last args)
                      (if (filter "wordlist" name) (nth 2 args)))))

  (if (filter "if" name)
      (c1-if (c1-arg (first args)) args)
      (or var-ref
          wordlist-fold
          default)))


(define (i-8 n)
  (words (nth-rest 9 (repeat-words ". . . ." n))))


(define (c1-ugly-arg ndx ups)
  ;; Return one copy of STR per dot in UPS, after subtracting SUB from UPS.
  (define `(ups-repeat str sub)
    (subst (.. "<" sub) nil
           "." str
           (.. "<" ups)))

  (define `argval
    (cond
     ((filter ndx "1 2 3 4 5 6 7 8 ;")
      (.. "$" ndx))

     ((filter ";%" ndx)
      (.. "$(" ndx ")"))

     ((findstring "+" ndx)
      ;; "rest" argument
      (if (filter ndx "1+ 2+ 3+ 4+ 5+ 6+ 7+ 8+")
          (.. "$(foreach N," (subst "+" nil ndx) ",$(^v))")
          (if (filter "9+" ndx)
              "$9"
              (.. "$(wordlist " (i-8 (subst "+" nil ndx)) ",99999999,$9)"))))
     (else
      (.. "$(call ^n," (i-8 ndx) ",$9)"))))


  (define `e-level
    (filter "%`" (.. "," (ups-repeat "`" ".."))))

  (subst "%" argval
         "$" (.. "$" (ups-repeat "-" "."))
         (if (filter "." ups)
             ;; argument of immediately enclosing function
             "%"
             ;; capture
             (.. "$(call ^E,%" e-level ")"))))


;; Local variable
;;
(define `(c1-IArg ndx ups)
  (or
   ;; make common and simple case fast
   (if (filter "." ups)
       (addprefix "$" (filter ndx "1 2 3 4 5 6 7 8 ;")))
   (c1-ugly-arg ndx ups)))


(define (c1-let stem c1body args)
  ;; "$-" => non-const lambda
  (if (findstring "$-" c1body)
      nil
      (.. "$(call " (crumb "fn" [stem c1body])
          (if args (.. "," (c1-args9 args)))
          ")")))


;; Call user-defined function (by name)
;;
(define (c1-CallX name args xv)
  (define `ename
    (protect-ltrim (escape name)))

  (define `commas
    (subst " ." ","
           (.. " " (filter "." (._. "." (join ". . . . . . . . ." args))))))

  (define `optimized
    (if (filter "^X ^n" name)
        (if (filter "^X" name)
            ;; hoist lambda to named function
            (case (first xv)
              ((ILambda stem body)
               (if stem
                   (c1-let stem (c1 body) args))))
            ;; ^n
            (if (findstring "99999999" args)
                (c1-xwordlist ICall name (first args) (nth 2 args) nil)))
        ;; $(call F,$1) -> $(F)
        ;; F must accept only one argument and must not be recursive.
        (and (filter "^d ^u ^k ^dk ^dv" name)
             (findstring args [(IArg 1 ".")])
             (.. "$(" name ")"))))

  (or optimized
      (.. "$(call " ename
          (if args (.. "," (c1-args9 args)))
          (if xv (.. commas (c1-args9 xv)))
          ")")))


;; Block: evaluate all nodes and return value of last node
(define (c1-Block nodes)
  (.. (concat-for (node (butlast nodes) nil)
        (c1-void node))
      (c1 (last nodes))))


(define (c1 node)
  (case node
    ((IString value) (escape value))
    ((IArg ndx ups) (c1-IArg ndx ups))
    ((IVar name) (c1-var (escape name)))
    ((IConcat nodes) (c1-vec nodes "" (native-name c1)))
    ((ILambda _ body) (c1-Lambda body))
    ((IBlock nodes) (c1-Block nodes))
    ((ICallX name nodes xv) (c1-CallX name nodes xv))
    ((IBuiltin name args) (c1-Builtin name args))
    ((IFor name list body) (c1-Builtin "foreach" [(IString name) list body]))
    ((IWhere pos) (c1-Where pos))
    ((ICrumb key value) (crumb key value))
    ((IEnv _ node) (c1 node))
    ((PError _ _) (crumb "errors" node))
    (else
     (if node
         (error (.. "c1: " node))))))


;;--------------------------------------------------------------
;; File Syntax
;;
;; Newlines may appear in function syntax but not in file syntax
;; assignments or expressions.  (But "define ... endef" can retain
;; them.)  We call protect-expr to encode newlines in expressions, and
;; protect-lhs and protect-rhs for assingments.

(declare (c1-file node))


;; Embed a function-syntax expression in file syntax
;;
(define (c1-file-expr expr)
  (if expr
      (.. (protect-expr expr) "\n")))


;; Construct a file-syntax assignment.
;;
;; After "LHS = RHS", $(value LHS) == RHS
;;
(define (c1-file-set lhs rhs is-simple)
  (define `(unescape str)
    (subst "$`" "$" str))

  (cond
   (is-simple
    (.. (protect-lhs lhs) " := " (protect-rhs rhs) "\n"))

   ;; If RHS is non-const (has un-escaped "$") it needs to be eval'ed.
   ;; Also, avoid unescaping a "$`." string literal.
   ((or (findstring "$" (subst "$`" nil "$." nil rhs))
        (findstring "$`." rhs))
    (c1-file-expr
     (.. "$(call ^SF," (protect-arg lhs) "," (protect-arg rhs) ")")))

   ;; RHS is const
   ((or (findstring "#" rhs)
        (findstring "\n" rhs)
        ;; leading whitespace?
        (filter "~%" (subst "\t" "~" " " "~" rhs)))
    ;; Use 'define ... endef' so that $(value F) will be *identical*
    ;; to value of RHS almost always.
    (.. "define " (protect-lhs lhs) "\n"
        (protect-define (unescape rhs))
        "\nendef\n"))

   (else
    (.. (protect-lhs lhs) " = " (unescape (protect-rhs rhs)) "\n"))))


;; Compile a vector of expressions to file syntax.
;;
(define `(c1-file* nodes)
  (concat-for (node nodes "")
    (c1-file node)))


;; Compile a node to file syntax.
;;
(define (c1-file node)
  (or
   (case node

     ;; Normalize (IBuiltin "call" (IString S)) to (ICallX S ...).
     ;; Compile (IBuiltin "eval" (IString "...")) as "...\n"
     ((IBuiltin name [arg1 ...other-args])
      (case arg1
        ((IString value)
         (if (filter "eval" name)
             (.. value "\n")
             (if (filter "call" name)
                 (c1-file (ICall value other-args)))))))

     ;; Handle assignments using "a = b" vs. "$(call ^SF,a,b)"
     ((ICallX name args _)
      (define `[var value] args)
      (if (filter "^S:2 ^SF:2" (.. name ":" (words args)))
          (c1-file-set (c1 var) (c1 value) (filter "^S" name))))

     ;; Compile block members as also in file scope
     ((IBlock nodes) (c1-file* nodes)))

   (c1-file-expr (c1-void node))))


;; NAME = function name
;; BODY = function body (as a *literal*, not code that evals to the body)
;;
(define (c1-set-fn name body is-file)
  (if is-file
      (.. (protect-lhs name) " = " (protect-rhs body) "\n")
      (.. "$(call ^SF," (protect-arg name) "," (protect-arg (escape body)) ")")))


;; CHUNKS = [code] split at crumb boundaries
;; HOIST-MAP = {body: name, ...}
;; CODE = vector-encoded code that has been processed
;; CRUMBS = map of body -> name for hoisted chunks
;;
(define (decrumb-loop chunks hoist-map code crumbs is-file)
  (define `w (word 1 chunks))
  (define `(recur code-out crumbs-out)
    (decrumb-loop (rest chunks) hoist-map code-out crumbs-out is-file))

  ;; Handle a hoisted function crumb
  (define `hoist-recur
    ;; dictionary pairs are single words
    (foreach (binding (crumb-decode (promote (patsubst "$.{%$.}" "%" w))))
      (define `{=key: [stem body]}
        binding)
      (define `conflict
        (first (filter (.. [stem] "'%") (dict-values hoist-map))))
      (define `hoist-name
        (.. (or conflict stem) "'"))

      (if (eq? key "fn")
          ;; fn crumb: replace with name
          (or (foreach (name (dict-get body hoist-map))
                (recur (.. code [name]) crumbs))
              (foreach (name hoist-name)
                ;; Construct function definition, and run it through this
                ;; loop because it might contain crumbs.  Since it must
                ;; appear at the start of the code, we run all of CODE back
                ;; through the loop, too (but it should be one word).
                (define `new-chunks
                  (.. (split-crumbs (c1-set-fn name body is-file))
                      code [name] " " (rest chunks)))
                (define `new-map
                  (._. {=body: name} hoist-map))
                (decrumb-loop new-chunks new-map nil crumbs is-file)))
          ;; other crumb: extract and add to dict
          (recur code (._. crumbs binding)))))

  (if chunks
      (if (filter "$.{%" w)
          hoist-recur
          (recur (.. code w) crumbs))
      (append {code: (concat-vec code)}
              (dict-collate crumbs))))


;; Extract crumbs from CODE, replacing "fn" crumbs with the hoisted function
;; name.  Collate all other crumb name/values.
;;
;; Return {code: RESULTCODE, <NAME>: [<VALUE> ...], ... }
;;
(define `(decrumb code is-file)
  (decrumb-loop (split-crumbs code) nil nil nil is-file))


;; Compile a vector of IL nodes.
;; Returns:  { code: CODE, errors: VEC, env: ENV, require: MODS}
;;
(define (gen1 node-vec is-file)
  &public
  (decrumb (if is-file
               (c1-file* node-vec)
               (c1 (IBlock node-vec)))
           is-file))
