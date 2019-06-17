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
;;     Function Code:   $(call ^set,x,1)      $(call +,1,2)
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
;; Function code will be *expanded*, so any literals including "$" are
;; replaced with "$`" so that after one round of expansion they evaulate
;; to "$".  (The SCAM runtime defines the variable "`" as "$".)
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
;;   -->  $``$`1$(call ^E,$1)
;;
;; Note that the value of `x` results in the code `$(call ^E,$1)`.  The steps
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


;; "FILE" or "FILE:LINE" as given by POS and current file.
;;
(define (c1-Where pos)
  (define `lnum
    (get-subject-line pos *compile-subject*))
  (escape (.. *compile-file* (if pos (.. ":" lnum)))))


;; Lambda-escape CODE
;;
(define (c1-Lambda code)
  (subst "$" "$`"
         "$`-" "$"
         "$`." "$."
         code))


;; Crumbs are name/value pairs can be placed anywhere in generated code.
;; They will survive lambda-escaping, protect-XXX, and other transformations
;; performed during code generation.  Before `gen1` returns, all crumbs are
;; removed from the generated code and collated into name/vector pairs.

(define `(crumb-encode str)
  (subst "~" "~1" "(" "~L" "," "~C" ")" "~R" "$" "~S" "\n" "~N" str))

(define `(crumb-decode str)
  (subst "~N" "\n" "~S" "$" "~R" ")" "~C" "," "~L" "(" "~1" "~" str))


;; Construct a crumb.
;;
(define (crumb key value)
  (.. "$.{" (crumb-encode {=key: value}) "$.}"))


;; Extract crumbs.  Returns { code: CODE, errors: ERRORS }.
;;
(define `(crumb-extract code)
  (let ((dc (subst "$.{" " $.{" "$.}" " " [code])))
    (append {code: (concat-vec (filter-out "$.{%" dc))}
            (dict-collate (foreach w (filtersub "$.{%" "%" dc)
                                   (crumb-decode (promote w)))))))


;; Construct a node that expands NODE but returns nil.
;;
(define `(voidify node)
  (define `void-fns
    "error eval info ^R ^at")
  (if (case node
        ((IBuiltin name args) (filter void-fns name))
        ((ICall name args) (filter void-fns name))
        ((ICrumb _ _) 1))
      node
      (IBuiltin "if" [node (IString "")])))


(define `one-char-names
  (._. "a b c d e f g h i j k l m n o p q r s t u v w x y z"
       "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z _"))


(declare (c1 node))


;; These nodes generate code with balanced parens and without leading or
;; trailing whitespace.  This saves a trip through `protect-arg`.
;;
(define `(is-balanced? node)
  (case node
    ((ICall _ _)    1)
    ((IVar _)       1)
    ((IBuiltin _ _) 1)
    ((IFor _ _ _)   1)
    ((IFuncall _)   1)
    ((IArg _ _)   1)))


(define (c1-arg node)
  (if (is-balanced? node)
      (c1 node)
      (protect-arg (c1 node))))


;; protect an expression from having leading/trailing whitespace trimmed (as
;; in `and` and `or` expressions)
;;
(define (c1-arg-trim node)
  (if (is-balanced? node)
      (c1 node)
      (protect-trim (protect-arg (c1 node)))))


;; c1-vec: compile multiple expressions
(define (c1-vec args delim quotefn)
  (concat-for a args delim
              (native-call quotefn a)))


(define (c1-Error node)
  (crumb "errors"
         (case node
           ((PError pos msg) node)
           (else (PError 0 (.. "internal:bad IL: " node))))))


;; Call built-in function
(define (c1-Builtin name args)
  ;; (demote <builtin>) == <builtin> for all builtins
  (.. "$("
      (if (filter-out "=" name)
          ;; this space is necessary even when there are no arguments
          (.. name " "))
      (protect-ltrim (c1-vec args ","
                             (if (filter "and or" name)
                                 (native-name c1-arg-trim)
                                 (native-name c1-arg))))
      ")"))


;; Compile an array of arguments (IL nodes) into at most 9 positional arguments
;;
(define (c1-args9 nodes)
  (if (word 9 nodes)
      (.. (c1-vec (wordlist 1 8 nodes) "," (native-name c1-arg))
          "," (protect-arg (c1 (il-vector (nth-rest 9 nodes)))))
      (c1-vec nodes "," (native-name c1-arg))))


;; Call user-defined function (by name)
;;
(define (c1-Call name args)
  (define `ename (protect-ltrim (escape name)))

  (.. "$(call " ename (if args ",") (c1-args9 args) ")"))


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


;; Call lambda value
(define (c1-Funcall nodes)
  (define `func (first nodes))
  (define `args (rest nodes))
  (define `fnval (protect-arg (c1 func)))
  (define `commas
    (subst " " "" (or (wordlist (words (.. "x" args)) 9
                                ", , , , , , , , ,")
                      ",")))

  (.. "$(call ^Y," (c1-args9 args) commas fnval ")"))


;; Block: evaluate all nodes and return value of last node
(define (c1-Block nodes)
  (if (word 2 nodes)
      (.. "$(and " (c1-vec nodes "1," (native-name c1-arg)) ")")
      (if nodes
          (c1 (first nodes)))))


(define (c1-Var name)
  (.. "$" (or (filter one-char-names name)
              (.. "(" (escape name) ")"))))


(define (c1 node)
  (case node
    ((IString value) (escape value))
    ((IArg ndx ups) (c1-IArg ndx ups))
    ((ICall name args) (c1-Call name args))
    ((IVar name) (c1-Var name))
    ((IConcat nodes) (c1-vec nodes "" (native-name c1)))
    ((ILambda code) (c1-Lambda (c1 code)))
    ((IBlock nodes) (c1-Block nodes))
    ((IFuncall nodes) (c1-Funcall nodes))
    ((IBuiltin name args) (c1-Builtin name args))
    ((IFor name list body) (c1-Builtin "foreach" [(IString name) list body]))
    ((IWhere pos) (c1-Where pos))
    ((ICrumb key value) (crumb key value))
    ((IEnv _ node) (c1 node))
    (else
     (if node
         (c1-Error node)))))


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
(define `(c1-file-expr expr)
  (.. (protect-expr expr) "\n"))


;; Construct a file-syntax assignment for a simple variable
;;
;; After "LHS := RHS", $(LHS) or $(value LHS) == RHS.
;;
(define `(c1-file-set lhs rhs)
  (.. (protect-lhs lhs) " := " (protect-rhs rhs) "\n"))


;; Construct a file-syntax assignment for a recursive variable
;;
;; After "LHS = RHS", $(value LHS) == RHS
;;
(define (c1-file-fset lhs rhs)
  (define `(unescape str)
    (subst "$`" "$" str))

  (if (or (findstring "$" (subst "$`" "" rhs))
          (findstring "$`." rhs))
      ;; RHS is non-const (has un-escaped "$"), or would contain "$."
      (c1-file-expr
       (.. "$(call ^fset," (protect-arg lhs) "," (protect-arg rhs) ")"))
      ;; RHS is const
      (if (or (findstring "#" rhs)
              (findstring "\n" rhs)
              ;; leading whitespace?
              (filter "~%" (subst "\t" "~" " " "~" rhs)))

          ;; Use 'define ... endef' so that $(value F) will be *identical*
          ;; to RHS almost always.
          (.. "define " (protect-lhs lhs) "\n"
              (protect-define (unescape rhs))
              "\nendef\n")
          (.. (protect-lhs lhs) " = " (unescape (protect-rhs rhs)) "\n"))))


;; Compile a vector of expressions to file syntax.
;;
(define `(c1-file* nodes)
  (concat-for node nodes ""
              (c1-file node)))


;; Compile a node to file syntax.
;;
(define (c1-file node)
  (or
   (case node

     ;; Normalize (IBuiltin "call" (IString S)) to (ICall S ...).
     ;; Compile (IBuiltin "eval" (IString "...")) as "...\n"
     ((IBuiltin name args)
      (case (first args)
        ((IString value)
         (if (filter "eval" name)
             (.. value "\n")
             (if (filter "call" name)
                 (c1-file (ICall value (rest args))))))))

     ;; Handle assignments using "a = b" vs. "$(call ^fset,a,b)"
     ((ICall name args)
      (if (not (filter-out [nil] (word 3 args)))
          (if (filter "^set" name)
              (c1-file-set (c1 (nth 1 args)) (c1 (nth 2 args)))
              (if (filter "^fset" name)
                  (c1-file-fset (c1 (nth 1 args)) (c1 (nth 2 args)))))))

     ;; Compile block members as also in file scope
     ((IBlock nodes) (c1-file* nodes)))

   (c1-file-expr (c1 (voidify node)))))


;; Compile a vector of IL nodes.
;; Returns:  { code: CODE, errors: ERRORS }
;;
(define (gen1 node-vec is-file)
  &public
  (crumb-extract (if is-file
                     (c1-file* node-vec)
                     (c1 (IBlock node-vec)))))
