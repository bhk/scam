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
;; Function syntax consists of text that, when expanded in Make, will expand
;; to the *value* described by the corresponding IL node.  For example, the
;; IL node (IString "$") compiles to "$`", which, when evaluated by Make --
;; as in, say "$(info $`)" -- will yield "$".  So in a sense there is an
;; "extra" level of escaping in all function syntax, and no literal "$"
;; will appear un-escaped.  When "$" is escaped, we use "$`", ensuring that
;; each "$" will be followed only by a restricted set of characters.  We
;; assign special meaning to a couple of character sequences that do not
;; appear in actual "code":
;;
;;    "$." is a marker.
;;    "$-" is a negative-escape sequence.
;;
;; Lambda-escaping handles these specially.  Whereas every "$" usually
;; escapes to "$`", "$." remains "$." and "$-" becomes "$".
;;
;; Lambda Values, Captures, and Lambda-escaping
;; ----------------
;;
;; Code that lies within an anonymous function will be "lambda-escaped" for
;; inclusion in the body of a parent function.  Consider this example:
;;
;;   (define f (lambda () (lambda () (lambda (a) (IConcat "$" a)))))
;;   f  -->  "$```$``1"
;;   (((f)) "A")   -->  "$A"
;;
;; Here, the code generated for the innermost lambda, "$`$1", was escaped
;; twice to yield the value of `f`, because it had to survive two rounds of
;; expansion before being executed.
;;
;; Now consider this:
;;
;;   (define f (lambda (c) (lambda (b) (lambda (a) (concat "$" a b c)))))
;;   (((f "C") "B") "A")  -->   "$ABC"
;;
;; The innermost lambda's IL node looks like this:
;;
;;   (ILambda (IConcat (IString "$") (ILocal 1 0) (ILocal 1 1) (ILocal 1 2)))
;;
;; The `IConcat` node compiles to:
;;
;;   "$`$1$-(call ^E,$-1)$--(call ^E,$--1,`)"
;;
;; When evaluated in Make, "$`" expands to "$", and "$1" expands the
;; innermost local variable value.  The other local variables are
;; *captures*.  Their numeric variables (e..g "$1") are not accessible when
;; the innermost lambda executes ... instead, they must have been expanded
;; earlier when the corresponding ancestor executed.  But the ancestor's
;; code cannot be generated until after the nested lambda has been compiled.
;; For this, we have a notion of "negative" escaping.  We can use "$-",
;; which after a round of escaping, yields a "bare", un-escaped "$".
;;
;; The sequence "$-(call ^E,$-1)" will escape to "$(call ^E,$1)", so when
;; the parent function executes its "$1" argument will be embedded in the
;; lambda expression.  `^E` escapes the value at run-time, since it must
;; survive a round of expansion (when the lambda expression is evaluated).
;; For 'c' -- (ILocal 1 2) -- an extra "`" argument is passed to `^E` to
;; indicate that the run-time expansion must survive an additional round of
;; expansion, since the value will be captured in a lambda expression that
;; is two levels down.
;;
;; The `IConcat` code is then expanded to produce the value of the innermost
;; Lambda -- which constitutes the the *body* of the middle Lambda:
;;
;;   "$``$`1$(call ^E,$1)$-(call ^E,$-1,`))"
;;
;; This is then escaped to obtain the body of the outer Lambda:
;;
;;   "$```$``1$`(call ^E,$`1)$(call ^E,$1,`))"
;;
;; Now we have the *value* of `f`, although compiling the outer Lambda
;; proceeds to escape this once more, producing the compiled form of the
;; body of `f` (code = escaped value).
;;
;;                     f  -->  "$```$``1$`(call ^E,$`1)$(call ^E,$1,`))"
;;               (f "C")  -->  "$``$`1$(call ^E,$1)C"
;;         ((f "C") "B")  -->  ("$``$`1$(call ^E,$1)C" "B")
;;                        -->  "$`$1BC"
;;   (((f "C") "B") "A")  -->  ("$`$1BC" "A")
;;                        -->  "$ABC"


;; "FILE" or "FILE:LINE" as given by POS and current file.
;;
(define (c1-Where pos)
  (define `lnum
    (get-subject-line pos *compile-subject*))
  (escape (concat *compile-file* (if pos (concat ":" lnum)))))


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
  (concat "$.{" (crumb-encode {=key: value}) "$.}"))


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
  (concat "a b c d e f g h i j k l m n o p q r s t u v w x y z "
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
    ((IFuncall _)   1)
    ((ILocal _ _)   1)))


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
              (call quotefn a)))


(define (c1-Error node)
  (crumb "errors"
         (case node
           ((PError pos msg) node)
           (else (PError 0 (concat "internal:bad IL: " node))))))


;; Construct an IL node that evaluates to a vector.  `nodes` is a vector of
;; IL nodes containing the item values.
;;
(define (il-vector nodes)
  (il-concat (subst " " (concat " " [(IString " ")] " ")
                    (for n nodes (il-demote n)))))


;; Call built-in function
(define (c1-Builtin name args)
  ;; (demote <builtin>) == <builtin> for all builtins
  (concat "$(" name
          " " ; this space is necessary even when there are no arguments
          (protect-ltrim (c1-vec args ","
                              (if (filter "and or" name)
                                  (native-name c1-arg-trim)
                                  (native-name c1-arg))))
          ")"))

;; Compile an array of arguments (IL nodes) into at most 9 positional arguments
;;
(define (c1-args9 nodes)
  (if (word 9 nodes)
      (concat (c1-vec (wordlist 1 8 nodes) "," (native-name c1-arg))
              (concat "," (protect-arg (c1 (il-vector (nth-rest 9 nodes))))))
      (c1-vec nodes "," (native-name c1-arg))))


;; Call user-defined function (by name)
;;
(define (c1-Call name args)
  (define `ename (protect-ltrim (escape name)))

  (concat "$(call " ename (if args ",") (c1-args9 args) ")"))


;; Repeat WORDS (B - A + 1) times.
;; Note: A and B must be >= 1.
;;
(define (make-list a b words)
  (if (word b words)
      (subst " " "" (wordlist a b words))
      (make-list a b (concat words " " words " " words))))


;; Local variable  (level 0 = current)
;;
(define (c1-Local ndx level)
  (if (filter-out 0 level)
      ;; ups is non-zero, non-nil
      (subst "-" (make-list 1 level "-")
             ",)" ")"
             (concat "$-(call ^E,$-" ndx "," (make-list 2 level "`") ")"))
      (concat "$" ndx)))


;; Call lambda value
(define (c1-Funcall nodes)
  (define `func (first nodes))
  (define `args (rest nodes))
  (define `fnval (protect-arg (c1 func)))
  (define `commas
    (subst " " "" (or (wordlist (words (concat "x" args)) 9
                                ", , , , , , , , ,")
                      ",")))

  (concat "$(call ^Y," (c1-args9 args) commas fnval ")"))


;; Block: evaluate all nodes and return value of last node
(define (c1-Block nodes)
  (if (word 2 nodes)
      (concat "$(and " (c1-vec nodes "1," (native-name c1-arg)) ")")
      (if nodes
          (c1 (first nodes)))))


(define (c1-Var name)
  (concat "$" (or (filter one-char-names name)
                  (concat "(" (escape name) ")"))))

(define (c1 node)
  (case node
    ((IString value) (escape value))
    ((ILocal ndx ups) (c1-Local ndx ups))
    ((ICall name args) (c1-Call name args))
    ((IVar name) (c1-Var name))
    ((IConcat nodes) (c1-vec nodes "" (native-name c1)))
    ((ILambda code) (c1-Lambda (c1 code)))
    ((IBlock nodes) (c1-Block nodes))
    ((IFuncall nodes) (c1-Funcall nodes))
    ((IBuiltin name args) (c1-Builtin name args))
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
  (concat (protect-expr expr) "\n"))


;; Construct a file-syntax assignment for a simple variable
;;
;; After "LHS := RHS", $(LHS) or $(value LHS) == RHS.
;;
(define `(c1-file-set lhs rhs)
  (concat (protect-lhs lhs) " := " (protect-rhs rhs) "\n"))


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
       (concat "$(call ^fset," (protect-arg lhs) "," (protect-arg rhs) ")"))
      ;; RHS is const
      (if (or (findstring "#" rhs)
              (findstring "\n" rhs)
              ;; leading whitespace?
              (filter "~%" (subst "\t" "~" " " "~" rhs)))

          ;; Use 'define ... endef' so that $(value F) will be *identical*
          ;; to RHS almost always.
          (concat "define " (protect-lhs lhs) "\n"
                  (protect-define (unescape rhs))
                  "\nendef\n")
          (concat (protect-lhs lhs) " = " (unescape (protect-rhs rhs)) "\n"))))


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
             (concat value "\n")
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
