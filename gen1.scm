;;--------------------------------------------------------------
;; gen1: compiler back-end
;;--------------------------------------------------------------

(require "core")
(require "parse")
(require "escape")
(require "gen")
(require "gen0")


;; Repeat a word k times.
;;   Note: k must be >= 1.
;;         w must contain no whitespace.
(define (rep-word k w)
  (if (word k w)
      (subst " " "" (wordlist 1 k w))
      (rep-word k (concat w " " w " " w))))


;;--------------------------------------------------------------
;; "gen" coding
;;
;; To embed up-value references and error messages in generated Make
;; expressions, we use special character sequences that do not appear
;; otherwise.
;;
;;  * Upvalue from parent:   ($.^=1)  ($.^=2)  ...
;;  * Grandparent upvalue:   ($.^^=1,2) ($.^^=2,2) ...
;;  * Errors:                ($.@ERROR@)
;;  * Temporary usage:       ($)
;;

;; Escape a literal string for a Make expression
(define (gen-escape-literal literal)
  (subst "$" "$$" literal))


;; Escape a lambda value for inclusion in a Make expression
(define (gen-escape-lambda code)
  (subst "$" "$$"
         "($$." "($."
         "($.^" "($."
         "($.=" "$(call ^e,$"
         code))


;; Embed an arbitrary string into a Make expression.  This will survive any
;; `gen-quote` or `protect-...` operations intact; `gen-extract` should
;; recover the exact same string from the final c1 result.
;;
(define (gen-embed str)
  (concat "($.@"
          (subst "~" "~1" "(" "~L" "," "~C" ")" "~R" "$" "~S" str)
          "@)"))


;; Extract embedded strings.  Returns a vector.
;;
(define (gen-extract code)
  (foreach w (rest (split "($.@" code))
           (subst "~L" "(" "~C" "," "~R" ")" "~S" "$" "~1" "~"
                  (first (split "@)" w)))))

;;--------------------------------------------------------------

;; returns demoted name of builtin or user function (if builtin = call)
;; or empty string if 'node' is not a function invocation
(define (il-funcname node)
  (if (type? "f" node)
      (word 2 node)
      (if (type? "F" node)
          (or (filter-out "call" (word 2 node))
              (word 2 (nth 3 node))))))

;; Return arguments to a user function
;; Could be  [F call FNSYM var val ret]
;;       or  [f FNNAME var val ret]
(define `(il-user-args node)
  (nth-rest (if (type? "F" node) 4 3) node))


;; Detect whether an IL node will always evaluate to "".
;;    node = IL node
;;    name = (il-funcname node)
;;
(define `(void-node? node name)
  ;; void-names are builtins and runtime functions that we know always
  ;; return nil
  (define `void-names
    "error eval info ^require")

  (filter void-names name))


;; return new IL node that discards return value of e, if any
(define `(voidify e)
  (if (void-node? e (il-funcname e))
      e
      ["F" "if" e ["Q" ""]]))

;;--------------------------------

(define one-char-names
  (concat "a b c d e f g h i j k l m n o p q r s t u v w x y z "
          "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z _"))

(declare (c1))


(define (c1-arg node)
  (if (type? "f F" node)
      ;; c1-f and c1-F generate balanced expressions
      (c1 node)
      (protect-arg (c1 node))))


;; protect an expression from having leading/trailing whitespace trimmed (as
;; in `and` and `or` expressions)
;;
(define (c1-arg-trim node)
  (if (type? "f F" node)
      ;; c1-f and c1-F generate balanced expressions that don't trim
      (c1 node)
      (protect-trim (protect-arg (c1 node)))))


;; c1-vec: compile multiple expressions
(define (c1-vec args delim quotefn)
  (concat-for a args delim
              (call quotefn a)))


(define (c1-E node)
  (gen-embed node))


;; Construct an IL node that evaluates to a vector.  `nodes` is a vector of
;; IL nodes containing the item values.
;;
(define (il-vector nodes)
  (il-foldcat (il-qmerge (subst " " (concat " " [["Q" " "]] " ")
                                (for n nodes (il-demote n))))))


;; Call built-in function:   ["F" <name> <a> <b>]
(define (c1-F node)
  ;; (demote <builtin>) == <builtin> for all builtins
  (define `name (word 2 node))
  (concat "$(" name
          " " ; this space is necessary even when there are no arguments
          (protect-ltrim (c1-vec (rrest node) ","
                              (if (filter "and or" name)
                                  (global-name c1-arg-trim)
                                  (global-name c1-arg))))
          ")"))

;; Compile an array of arguments (IL nodes) into at most 9 positional arguments
;;
(define (c1-args9 nodes)
  (if (word 9 nodes)
      (concat (c1-vec (wordlist 1 8 nodes) "," (global-name c1-arg))
              (concat "," (protect-arg (c1 (il-vector (nth-rest 9 nodes))))))
      (c1-vec nodes "," (global-name c1-arg))))


;; Call user-defined function (by name):   ["f" <name> <a> <b>]
(define (c1-f node)
  (define `ename (protect-ltrim (gen-escape-literal (nth 2 node))))
  (define `args (c1-args9 (rrest node)))

  (concat "$(call " ename (if (rrest node) ",") args ")"))


(define (c1-U node)
  (define `n (word 2 node))
  (define `ups (word 3 node))
  (define `carets (rep-word ups "^"))

  (if (filter-out 0 ups)
      (concat "($." carets "=" n (filter-out ",1" (concat "," ups)) ")")
      (concat "$" n)))


;; Call lambda value:  ["Y" <fn> <a> <b> ... ]
(define (c1-Y node)
  (define `args (c1-args9 (rrest node)))
  (define `fnval (protect-arg (c1 (nth 2 node))))
  (define `commas (subst " " "" (filter "," (or (wordlist (words node) 11
                                                          "x x , , , , , , , , ,")
                                                ","))))

  (concat "$(call ^Y," args commas fnval ")"))


;; block: ["B" <exp> ...]
;; return value of last expression
(define (c1-B node)
  (if (word 3 node)
      (concat "$(and " (c1-vec (rest node) "1," (global-name c1-arg)) ")")
      (if (word 2 node)
          (c1 (nth 2 node)))))


(define (c1-V node)
  (concat "$" (or (filter one-char-names (word 2 node))
                  (concat "(" (gen-escape-literal (nth 2 node)) ")"))))

(define (c1-X node)
  (gen-escape-lambda (c1 (nth 2 node))))

(define `(c1-Q node)
  (subst "$" "$$" (nth 2 node)))

(define (c1 node)
  (cond ((type? "Q%" node) (c1-Q node))
        ((type? "U" node) (c1-U node))
        ((type? "f" node) (c1-f node))
        ((type? "V" node) (c1-V node))
        ((type? "C" node) (c1-vec (rest node) "" (global-name c1)))
        ((type? "X" node) (c1-X node))
        ((type? "B" node) (c1-B node))
        ((type? "Y" node) (c1-Y node))
        ((type? "F" node) (c1-F node))
        (else             (c1-E node))))


;;--------------------------------------------------------------
;; c1-file : compile to "statement" or "file" syntactic context.
;; (See "File vs. Function Context" in compile.scm.)

(declare (c1-file))


;; construct code for simple assignment
;;
;; After "LHS := RHS", $(LHS) or $(value LHS) == RHS.
;;
(define (c1-file-set lhs rhs)
  (concat (protect-lhs lhs) " := " (protect-rhs rhs) "\n"))


;; construct code for recursive assignment
;;
;; After "LHS = RHS", $(value LHS) == RHS
;;
(define (c1-file-fset lhs rhs)
  (if (findstring "$" (subst "$$" "" rhs))
      ;; rhs not constant
      (concat "$(call " "^fset" "," (protect-arg lhs) "," (protect-arg rhs) ")\n")
      (if (or (findstring "#" rhs)
              (findstring "\n" rhs)
              ;; leading whitespace?
              (filter "~%" (subst "\t" "~" " " "~" rhs)))

          ;; Use 'define ... endef' so that $(value F) will be *identical*
          ;; to rhs almost always.
          (concat "define " (protect-lhs lhs) "\n"
                  (protect-define (subst "$$" "$" rhs))
                  "\nendef\n")
          (concat (protect-lhs lhs) " = " (subst "$$" "$" (protect-rhs rhs)) "\n"))))


;; compile a vector of expressions for file context
;;
(define (c1-file* nodes)
  (concat-vec
   (for node nodes
        (c1-file node))))


;; compile one expression for file context
;;
(define (c1-file node)
  (or
   ;; top-level (eval STR) equivalent to STR
   (and (type? "F" node)
        (filter "eval" (word 2 node))
        (filter "Q%" (word 3 node))
        (concat (string-value (nth 3 node)) "\n"))

   ;; use makefile syntax for assignments, versus "$(call SET,...)
   (if (filter (concat "^set" " " "^fset") (il-funcname node))
          (let ((args (il-user-args node))
                (name (il-funcname node)))
            (if (not (nth 3 args))
                ( (if (filter "^set" name) c1-file-set c1-file-fset)
                  (c1 (nth 1 args))
                  (c1 (nth 2 args))))))

   (cond
    ((type? "B" node) (c1-file* (rest node)))

    (else (concat (protect-expr (c1 (voidify node))) "\n")))))


;; Compile a vector of IL nodes to an executable string.
;; Returns:  [ errors exe ]
;;
(define (gen1 nodes is-file)
  (let ( (c1o (if is-file
                  (c1-file* nodes)
                  (c1 (append "B" nodes)))) )
    [ (gen-extract c1o) c1o ]))
