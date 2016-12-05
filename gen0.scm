;;--------------------------------------------------------------
;; gen0 : compiler front end  (see gen.scm)
;;--------------------------------------------------------------
(require "core")
(require "parse")
(require "escape")
(require "gen")

(declare (c0 form env))
(declare (c0-block forms env))


;; Return number of "escapes" in unary form:
;;
;; $        -> ""
;; $$       -> "1"
;; $$$$     -> "1 1"
;; $$$$$$$$ -> "1 1 1"
;;
(define (c0-depth level)
  (if (findstring "$$" level)
      (concat "1 " (c0-depth (subst "$$" "$" level)))))


;; c0-local: Construct a local variable reference
;;
;;  arg = "$n" for an argument passed to the outermost function,
;;        "$$n" for an argument passed to the first nested function,
;;        "$$$$n" for an argument to a doubly-nested function, and so on.
;;  level = where the local variable is being referenced.
;;        "$" for top-level, "$$" for nested lambda, "$$$$" ...
;;
;; Returns:
;;   ["U" n ups]    n = arg number,  ups = 0 for local, 1 for parent, ...
;;
(define (c0-local arg level)
  (define `arg-level (subst " " "" (filter "$" (subst "$" "$ " arg))))
  ["U" (subst "$" "" arg) (words (c0-depth (subst arg-level "$" level))) ])


(if (findstring "U" SCAM_DEBUG)
    ;; Print warning for each upvalue [check SCAM_DEBUG again during
    ;; compilation so unit tests can suppress these warnings]
    (begin
      (declare (c0-local-unchecked))
      (set c0-local-unchecked c0-local)
      (define (c0-local arg level form)
        (if (not (findstring level arg))
            (compile-warn form "reference to upvalue '%s'" (symbol-name form)))
        (c0-local-unchecked arg level))))


;; S: symbol
;;    defn = definition for symbol (from environment)
(define (c0-S form env defn)
  &private
  (cond
   ;; (nth 2 ...) == (word 2 ...) for "A" cases:
   ((type? "A" defn)  (c0-local (word 2 defn) (word 2 (hash-get "$" env)) form))
   ((type? "V" defn)  (wordlist 1 2 defn))
   ((type? "F" defn)  (if (filter "#" (word 2 defn))
                                    (gen-error form "attempt to obtain value of compound macro")
                                    ["F" "value" ["Q" (nth 2 defn)]]))
   ((type? "M" defn)  (c0 (nth 2 defn) (env-rewind env (nth 2 form))))
   ((type? "I" defn)  (nth 2 defn))
   (else (gen-error form (if (type? "B" defn)
                             "attempt to obtain value of builtin: %q"
                             (if defn
                                 (concat "internal error: bad defn '" [defn]"'")
                                 "undefined variable: %q"))
                    (symbol-name form)))))


(define (c0-vec forms env)
  (for f forms (c0 f env)))


;; args = extra arguments (in addition to (rrest form)) if any
(define (il-F form env fn args)
  &private &inline
  (append ["F" fn] args (c0-vec (rrest form) env)))

(define (il-builtin form env name)
  &private
  ;; validate number of arguments
  (if (filter (builtin-argc name) (filter-out 0 (words (rrest form))))
      (il-F form env name nil)
      (gen-error (nth 2 form)
                "wrong number of arguments: %q accepts %s"
                name (builtin-argc name))))


;; Expand inlined function
;;
;; 1. Compile each argument expression to IL.
;; 2. Expand function definition in environment that matches its original
;;    environment PLUS bindings for the formal arguments (bound to an "I"
;;    value).
;;
(define (c0-inline-fn form env defn)
  (define `args (rrest form))
  (define `finl (nth 4 defn))
  (define `formal-args (first finl))
  (define `fbody (rest finl))
  (define `orig-env (hash-bind "#pos" (concat "P " (form-index form))
                               (env-rewind env (symbol-name (nth 2 form)) defn)))
  (define `(zip vec1 vec2)
    (join (addsuffix "!=" vec1) vec2))

  (define `argbindings
    (zip formal-args (for a args ["I" (c0 a env)])))

  (or (check-argc (words formal-args) form)
      (c0-block fbody (append argbindings orig-env))))


;; Special forms are implemented in functions that begin with "ml.special-".
;;
(define `(special-form-func name)
  (local-to-global (concat "ml.special-" name)))


;; L: compound forms
;;    defn = Result of (resolve op), which is either:
;;              a) "-" if op is not a symbol
;;              b) nil if op is an unbound symbol)
;;              b) an env record if op is a defined symbol
;;    inblock = if true, return: ["env" new-env <node>]
;;              if nil, return:  <node>
;;

(define (c0-L form env defn inblock)
  &private
  (define `op (nth 2 form))
  (define `opname (symbol-name op))
  (cond
   ;; global function variable
   ;;(il-F form env "call" [["Q" (nth 2 defn)]])
   ((type? "F" defn)
    (if (filter-out "!." (word 4 defn))
        (c0-inline-fn form env defn)
        (append "f" (word 2 defn) (c0-vec (rrest form) env))))

   ;; builtin
   ((type? "B" defn)  (il-builtin form env (nth 2 defn) nil))

   ;; empty parens
   ((not (word 2 form)) (gen-error form "missing function/macro name"))

   ((type? "X" defn)  (if (nth 3 defn)
                          (c0 (call (nth 2 defn) form) env inblock)
                          (gen-error form "cannot use xmacro in its own file")))

   ;; general case (non-symbol, argument, data var, etc.)
   (defn (concat "Y " (c0-vec (rest form) env)))

   ;; macro/special form?
   ((bound? (special-form-func opname))
    (call (special-form-func opname) form env inblock))

   ;; none of the above
   (else (gen-error op "undefined symbol: %q" opname))))


;; Collect "define flags" -- &private or &inline -- in one list.
;; Return [flags body...]
;;
(define (collect-flags forms flags)
  (if (and (type? "S%" forms)
           (filter "&private &inline &global" (symbol-name (first forms))))
      (collect-flags (rest forms) (append flags (word 1 forms)))
      (cons flags forms)))


;; Returns demoted symbol if one is found
;;
(define (find-flag name flags)
  &private &inline
  (firstword (filter (concat "%" name) flags)))


;; Env-modifying functions can return an environment when `inblock` is true.
;; Use this macro to generate the return value.
;;
(define `(block-result inblock env node)
  &private
  (append (if inblock ["env" env]) (or node "Q")))


;; Symbol name restrictions:
;;   `:` conflicts with Make's `$(name:a=b)` syntax.
;;   `$` conflicts with Make's `$(call name,...)` syntax.
;;   `%` would complicate/slow down SCAM's environment lookup.
;;
(define (check-name form)
  (foreach
   sym (or (findstring ":" form)
           (findstring "$" form)
           (findstring "%" form))
   (gen-error form "names may not contain '%s' characters" sym)))


;; Construct vector of [a b c], omitting trailing empty items.
;;
(define (trimvec a b c)
  &private
  (concat [a]
          (if (or b c)
              (concat " " [b]
                      (if c
                          (concat " " [c]))))))

;; Add a binding for symbol `sym` to `env`.  `type` = "F", "V", ...
(define (bind-sym env symbol type flags fdef)
  (define `is-global (find-flag "&global" flags))
  (define `priv (if (find-flag "&private" flags) "p"))
  (define `sym-name (symbol-name symbol))
  (define `name (if is-global
                    sym-name
                    (gen-global-name sym-name)))

  (concat (hash-bind sym-name
                     (concat type " " (trimvec name priv fdef)))
          (if env (concat " " env))))


;; (declare WHAT FLAGS...)
;; (declare (FNAME FARGS...) FLAGS...)
;; (define WHAT FLAGS... VALUE)
;; (define (FNAME FARGS...) FLAGS... BODY...)
;; (define `(MNAME MARGS...) FLAGS... BODY...)
;; (define `MSYM FLAGS... EXPR)

(define (c0-declare-define form env inblock what is-declare fb)
  (define `flags (first fb))
  (define `body (rest fb))
  (define `is-var (symbol? what))
  (define `is-func (list? what))
  (define `is-quoted (type? "`% '%" what))   ; quoted? or qquoted?
  (define `qwhat (nth 2 what))
  (define `xwhat (if is-quoted qwhat what))  ; X in "X" or "`X)
  (define `fargs (rrest what))
  (define `fname (nth 2 what))
  (define `mname (nth 2 qwhat))
  (define `value (nth 2 fb))
  (define `priv (if (find-flag "&private" flags) "p"))
  (define `is-inline (find-flag "&inline" flags))
  (define `is-global (find-flag "&global" flags))

  ;; inline function definition (to be embedded in env entry)
  (define `fdefn (append [ (for sym (rrest xwhat) (symbol-name sym)) ]
                         body))

  ;; describe syntax error
  (define `error
    (or
     ;; (declare/define SYM/LIST) or (define `SYM/`LIST)
     (check-type "L S" (if is-declare what (if is-quoted qwhat what))
                 form "FORM"
                 (concat "(" (or is-declare "define") " "
                         (if is-declare "" (if is-quoted "`")) "FORM"
                         (if is-declare "" " ...") ")"))

     (if is-declare
         (if body
             (gen-error body "too many arguments to (declare ...)"))
         ;; else is define
         (if (not body)
             (gen-error form "no BODY supplied to (define FORM BODY...)")
             (if (and (symbol? xwhat)
                      (word 2 body))
                 (gen-error (nth 2 body)
                            "too many arguments to (define %sVAR EXPR)"
                            (if is-quoted "`")))))

     ;; ensure FNAME/MNAME & FARGS/MARGS are symbols
     (if (list? xwhat)
         (first
          (filter-out
           [""]
           (for sym (filter-out "S%" (rest xwhat))
                (check-type "S" sym form "NAME"
                            (sprintf "(%s %s(NAME...))"
                                     (or is-declare "define")
                                     (if is-quoted "`")))))))

     ;; check variable/function/macro name
     (check-name (cond (is-func fname)
                       (is-var  what)
                       ((symbol? qwhat) qwhat)
                       (else mname)))

     (and is-inline
          (or is-declare (not is-func))
          (gen-error is-inline
                     "'&inline' applies only to function definitions"))))

  ;; make function name known *within* the function body
  (define `env-in (if is-func
                      (bind-sym env fname "F" flags)
                      env))

  ;; make function/variable known *after* the definition (include
  ;; inline-expanded definition, in the case of an inline function)
  (define `env-out (cond
                    (is-func (bind-sym env fname "F" flags (if is-inline fdefn)))
                    (is-var  (bind-sym env what "V" flags))
                    ((symbol? qwhat) (hash-bind (symbol-name qwhat)
                                                ["M" (first body) priv]
                                                env))
                    (else    (hash-bind (symbol-name mname)
                                        ["F" "#" priv fdefn]
                                        env))))
  (define `(globalize sym)
    ["Q" (if (find-flag "&global" flags)
             (symbol-name sym)
             (gen-global-name (symbol-name sym)))])

  ;; setform = expression that assigns the value to the name
  ;;           define var/func = declare + set
  (define `setform (if is-var
                       `(call ,["Q" "^set"] ,(globalize what) ,value)
                       `(call ,["Q" "^fset"] ,(globalize fname)
                              (lambda (,@fargs) ,@body))))

  (define `node (or error
                    (if (not (or is-declare
                                 is-quoted))
                        (c0 setform env-in))))

  (block-result inblock env-out node))


(define (ml.special-define form env inblock)
  (c0-declare-define form env inblock (nth 3 form) ""
                     (collect-flags (nth-rest 4 form))))

(define (ml.special-declare form env inblock)
  (c0-declare-define form env inblock (nth 3 form) "declare"
                     (collect-flags (nth-rest 4 form))))


;; (require STRING [&private])
;;
;; Emit code to call REQUIRE, and add the module's exported symbols to the
;; current environment.
;;
(define (ml.special-require form env inblock)
  (define `module (nth 3 form))
  (define `mod-name (string-value module))
  (define `priv (find-flag "&private" (nth-rest 4 form)))

  (or (check-type "Q" module form "STRING" "(require STRING)")
      (let ((imports (require-module mod-name priv))
            (env env)
            (mod-name mod-name))
        (if imports
            (block-result inblock
                          (append imports env)
                          ["f" "^require" ["Q" (notdir mod-name)]])))
      (gen-error form "require: Cannot find module %q" mod-name)))


;; c0-block-cc: Compile a vector of forms, calling `k` with results.
;;
;;   forms = vector of forms
;;   k = fuction to call: (k nodes new-env)
;;   prev-env = environment for previous form
;;   prev-results = results (not including previous compiled form)
;;   o = result of compiling previous form EXCEPT the first time
;;       this function is called, when it is nil.
;;       This may be a regular IL node *or* ["env" ENV NODE]

(define (c0-block-cc forms prev-env k prev-results o)
  (define `got-env (type? "env" o))
  (define `env     (if got-env (nth 2 o) prev-env))
  (define `node    (if got-env (nth-rest 3 o) o))
  (define `results (if (not o)
                       prev-results
                       (append prev-results (filter-out "Q" [node]))))
  (if (not forms)
      (k results env)
      (c0-block-cc (rest forms) env k results (c0 (first forms) env 1))))


;; Compile a vector of forms as a block (each expression may modiy
;; the environment for subsequent expressions). Return a single IL node.
;;
(define (c0-block forms env)
  (c0-block-cc forms
               env
               (lambda (results env)
                 (if (word 2 results)
                     (concat "B " results)
                     (or (first results) "Q")))))


;; Add local variables to environment.
;;
;; level = string of "$" for a lambda marker
;;
(define (lambda-arg9 xargs level env)
  &private
  (if xargs
      (let ((arg9 (gensym-name "arg9" env))
            (xargs xargs)
            (level level))
        (append
         (foreach n (indices xargs)
                  (hash-bind (symbol-name (nth n xargs))
                             ["M" ["L" "S call" ["Q" "^n"]
                                   (concat "Q " n)
                                   (concat "S " arg9)]]))
         (hash-bind arg9 ["A" (concat level 9)])))))

(define (lambda-env2 args env level)
  &private
  (hash-bind "$" ["$" level]
             (append
              ;; first 8 args = $1 ... $8
              (foreach n (indices (wordlist 1 8 args))
                       (hash-bind (symbol-name (nth n args))
                                  ["A" (concat (concat level n))]))
              ;; args 9 and up = (nth 1 $9), (nth 2 $9), ...
              (lambda-arg9 (nth-rest 9 args) level env)
              env)))

(define (lambda-env args env)
  (lambda-env2 args env (or (subst "$" "$$" (word 2 (hash-get "$" env))) "$")))


;;---------------------------------------------------------------
;; lambda

(define (lambda-check type form parent a)
  &private
  (check-type type form parent a "(lambda (NAME...) BODY...)"))

;; special form: (lambda ARGS BODY)

(define (ml.special-lambda form env)
  &private
  (define `argform (nth 3 form))
  (define `arglist (rest argform))
  (define `body (nth-rest 4 form))

  (or (lambda-check "L" argform form "(NAME...)")
      (vec-or (for a arglist
                   (lambda-check "S" a form "NAME")))
      ["X" (c0-block body (lambda-env arglist env)) ] ))


(define (ml.special-begin form env)
  &private
  (define `args (rrest form))
  (c0-block args env))


;; qquote: quote text with "unquoted" fragments

(declare (c0-mq))

;; Demote in IL domain
;;
;; (exec-il (il-demote (c0 'exp))) == (exec-il (c0 '(demote exp)))
;;
(define (il-demote node)
  (if (string? node)
      ["Q" (word 2 node)]
      ["f" "^d" node]))

;; Fold 'C' with its args, if possible
;;
(define (il-foldcat args)
  (cond ((word 2 args) (concat "C " args))
        ((word 1 args) (first args))
        (else ["Q" ""])))

;; Merge consecutive Q nodes into one Q node, deleting empty Q nodes.  Leave
;; other nodes unchanged.
;;
;;   text = text accumulated from previous Q nodes (demoted)
;;
(define (il-qmerge nodes text)
  (if (string? (word 1 nodes))
      (il-qmerge (rest nodes) (concat text (word 2 (first nodes))))
      (append (if (subst "!." "" text) [(concat "Q " text)])
              (if nodes (append (word 1 nodes)
                                (il-qmerge (rest nodes) nil))))))

;; Construct an IL node that evaluates to an AST node.
;;
;;    node = original node (word 1 will be preserved)
;;    args = vector of IL nodes to be appended to word 1
;;           These must be pre-demoted with il-demote
;;
(define (c0-mq-vec node args)
  (il-foldcat
   (il-qmerge (subst " " (concat " " [["Q" " "]] " ") args)
              [(concat (word 1 node) " ")])))

;; Construct an IL node that evaluates to an AST node.
;;
(define (c0-mq-node node arg)
  (c0-mq-vec node [(il-demote arg)]))


;; The result is an IL node that evaluates to the list.
;;
;; Without splicing:
;;   (il-vector (append "Q L" (for a (rest node) (c0-mq a env nest))))
;;
;;   ["Q L" "S a"]  -->  (il-vector ["Q" "L"] ["Q" "S a"])
;;                  -->  (il-concat ["f" D ["Q" "L"]] ["Q" " "]
;;                                  ["f" D ["Q" "S a"]])
;;                  -->  ["Q" "L S!0a"]
;;
;;   ["Q L" ["," ["S a"]  -->  (il-vector ["Q" "L"] ["V" "a"])
;;                        -->  (il-concat ["f" D ["Q" "L"]] ["Q" " "]
;;                                        ["f" D ["V" "a"])
;;                        -->  ["C" ["Q" "L "] ["f" D ["V" "a"]]]
;;
;;   ["Q L" ["@" ["S a"]  -->  (il-append [["Q" "L"]] ["V" "a"])
;;
(define (c0-mq-list node env nest)
  (c0-mq-vec node
             (for a (rest node)
                  (if (sunquoted? a)
                      (c0 (nth 2 a) env)
                      (il-demote (c0-mq a env nest))))))


;; expand a single node within a quasiquoted expression
;;
(define (c0-mq node env nest)
  (define `arg (nth 2 node))
  (cond
   ((unquoted? node) (if nest
                         (c0-mq-node node (c0-mq arg env (rest nest)))
                         (c0 arg env)))
   ((qquoted? node) (c0-mq-node node (c0-mq arg env (append nest 1))))
   ((list? node) (c0-mq-list node env nest))
   ((error? node) node)
   (else ["Q" node])))


(define (c0-error form)
  (gen-error form (if (unquoted? form)
                      "unquote (,) outside of a quasiquoted (`) expression"
                      "bad AST node: %q") form))

;; c0: compile an inline expression.  Return IL.  (see c0-block)
(define (c0 form env inblock)
  (cond
   ((symbol? form)  (c0-S form env (resolve form env)))
   ((string? form)  form)
   ((list? form)    (c0-L form env (resolve (nth 2 form) env) inblock))
   ((error? form)   form)
   ((quoted? form)  ["Q" (nth 2 form)])
   ((qquoted? form) (c0-mq (nth 2 form) env))
   (else            (c0-error form))))


;; forms -> IL_nodes
(define (gen0 forms env)
  (c0-block-cc forms
               env
               (lambda (results env)
                 results)))
