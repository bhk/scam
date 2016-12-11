;;--------------------------------------------------------------
;; gen0 : compiler front end  (see gen.scm)
;;--------------------------------------------------------------
(require "core")
(require "parse")
(require "escape")
(require "gen")

(declare (c0 form env))
(declare (c0-block forms env))


;; Demote in IL domain
;;
;; (exec-il (il-demote (c0 'exp))) == (exec-il (c0 '(demote exp)))
;;
(define (il-demote node)
  (case node
    ((String value) (String (word 2 node)))
    (else (Call "^d" [node]))))


;; c0-local: Construct a local variable reference.
;;
;; A unary counting system identifies levels of nesting of functions:
;;
;;    "." = outermost function
;;    ".." = first nesting level down
;;    "..." = third down
;;
;; ARG is an index (decimal) preceded by the where it was bound (e.g. "..3").
;; LEVEL is the level where the variable is being referenced.
;;
(define (c0-local arg level)
  (define `ndx (subst "." "" arg))
  (Local ndx
         (words (subst "." ". " (subst arg "" (concat level ndx))))))


(if (findstring "U" SCAM_DEBUG)
    ;; Print warning for each upvalue [check SCAM_DEBUG again during
    ;; compilation so unit tests can suppress these warnings]
    (begin
      (declare (c0-local-unchecked))
      (set c0-local-unchecked c0-local)
      (define (c0-local arg level form)
        (if (not (findstring level arg))
            (compile-warn form "reference to upvalue '%q'" (symbol-name form)))
        (c0-local-unchecked arg level))))


;; Return the "value" of a record constructor: an equivalent anonymous
;; function.
;;
(define (c0-ctor form encs env)
  &private
  (define `args
    (for i (indices encs)
         (gensym (concat "S a" i) env)))
  (c0 `(lambda (,@args) (,form ,@args)) env))


(define (c0-S-macro-error form)
  (gen-error form "attempt to obtain value of compound macro %q"
             (symbol-name form)))

(define (c0-S-error form defn)
  (gen-error form
             (case defn
               ((EBuiltin realname p argc)
                "attempt to obtain value of builtin %q")
               (else (if defn
                         (concat "internal error: binding = '" defn "'")
                         "undefined variable %q")))
             (symbol-name form)))

;; Symbol
;;    defn = definition for symbol (from environment)
(define (c0-S form env defn)
  &private
  (case defn
    ((EArg ref)
     (c0-local ref (word 2 (hash-get "$" env)) form))

    ((EVar name p)
     (Var name))

    ((EFunc name p inln)
     (if (filter "#" name)
         (c0-S-macro-error form)
         (Builtin "value" [(String name)])))

    ((ESMacro name p)
     (c0 name (env-rewind env (nth 2 form))))

    ((EIL node priv)
     node)

    ((ERecord encs p tag)
     (c0-ctor form encs env))

    (else (c0-S-error form defn))))


;; Compile a vector of expressions independently, as in an argument
;; list.  Declarations and definitions on one expression do not apply to
;; subsequent ones (inlike c0-block-cc).
;;
(define (c0-vec forms env)
  (for f forms (c0 f env)))


;; Expand inlined function
;;
;; 1. Compile each argument expression to IL.
;; 2. Expand function definition in environment that matches its original
;;    environment PLUS bindings for the formal arguments (bound to an "I"
;;    value).
;;
(define (c0-inline-fn form env defn finl)
  (define `args (rrest form))
  (define `formal-args (first finl))
  (define `fbody (rest finl))
  (define `orig-env (hash-bind "#pos" (concat "P " (form-index form))
                               (env-rewind env (symbol-name (nth 2 form)) defn)))
  (define `(zip vec1 vec2)
    (join (addsuffix "!=" vec1) vec2))

  (define `argbindings
    (zip formal-args (for a args (EIL (c0 a env) "p"))))

  (or (check-argc (words formal-args) form)
      (c0-block fbody (append argbindings orig-env))))


;; Special forms are implemented in functions that begin with "ml.special-".
;;
(define `(special-form-func name)
  (local-to-global (concat "ml.special-" name)))


;; form = `(Ctor ARG...)
(define (c0-record form env encodings tag)
  (define `args (nth-rest 3 form))

  (or
   (check-argc (words encodings) form)

   (Concat
    (cons
     (String tag)
     (foreach n (indices encodings)
              (begin
                (define `enc (word n encodings))
                (define `arg (nth n args))
                (define `value (c0 arg env nil))
                (define `field (if (filter "S" enc)
                                   (il-demote value)
                                   value))

                (append [(String " ")] [field])))))))


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

  (case defn
    ((EFunc realname p inln)
     (if inln
        (c0-inline-fn form env defn inln)
        (Call realname (c0-vec (rrest form) env))))

    ((EBuiltin realname p argc)
     (or (check-argc argc form)
         (Builtin realname (c0-vec (rrest form) env))))

    ((EXMacro name priv)
     (if priv
         (c0 (call name form) env inblock)
         (gen-error form "cannot use xmacro in its own file")))

    ((ERecord encodings priv tag)
     (c0-record form env encodings tag))

    (else
     (cond
      ;; Empty list
      ((not (word 2 form))
       (gen-error form "missing function/macro name"))

      ;; Defined symbol OR non-symbol => evaluate and call
      (defn
        (Funcall (c0-vec (rest form) env)))

      ;; Macro/special form.
      ((bound? (special-form-func opname))
       (call (special-form-func opname) form env inblock))

      ;; none of the above
      (else
       (gen-error op "undefined symbol: %q" opname))))))


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


;; (declare SYM FLAGS...)
;; (define  SYM FLAGS... VALUE)
;; (define `MSYM FLAGS... EXPR)
;; (declare (FSYM FARGS...) FLAGS...)
;; (define  (FSYM FARGS...) FLAGS... BODY...)
;; (define `(MSYM MARGS...) FLAGS... BODY...)

(define (c0-declare-define form env inblock what is-declare fb)
  (define `flags (first fb))
  (define `body (rest fb))
  (define `is-var (symbol? what))
  (define `is-func (list? what))
  (define `is-quoted (type? "`% '%" what))   ; quoted? or qquoted?
  (define `qwhat (nth 2 what))
  (define `xwhat (if is-quoted qwhat what))  ; X in "X" or "`X)
  (define `fargs (rrest what))
  (define `fsym (nth 2 what))
  (define `fname (symbol-name fsym))
  (define `gfname (gen-global-name fname flags))
  (define `wname (symbol-name what))
  (define `gwname (gen-global-name wname flags))
  (define `msym (nth 2 qwhat))
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

     ;; ensure FSYM/MSYM & FARGS/MARGS are symbols
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
     (check-name (cond (is-func fsym)
                       (is-var  what)
                       ((symbol? qwhat) qwhat)
                       (else msym)))

     (and is-inline
          (or is-declare (not is-func))
          (gen-error is-inline
                     "'&inline' applies only to function definitions"))))

  ;; make function name known *within* the function body
  (define `env-in
    (if is-func
        (hash-bind fname (EFunc gfname (or priv ".") nil) env)
        env))

  ;; make function/variable known *after* the definition (include
  ;; inline-expanded definition, in the case of an inline function)
  (define `env-out
    (append
     (cond
      (is-func (hash-bind fname (EFunc gfname (or priv ".") (if is-inline fdefn))))
      (is-var  (hash-bind wname (EVar gwname priv)))
      ((symbol? qwhat) (hash-bind (symbol-name qwhat)
                                  (ESMacro (first body) priv)))
      (else (hash-bind (symbol-name msym)
                       (EFunc "#" (or priv ".") fdefn))))
     env))

  (define `(globalize sym)
    ["Q" (gen-global-name (symbol-name sym) flags) ])

  ;; setform = expression that assigns the value to the name
  ;;           define var/func = declare + set
  (define `setform (if is-var
                       `(call ,["Q" "^set"] ,(globalize what) ,value)
                       `(call ,["Q" "^fset"] ,(globalize fsym)
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
                          (Call "^require" [ (String (notdir mod-name)) ]))))
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
                     (Block results)
                     (or (first results) (String ""))))))


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
                             (ESMacro ["L" "S call" ["Q" "^n"]
                                       (concat "Q " n)
                                       (concat "S " arg9)] ".")))
         (hash-bind arg9 (EArg (concat level 9)))))))

(define (lambda-env2 args env level)
  &private
  (append (hash-bind "$" (EMarker level))
          ;; first 8 args = $1 ... $8
          (foreach n (indices (wordlist 1 8 args))
                   (hash-bind (symbol-name (nth n args))
                              (EArg (concat level n))))
          ;; args 9 and up = (nth 1 $9), (nth 2 $9), ...
          (lambda-arg9 (nth-rest 9 args) level env)
          env))

(define (lambda-env args env)
  (lambda-env2 args env (concat "." (word 2 (hash-get "$" env)))))


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
      (Lambda (c0-block body (lambda-env arglist env)))))


(define (ml.special-begin form env)
  &private
  (define `args (rrest form))
  (c0-block args env))


;; qquote: quote text with "unquoted" fragments

(declare (c0-mq))

;; Fold 'C' with its args, if possible
;;
(define (il-foldcat args)
  (cond ((word 2 args) (Concat args))
        ((word 1 args) (first args))
        (else (String ""))))

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
   (il-qmerge (subst " " (concat " " [(String " ")] " ") args)
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
   (else (String node))))


(define (c0-error form)
  (gen-error form (if (unquoted? form)
                      "unquote (,) outside of a quasiquoted (`) expression"
                      "bad AST node: %q") form))

;; c0: compile an inline expression.  Return IL.  (see c0-block)
(define (c0 form env inblock)
  (cond
   ((symbol? form)  (c0-S form env (resolve form env)))
   ((string? form)  (String (nth 2 form)))
   ((list? form)    (c0-L form env (resolve (nth 2 form) env) inblock))
   ((error? form)   form)
   ((quoted? form)  (String (nth 2 form)))
   ((qquoted? form) (c0-mq (nth 2 form) env))
   (else            (c0-error form))))


;; forms -> IL_nodes
(define (gen0 forms env)
  (c0-block-cc forms
               env
               (lambda (results env)
                 results)))
