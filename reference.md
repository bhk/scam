# SCAM Reference

## Index

 * [Overview] (#overview)
 * [Syntax] (#syntax)
 * [Data Values] (#data-values)
 * [Symbols] (#symbols)
 * [Standard Features] (#standard-features)
 * [Make Features] (#make-features)
 * [Debugging] (#debugging)


## Overview

A SCAM program consists of *expressions*. Some examples of expressions are
constants, function invocations, and variable declarations.

Expressions are *evaluated* when the program is executed. Evaluation
computes a data value that is said to be "returned" from the expression.
Some expressions, like function invocations, can contain sub-expressions,
and evaluation of those may be performed as part of evaluating the parent
expression.

Expressions can occur in *block* or non-block contexts. A block is a
sequence of expressions that are evaluated in order. The return value of
each expression is discarded, except for the last expression, whose return
value serves as the value for the entire block. Expressions in a block
context may *declare* symbols that are meaningful within expressions that
follow (up to the end of the block).

A SCAM module is a source file that contains a sequence of expressions that
are treated as members of a block. When a module is executed, the return
values (including that of the last expression) are discarded.

## Syntax

### String Literals

String literals are enclosed in double quote characters (`"`).

The sequences `\n` and `\t` represent newline and tab. Use a backslash
`\` to escape another blackslash or a double quote. String literals
*may* contain embedded (unquoted) newlines (except in the REPL).

    "abc"
    "Hello, World!\nAnd one more line"

String literals evaluate to the content of the string.

### Numbers

Numbers consist entirely of digits, optionally preceded with a `-`
character, and optionally including one decimal point after the first
digit, and optionally followed by an exponent -- "e" or "E" followed by a
decimal integer.

    123
    12341234234982341234.0987098677896
    0.12e99

Numbers evaluate to the string used to represent them. Writing `1.0` is
equivalent to writing `"1.0"`.

### Symbols

Symbols are sequences of characters delimited by whitespace, `(`, `)`, `[`,
`]`, or `;`.

    a
    this-is-a-long-symbol-with-perhaps-*/@-unusual-characters!

WHen symbols are used to name functions or variables, they may not contain
`:`, `%`, or `$` characters.

### Compound Expressions

Compound expressions are a number of expressions enclosed in
parentheses. These denote function calls, macro invocations, or "special
forms" that can control program flow.

    (eq a (subst b c d))

    (if a (print b))

### Vector Constructors

Vector constructors are a number of expressions enclosed in square brackets.

    [ 1 2 "c" ]
    []

Vectors represent sequences of values.  [vector operations]
(#vectors) such as `nth`, `first`, and `rest` can be used extract
values from vectors.

### Syntax Quoting

A "single quote" or "backtick" character can precede an expression,
producing a *quoting* expression. No spaces may occur between these prefix
characters and the expressions that follow them.

Within a backtick-escaped expression, a comma prefix "un-escapes" an
expression, and ``,@`` un-escapes and splices the results into the current
list.

    '(f 1 2)
    `(let ((,var ,value)) ,@body)

Quoting expressions evaluate to the parse tree (AST) representation of the
quoted expression, which is called a "form". Syntax quoting is not useful
for quoting ordinary data structures (as in other Lisps) because ASTs are
not represented by native data types. Use the vector constructor, instead,
unless you are actually interested in manipulating source code.

## Data Values

SCAM has one data type. All values are character strings. Strings are
immutable. Variable assignments and parameter passing are by value, not by
reference. Although SCAM lacks many of the data types you may be familiar
with from other languages -- numbers, booleans, functions, vectors, or
hashes -- all the same information can be conveyed in strings.

Numbers are typically represented as a string of decimal digits. GNU Make
builtin functions and the the `num` module supplied with SCAM expect numbers
in this format.

    > (word 2 "a b c")
    "b"
    > (+ 12 34)
    46

Boolean results use the empty string to represent the false or failure
condition, and any other value to represent true.

    > (if "" 1 2)
    2
    > (if "false" 1 2)
    1

Complex data structures can be represented by strings that have a
well-defined internal structure.

Functions are strings that can be executed.

### Vectors

A vector is a list of zero or more words delimited by a whitespace
character.  Each word contains a string that has been *word-encoded*.  By
encoding vectors in this manner we can employ Make builtin functions that
provide random access into a word list, such as `word`, `wordlist`, and
`lastword`.

The function `demote` word-encodes a string, and `promote` reverses the
effects of `demote`. Word encoding leaves strings unchanged unless they
contain a space, a tab, or "`!`", so for many common data sets SCAM vectors
are identical to Make word lists.

`(vector A B C ...)` creates a vector. This is equivalent to `(concat
(demote A) " " (demote B) " " (demote C) ... )`.

`(nth INDEX VEC)` extracts an item from a vector. This is shorthand for
`(promote (word INDEX VEC))`.

Here is an overview of operations on vectors. The last column shows SCAM
syntax for the analogous operation on ordinary word lists (in which items
are not word-encoded).

    +--------------------+------------------------+-----------------------------+
    | Operation          | Vectors                | Word Lists                  |
    +====================+========================+=============================+
    | Create             | `[A B C]`              | `(concat A " " B " " C)`    |
    +--------------------+------------------------+-----------------------------+
    | Extract one item   | `(nth NDX VEC)`        | `(word NDX LIST)`           |
    +--------------------+------------------------+-----------------------------+
    | Extract first item | `(first VEC)`          | `(firstword LIST)`          |
    +--------------------+------------------------+-----------------------------+
    | Extract last item  | `(last VEC)`           | `(lastword LIST)`           |
    +--------------------+------------------------+-----------------------------+
    | Prepend item       | `(cons ITEM VEC)`      | `(concat WORD " " LIST)`    |
    +--------------------+------------------------+-----------------------------+
    | Append item        | `(conj VEC ITEM)`      | `(concat LIST " " WORD)`    |
    +--------------------+------------------------+-----------------------------+
    | Append vectors     | `(append V1 V2...)`    | `(concat LIST1 " " LIST2)`  |
    +--------------------+------------------------+-----------------------------+
    | Count items        | `(words VEC)`          | `(words LIST)`              |
    +--------------------+------------------------+-----------------------------+
    | Get range          | `(wordlist A B VEC)`   | `(wordlist A B LIST)`       |
    +--------------------+------------------------+-----------------------------+


Square brackets provide a shorthand syntax for vector construction. `[X Y Z
...]` is equivalent to `(vector X Y Z)`. Some examples from the interactive
prompt:

    > (print ["a" "b" "c"])
    a b c
    > (print ["a" "b c"])
    a b!0c
    > (print (nth 2 ["a" "b c"]))
    b c


### Function Values

A *function value* is a string that contains executable code. Functions are
invoked when a compound form is evaluated and the first item in the compound
form is function value.

    > (define (f x y)
    +   (concat x y))
    > (f "a" "b")
    "ab"

Typically, function values are stored in global variables, as in the example
above, but functions are first class values in SCAM, so any expression can
be used as a function value.

    > ((if 1 f) "a" "b")
    "ab"

You may wonder how functions are encoded as strings.  Officially, this is
implementation-dependent.  SCAM does not guarantee any particular
representation, so it is best not to make any assumptions about how function
values are executed.  There is however, only one implementation at present,
and it uses Make syntax to encode functions, so you will see the following
behavior in the REPL:

    > (lambda (a b) (concat a b))
    "$1$2"
    > ("$1$2" "a" "b")
    "ab"


### Syntax Trees

The result of parsing an expression is a SCAM syntax tree. Each node in the
syntax tree is called a "form".

In ordinary Lisps, various syntactic constructs (symbol, string, number,
list) correspond to distinct primitive types. In SCAM, there is only one
primitive type (string), so the different syntactic constructs are
represented as vectors.

Forms are not ordinarily manipulated by SCAM code, except in the compiler
source code. Refer to `parse.scm` for more information on forms.


## Symbols

Symbols can be *bound* to a definition, depending on the lexical scope of
the expression in which they are mentioned. A symbol can identify:

- A local "variable"
- A global variable
- A Make builtin
- A symbol macro
- A compound macro

Some symbol bindings are built in to the SCAM language. For example, all
Make builtin functions are made available to SCAM modules in the initial
lexical environment.

SCAM modules can introduce new bindings in the following ways:

- The `define` and `declare` expressions introduce bindings that are visible
  to subsequent expressions in the same block.

- The `let`, `let&`, and `lambda` expressions introduce bindings that are
  visible within some of their sub-expressions.

### Variables

Variables are symbols that evaluate to values. This is a bit of a misnomer
because local variables are immutable.

There are three kids of variables in SCAM:

 * local
 * global
 * automatic

Local variables are created with `let` expressions, or as function
parameters in a `lambda` expression or function definition. The visibility
of a local variable is limited to the expression in which it is defined, and
the lifetime of the local variable is limited to the execution of that
expression. Local variables are immutable. They are assigned a value when
created, and cannot be assigned a different value.  See [`let`] (#let).

Global variables are ordinary GNU Make variables. Their lifetime is
unlimited, and they are visible to other SCAM modules and Makefiles (SCAM
globals are Make variables). However, to reference a global variable with a
symbol in SCAM, you must first declare the variable. Global variables can be
of two "flavors" that correspond to GNU Make "flavors" (described below).

For readers comparing with other Lisp dialects, it is useful to note that
globals are functionally equivalent to "dynamic" or "special" variables in
the parlance of Common Lisp. The following special forms deal with global
variables: [`define`] (#define), [`declare`] (#declare), [`let-global`]
(#let-global), [`set`] (#set), [`set-global`] (#set-global), [`set-rglobal`]
(#set-rglobal).

Automatic variables are the variables created by GNU Make's `foreach`
builtin or SCAM's `for` macro. Automatic variables are immutable (as are
local variables) but they are visible to code in other modules, shadowing
any global variables that are known to those other modules.

The visibility of globals and automatic variables makes them less safe than
locals, but globals provide an outlet when side effects are needed, and
`foreach` provides a relatively performant mechanism for many operations, so
avoiding them entirely is unrealistic for many real-world applications.
Here are recommended naming conventions for limiting running into unintended
behavior:

  - Use long names for globals, and begin and end them with "`*`" (a Common
    Lisp convention).

  - Use very short variable names for automatic variables.

  - Avoid very short variable names for local variables.

#### Variable Flavor

One interesting aspect of GNU Make is the notion of flavor: global
variables can be "recursive" or "simple".

When a variable is *recursive*, evaluation of the Make expression `$(var)`
will *expand* the variable, which means that `$`-initiated sequences are
evaluated and replaced with the resulting value. When a variable is
*simple*, `$(var)` will simply evaluate to the value of the variable.

In SCAM, by contrast, the distinction between "execute X" and "return the
value of X" is explicit in the source code.  `X` means "value of X" whereas
`(X)` means "execute X". This is true whether X is recursive or simple.

In order to make interoperability with raw Make code more convenient, SCAM
variables that are defined *as* functions will be stored in Make recursive
variables, and other values will be stored in simple variables.

    > (define (f) o1)
    > (flavor "f")
    "recursive"
    > (define x 1)
    > (flavor "x")
    "simple"

### Inline Functions

Functions can be designated as "inline functions" by including the flag
`&inline` before the function body.  For example:

    > (define (f a b)
    +    &inline
    +    (concat a b))

When an inline function is called directly by name -- e.g.  `(f 1 2)` -- the
SCAM compiler will expand the definition of the function inline, instead of
generating a Make `call` expression.  Since this avoids the overhead of a
function call, simple functions will often execute faster when inlined.
However, the resulting generated code will often be larger, and at some
point the increased size will result in even slower performance than a
non-inline call.

Be aware that behavior can differ from an ordinary call in the following
respect: argument expressions may be evaluated zero or more times or in a
different order, whereas arguments to ordinary function calls are evaluated
exactly once and in a well-defined order. So if your argument expressions
include side effects, and an inline function does not evaluate each argument
exactly once and in-order, the results may differ. Here is one example:

    > (define x 0)
    > (define (i)
    +    (set x (1+ x) x))
    > (define (foo a)
    +    &inline
    +    (concat a a a))
    > (foo (i))
    012

Like ordinary function definitions, inline function definitions bind a
global variable to a function value, so the function name can be called from
Make code, or passed as a argument to a function that accepts function
values.

### Macros

Macros are user-defined syntax extensions. There are two forms of macros:
symbol macros and compound macros.

Symbol macros associate a symbol with an expression.  The syntax is:

    (define `NAME EXPR)

Each subsequent occurrence of the symbol within that block will expand to
the expression.  Here is an example in the REPL:

    > (define `TIME
    +     (shell "date +%s"))
    > TIME
    1358790184
    > TIME
    1358790187

Compound macros accept arguments.  The syntax for a compound macro definition is:

    (define `(NAME ARGS...) BODY...)

Macros are invoked just like functions:

    (NAME EXPR...)

Each invocation of the macro will be replaced by a `begin` block containing
the macro body.  Within the macro body, the macro argument names are bound
to the argument expressions.  Compound macros behave much like functions, but
(1) they do not bind global variables to a function value, as function
definitions do, and (2) when invoked, their argument expressions may be
evaluated zero or more times.  Here is an example in the REPL:

    > (define `(m a)
    +     (subst " " "" a))
    > (m [1 2 3])
    123
    > m
    Error: attempt to evaluate a compound macro name

SCAM macros are hygienic -- they adhere to lexical scoping rules, just like
SCAM functions.  Symbols named in the body of a macro are matched with the
binding that was in scope where the macro was defined.

    >  (let ((a 1))
    +     (define `X a)
    +     (let ((a 2))
    +        (define `(f z) (concat X a z))
    +        (let ((a 3))
    +           (f a))))
    123


### Builtins

Every Make [builtin function]
(http://www.gnu.org/software/make/manual/make.html#Functions) is available
from SCAM. For example:

    > (word 2 "a b c")
    "b"
    > (addsuffix ".c" "x")
    "x.c"

Builtins are "special forms". They are not variables bound to function
values.

    > subst
    Line 1: Attempt to obtain value of builtin: subst
    at: *subst*

Builtins may also differ from ordinary functions in the way arguments are
evaluated. Ordinarily, all arguments are evaluated in order *before* a
function is called. In the case of `if`, `and`, `or`, and `foreach`,
however, arguments may not be evaluated at all, and in the case of
`foreach`, they may be evaluated more than once.

    > (if "" (error "unexpected") 1)
    1

Some builtins accept variable *names*. These builtins -- `value` and `call`
-- are potentially confusing because they cross layers of abstraction. Be
aware that they use string values to refer to the variables, whereas in SCAM
you ordinarily reference variables by symbols. Using an unquoted symbol
name can produce unexpected results. The following example demonstrates
`value` and `call`:

    > (define (g x) (concat x x))
    > (value "g")    ; NOT (value g)
    "$1$1"
    > (call "g" "x") ; NOT (call g "x")
    "xx"
    > (define f "g")
    > (value f)      ; same as (value "g")
    "$1$1"
    > (call f "x")   ; same as (call "g" "x")
    "xx"

The names [`foreach`] (#foreach) and [`subst`] (#subst) builtins are
actually special forms that invoke the true builtins, which are available
in their raw forms by the names `.foreach` and `.subst`.

Directly using the `.foreach` builtin can be awkward, because the referenced
variable name is unknown to SCAM and will trigger an error unless a declaration
is used:

    > (.foreach "x" "1 2 3" (1+ x))
    line 1: undefined variable: x
    at: (.foreach "x" "1 2 3" (1+ *x*))
    > (.foreach "x" "1 2 3" (begin (declare x) (1+ x)))
    "2 3 4"

The [`foreach`] (#foreach) special form avoids this problem.  The
[`subst`] (#subst) special form enhances `.subst` by allowing multiple
substitutions to be performed.


## Standard Features

The following symbols are defined by the SCAM language.  They call into
three different categories:

 - Special forms
 - Make builtins
 - Manifest functions
 - Manifest macros

Manifest functions are like other functions in SCAM, except that they are
provided by the language itself.  Special forms, builtins, and macros are
not functions, so they do not have values and cannot be passed to other
functions; they can only be invoked like a function.


### `begin`

[Special form]

    (begin EXPR...)

Encloses a *block* of expressions.  A block is a sequence of expressions
that are evaluated in order.  The result of that last expression is
returned (or nil if no expressions are given).

`begin` can be used to contain multiple expressions in a single expression.
It can also be used to limit the scope of symbols.  The end of the block
terminates the scope of any definitions and declarations made within the
block.


### `lambda`

[Special form]

    (lambda (ARGS...) BODY...)

A `lambda` expression evaluates to a function value.

`ARGS...` is zero or more symbols that name the formal arguments.

`BODY...` is a block of one or more expressions (see [`begin`] (#begin) )
that will be executed when the function is called. The initial environment
of `BODY...` contains bindings for the arguments names to the values passed
to the function.

### `require`

[Special form]

    (require MODULE &private?)

The `require` form provides access to functionality defined in other
modules.  `MODULE` identifies either a SCAM source file, minus its `.scm`
extension, relative to the directory containing the requiring file) or a
bundled module.

`MODULE` must be a literal string value, because `require` is processed at
build time and compile time in addition to run time.

**Build Time:** When SCAM is invoked with the `scam -o EXE SOURCEFILE`
command syntax, it first constructs a dependency graph that describes all
required compilation and testing steps for all of the named source files and
their direct and indirect dependencies.  This graph construction phase is
what we mean by "build time".

SCAM discovers dependencies by scanning source files for `require`
directives.  For each required module, SCAM looks for a corresponding
source file, whose name is constructed by appending ".scm" to the module
name and treating that as a path relative to the directory containing the
requiring source file.

If the source file is found, SCAM will build a corresponding `.min` file in
the "object directory", which is the directory that contains the target
executable.  Outside of the context of a `scam -o` command, such as during
`scam -e` or `scam -x`, the object directory is not in effect, and can be
thought of as an empty directory.

If the source file is not found, no build step is performed for it (it is
assumed to exist as a bundled module).

**Compile Time**: When a `(require ...)` directive is compiled, the compiler
obtains the symbol table for the required module by loading its object file.
(Required files must be compiled before their requiring files.)

Modules are located as at run time, except that bundled files are located
within the compiler itself (not the resulting program).

**Run Time:** When a module executes a `require` directive, the module's
object file will be loaded and executed, unless it has already been
required.  Object files are in Makefile syntax, and are assigned a ".min"
extension by SCAM.  Modules are located using only the `notdir` portion of
the module name, either in a module bundled with the running program, or as
a file in the object directory.

 - When execution is triggered by a `scam -o` build, the module will be read
   from the object directory if it is one of the modules included in the build.

 - Otherwise, if a matching bundled module is present, it will be used.

 - Otherwise, the object directory will be searched.

**Bundling:** "Bundled" modules are those that have been incorporated into a
SCAM program.  The `scam` command, for example, contains bundled versions of
all modules required for compilation (it needs them in order to compile).
When programs are build with `scam -o ...`, all required modules will be
bundled in the resulting program.


### `let`

[Special form]

    (let ( (NAME VALUE)... ) BODY... )

The `let` special form assigns values to a number of local variables.
The VALUE expressions are evaluated, in order.  Then BODY (a block of
expressions) is evaluated in an environment in which each NAME is bound
to its corresponding value.  The `let` expression returns the value of
the last form in BODY.

`let` is implemented in terms of `lambda`, as such:

    ((lambda (NAME...) BODY...) (VALUE...))

### `let&`

[Special form]

    (let& ( (NAME EXPR)... ) BODY... )

`let&` is a "lazy" let.  It binds the names to symbol macros instead of
local variables.  It also differs from `let` in that each expression is
evaluated in the context of the previous bindings -- more like Scheme's
`let*`.  It is equivalent to the following:

    (begin
      (define `NAME EXPR)...
      BODY...)

Since `let&` constructs symbol macros, each bound expression is not
always evaluated exactly once, as with `let`.  Instead, each expression
is evaluated once each time its associated name is evaluated within
`BODY...` -- perhaps zero times, perhaps many more.

Aside from the potential for multiple re-evaluations of expressions,
`let&` generally has lower overhead than `let`, since it does not involve
a Make function call (as does `let`).

### `define`

[Special form]

    (define NAME FLAGS... VALUE)               ; global variable
    (define (NAME ARGS...) FLAGS... BODY...)   ; global function
    (define `NAME EXPR)                        ; symbol macro
    (define `(NAME ARGS...) FLAGS... BODY)     ; compound macro

The `define` special form adds a name to the environment and associates
it with a definition.  The defined name will be visible to subsequent
expressions in the same block, and the definition supersedes any
previous definitions associated with the same name.

`FLAGS` consists of zero or more of the symbols `&private` and `&inline`.

  * `&private` indicates that the symbol should not be visible outside
    of the file in which it is declared.

  * `&inline` indicates that the function should be expanded inline when it
    is called by name.  This flag can be used only with `define` (not with
    `declare`) and only for function definitions.

Some examples:

    (define foo (wildcard "*"))

... expands to:

    (declare foo)
    (set foo (wildcard "*"))

... which compiles to the following Make syntax:

    foo := $(wildcard *)

A function definition:

    (define (func pat) (wildcard pat))

... expands to:

    (declare (func pat))
    (set func (lambda (pat) (wildcard pat)))

... and compiles to:

    func = $(wildcard $1)


### `declare`

[Special form]

    (declare NAME FLAGS...)              ; declare global variable
    (declare (NAME ARGS...) FLAGS...)    ; declare global function variable

The `declare` special form declares a global variable without assigning
a value.  This is usually used to access non-SCAM functions, or when
mutually recursive functions are defined.

In GNU Make terms, these forms do not actually *create* a variable since
they perform no assignment. They do however cause SCAM to *assume* a
flavor of "simple" (in the first case) or "recursive" (in the function
form). This assumption will affect subsequent references and assignments
performed within SCAM.


### `set`

[Special form]

    (set NAME VALUE RETVAL?)

The `set` special form assigns a value to a previously declared global
variable.

NAME is given as a symbol, not a string. For example:

    (set var 12)

`RETVAL` is an optional parameter.  The `set` expression returns `RETVAL`
(or "" if `RETVAL` is not provided).


### `let-global`

[Special form]

    (let-global ( (NAME VALUE)... ) BODY...)

This form modifies the value of some number of global variables *during
the execution of BODY*. Afterwards, the original values are restored.

This expression evaluates to the value of the last expression in BODY.


### `set-global`

[Manifest function]

    (set-global NAME-EXPR VALUE)

This executes a "simple" global variable assignment.  The variable name
is given by the value of NAME-EXPR.  The assigned value is given by
VALUE. REPL example:

    > (set-global "V" "abc")
    > (flavor "V")
    simple
    > (value "V")
    "abc"


### `set-rglobal`

[Manifest function]

    (set-rglobal NAME-EXPR VALUE)

This is equivalent to [`set-global`] (#set-global), except that is executes
a "recursive" global variable assignment.  REPL example:

    > (set-rglobal "V" "abc")
    > (flavor "V")
    recursive
    > (value "V")
    "abc"


### `concat`

[Special form]

    (concat VALUE...)

This special form concatenates all of its arguments.


### `subst`

[Special form]

    (subst FROM TO {FROM TO}... VALUE)

This special form replaces substrings with replacement strings within the
given VALUE.  It is equivalent to the GNU Make builtin `subst` except that
it accepts multiple pairs of replacement strings.  For example:

    > (subst 2 3 1 2 12)
    23
    > (lambda () (subst 2 3 1 2 12))
    "$(subst 1,2,$(subst 2,3,12))"


### `foreach`

[Special form]

    (foreach VAR LIST EXPR)

The `foreach` special form iterates over a list, evaluates an expression
once for each word, and constructs a new word list from the results of
each evaluation.

Each word is bound to the name `VAR` and `EXPR` is evaluated to obtain
the corresponding word in the new list.

SCAM provides a macro named `foreach` that is more intuitive:

    > (foreach x "1 2 3" (1+ x))
    "2 3 4"

### `for`

[Special form]

    (for VAR VECTOR EXPR)

`for` iterates over items in a vector, constructing a new vector. Example:

    > (for x [[1 2] [3 4]]
    +     (reverse x))
    [[2 1] [4 3]]


### `demote`

[Manifest function]

    (demote VALUE)

`demote` encodes any value as a word so that it may be embedded in a word
list.  It is used internally to construct vectors.

### `promote`

[Manifest function]

    (promote VALUE)

`promote` reverses the encoding done by `demote`.

### `nth`

[Manifest function]

    (nth INDEX VEC)

Returns the value stored at index INDEX (1-based) in vector VEC.

### `print`

[Special form]

    (print VALUE...)

Concatenates all values and write them to stdout.


### `current-env`

[Special form]

    (current-env)

This special form evaluates to the data structure that describes the lexical
environment at the point where it is invoked.  (See `gen.scm` for details of
this structure.)

### `current-file-line`

[Special form]

    (current-file-line)

This special form evaluates to the file name and line number of where it was
invoked, in this format:  `FILENAME:LINE`


### `vector`

[Special form]

    (vector A B C ...)

This constructs a vector.  It is equivalent to `[A B C ...]`.


### `?`

[Special form]

    (? FUNCNAME ...)

This performs call-site tracing, describe above under "Debugging".


### `cond`

[Special form]

    (cond (CONDITION BODY)... [(else BODY)] )

`cond` expresses conditional execution of an arbitrary number of possible
blocks of code.

Example:

    (cond ((filter "foo" a)  "Found FOO")
          ((filter "bar" a)  "Found BAR")
          ((filter "baz" a)  (print "BAZ") "Found BAZ")
          (else              "Found NOTHING"))

### `apply`

[Manifest function]

    (apply LAMBDA VEC)

Call LAMBDA, passing as arguments the members of the vector VEC.

Example:

    (apply nth [3 "a b c d"])    ;; --> c


### `*args*`

[Manifest macro]

This symbol evaluates to a vector of all arguments passed to the function.
This is evaluated at run-time, and reflects all arguments passed, not just
the list of declared arguments.  This is for use by functions that take
variable numbers of arguments.

Note: `*args*` returns unpredictable results when it is used within a `let`
or `let*` block.


### `*file*`

[Manifest global]

This symbol evaluates to the file currently being loaded via `require`.
This is evaluated at run-time, not compile-time, so it does not necessarily
return the name of the file in which it appears.


## Make Features

Variables and builtin functions defined by Make are available to SCAM
programs.  See <http://www.gnu.org/software/make/manual/make.html#Name-Index>
for a complete list.

### Make Variables

The following Make-defined variables are pre-declared for SCAM programs:

    .DEFAULT_GOAL MAKEFILE_LIST

Other variables can be accessed using the `value` function, or by declaring
the variable and then referencing its name:

    (declare MAKELEVEL)
    (print MAKELEVEL)

### Make Builtin Functions

The following Make builtin functions are available to SCAM programs:

    abspath basename dir error eval firstword flavor info lastword notdir
    origin realpath shell sort strip suffix value warning wildcard words
    addprefix addsuffix filter filter-out findstring join word patsubst
    subst wordlist if and or call

Also, the `foreach` builtin is available under the name `.foreach`.  See
[`foreach`] (#foreach), above, for more details.

### Rule Processing

After a SCAM program's `main` executes, make will proceed to perform rule
processing.  During execution of `main`, the program can create rules using
`eval`.  For example:

    (eval "foo.o: foo.c ; cc -o $@ $<")

If the program defines rules, then Make will proceed to process them as
usual, treating the first-defined rule as the default goal.

If the program defines no rules, then the runtime creates a rule to serve as
the default goal which is used to cause the program to return the exit code
that `main` returned.


## Debugging

### Call Site Tracing

The "?" macro can be used for tracing at a function call site. `(? f ...)`
is equivalent to `(f ...)`, but when it executes the function name, its
arguments and return value will displayed. For example, in the REPL:

    > (define (f x) (concat x x))
    > (? f 1)
    --> (f "1")
    <-- f: 11
    11

### Run-time Tracing

The `SCAM_TRACE` variable can be assigned to instrument functions at
run-time, rather than compile-time.  For complete details, see `trace.scm`.
The following command line, which counts function invocations across the
compiler, gives a taste of what SCAM_TRACE is capable of:

    $ SCAM_TRACE=':c' bin/scam num.scm

The `SCAM_DEBUG` variable causes certain debug information to be written to
stdout based on the presence or absence of certain substrings:

  * "R" ==> "require: <filename>" when a file is included by `require`.
  * "Rx" ==> "R" + "exited: <filename>" after a required file exits.
  * "E" ==> "eval: <string>" when SCAM passes text to Make's `eval` builtin.
  * "O" ==> "OK: ..." when an `expect` macro succeeds. (See [the core library](core.scm).)
  * "U" ==> display compile-time "warnings" for each upvalue reference
  * "Tl" ==> "Tl: <func>" each time a function is called
  * "Tk" ==> invocation counts of functions

### Profiling

You can use invocation counts (as describe above) to look for potential
hot spots.

To measure the time spent in a particular function, you can use the
following approach:

 * Time execution of a use case.  For example:

       $ time ...command...

   The command can invoke your program via `scam -x program.scm ...args...`
   or it can invoke your program directly, assuming you have compiled it
   using `scam -o ...`.

 * Use an `x11` trace action to multiply the time spent in an individual
   function, and run the same use case:

       $ SCAM_TRACE='function-name:x11' time ...command...

 * This second invocation should take longer to execute. Dividing the
  additional time by 10 will give the amount of time spent in that function
  during that use case.
