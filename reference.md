# SCAM Language Reference

## Index

 * [Overview](#overview)
 * [Syntax](#syntax)
   * [String Literals](#string-literals)
   * [Numeric Literals](#numeric-literals)
   * [Symbols](#symbols)
   * [Compound Expressions](#compound-expressions)
   * [Vector Constructors](#vector-constructors)
   * [Dictionary Constructors](#dictionary-constructors)
   * [Syntax Quoting](#syntax-quoting)
   * [Comments](#comments)
 * [Data Types](#data-types)
   * [Booleans](#booleans)
   * [Numbers](#numbers)
   * [Word Lists and Vectors](#word-lists-and-vectors)
   * [Dictionaries](#dictionaries)
   * [Records](#records)
   * [Functions](#functions)
 * [Variables](#variables)
   * [Scope](#scope)
 * [Macros](#macros)
 * [Special Forms](#special-forms)
 * [Libraries](#libraries)
 * [Debugging](#debugging)
   * [Call Site Tracing](#call-site-tracing)
   * [Run-time Tracing](#run-time-tracing)
   * [Profiling](#profiling)
 * [The SCAM Compiler](#the-scam-compiler)
 * [Hashbang](#hashbang)

## Overview

If you run the [SCAM Compiler](#the-scam-compiler) without any arguments it
will enter interactive mode (a REPL: Read-Eval-Print Loop).  In this mode,
you can type **expressions** and SCAM will immediately **evaluate** them
(compute a value) and display the resulting value. Expressions are described
in the [Syntax](#syntax) section, below.

SCAM can also read expressions from source files, called **modules**.  In
the REPL you can load a module using `(require "FILENAME")`.  The first time
a file is required by a program, the module will be **loaded**, which means
that all expressions in the module will be executed.  Typically, these
expressions store function values (or other values) in global variables.
Symbols that are exported from the module will be available to the caller of
`require`.

If you run SCAM with a module (file) name as an argument, SCAM will invoke
the module a **program**.  This will load the module and then run a function
called "main" if the module has defined such a function.  Alternatively, you
can compile a program and then invoke the resulting executable file
directly.


## Syntax


### String Literals

String literals are enclosed in double quote characters (`"`).  An
expressions consisting of a string literal evaluates to the content of the
string.

A backslash introduces an escape sequence:

* `\n` represents newline.
* `\t` represents a tab character.
* `\"` represents a double quote.
* `\\` represents a single backslash.
* `\xHH` represents the byte given by the hexadecimal value `HH`.

String literals *may* contain embedded (unquoted) newlines (except in the
REPL).

    "abc"
    "Hello, World!\nAnd one more line"
    "a\x62c"

SCAM string values are sequences of non-NUL bytes.  Any `\x00` escape
sequences in string literals are silently discarded.  Where character
encoding is a factor, UTF-8 is assumed.

### Numeric Literals

Numbers consist entirely of digits, optionally preceded with a `-`
character, and optionally including one decimal point after the first digit,
and optionally followed by an exponent -- "e" or "E" followed by an optional
sign ("+" or "-") and a decimal integer

    123
    12341234234982341234.0987098677896
    0.12e+99

Numbers evaluate to the string used to represent them. Writing `1.0` is
equivalent to writing `"1.0"`.


### Symbols

Symbols are sequences of non-whitespace characters, except for any of the
following: ``()[]{},;:%$`'"``.

    a
    this-is-a-long-symbol-with/perhaps/*unusual*-characters!

Symbols can identify:

- [variables](#variables) (including functions)
- [macros](#macros)
- [special forms](#special-forms)
- [record constructors](#records)


### Compound Expressions

A compound expression is a parentheses-enclosed, space-delimited list of
expressions.

    (eq? a (subst b c d))

    (if a (print b))

If the first expression in the list is a symbol that names a "special form",
the compound expression is handled according to the definition of that
special form.

If the first expression is a symbol that is bound to a macro, the macro will
be expanded and the resulting expression evaluated.

Otherwise, the compound expression is treated as a function invocation.  All
sub-expressions will be evaluated, and the value of the first expression
will be treated as a function, which will be called with all the other
sub-expression values as its arguments.


### Vector Constructors

Vector constructors are written as a pair of square brackets enclosing zero
or more expressions.

    [ 1 2 "c" ]
    []

Vector values and functions that operate on them are [described
below](#vectors).


### Dictionary Constructors

Dictionary constructors contain a list of key-value pairs between `{` and
`}`.  A comma separates each pair, and optionally terminates the list.

    {a:1}
    { a: 1, b: 3, "a b c": "d e f" }
    {}

Any kind of expression can be used for keys and values.  In general, the
expressions evaluated when constructing the dictionary, but symbols used as
keys are treated differently.  A symbol not beginning with `=` is treated
literally; the name of the symbol becomes the key.  When a symbol begins
with `=`, another symbol is constructed from the characters that follow the
initial `=`, and that symbol is evaluated to determine the key.  For
example, `{ a: 1 }` is equivalent to `{ "a": 1 }`, and `{ =a: 1 }` is
equivalent to `{ (or a): 1 }`.

Dictionary values and functions that operate on them are [described
below](#dictionaries).


### Syntax Quoting

A "single quote" or "backtick" character can precede an expression,
producing a *quoting* expression. No spaces may occur between these prefix
characters and the expressions that follow them.  Within a backtick-escaped
expression, a comma prefix "un-escapes" an expression, and ``,@`` un-escapes
and splices the results into the current list.

    '(f 1 2)
    `(let ((,var ,value)) ,@body)

Unlike Lisps, SCAM does not use syntax quoting to represent
non-syntax-specific data structures.  SCAM's vectors and dictionaries
address those needs.  Quoting expressions evaluate to the parse tree (AST)
representation of the quoted expression.  SCAM ASTs are represented as
[records](#records), and contain additional syntax information, such as the
original position in the source code, and typing information.  See
`parse.scm` for complete details.


### Comments

Comments begin at a `;` character (outside of a quoted string) and continue
to the end of the line.  A comment is treated as whitespace.

Additionally, if the first line of a SCAM source file begins with a `#`
character, that line is ignored.  This enables [hashbang](#hashbang)
executables.


## Data Types

The notion of "data type" in computer science is both ubiquitous and
elusive.

All values in SCAM are character strings.  These strings are immutable, so
one should think of variable assignments and parameter passing as "by
value", not "by reference".

One might argue that SCAM is a safe, strongly typed, statically typed
language, since every variable contains a value of a known type (string).
The compiler does not allow anything other than a string to be assigned to a
parameter or variable.  There is no "casting" or conversion of strings to
types incompatible with strings (because there are only strings).  Memory
safety is guaranteed, there are no run-time type violations, and control
flow integrity is always preserved.

That said, we can also think of SCAM as hosting a rich set of *subordinate*
data types.  These are ranges or sets of values (types, if you will) that
serve a certain purpose and are used in a certain way.  For example, *some*
strings can represent (or "be") numbers.  Not all strings are numbers, but
all numbers are strings.  Similarly, some strings can represent vectors of
strings.  Not all strings are vectors, but all vectors are strings, so we
can think of vectors as a subset of the set of strings.  At the same time, a
vector can contain any number of any string values, so we can also think of
the set of vectors as the Kleene closure of the set of strings.

In terms of these subordinate types, SCAM is not statically typed at all.
SCAM is mostly oblivious to them.  It provides syntax for constructing some
of these types, functions for manipulating them, and it can pretty-print
them.  However, if you were to pass, say, a non-vector to the `append`
function, it will perform a deterministic string manipulation and happily
succeed whether or not the result makes any sense.

Some of these subordinate types are overlapping sets.  For example, `1` is
equivalent to `[1]` (and `[[1]]` and so on), but mostly they are disjoint.
For example, each data record can be distinguished from any other data
record, vector, or dictionary.  Non-empty vectors and dictionaries also
disjoint.  This allows SCAM to display values in a more meaningful way.


### Booleans

The empty string (written `""` or `nil`) is used to represent "false" in
control flow expressions.  Any non-nil value is considered "true"; the value
"1" is often used to represent true when no other value is an obvious
candidate.

    > (if nil 1 2)
    2
    > (if "false" 1 2)   ; not false...
    1


### Numbers

Numbers are written as one or more decimal digits, optionally preceded by a
`-` character, and optionally followed by a fractional portion and a
E-notation suffix.  The E-notation suffix consists of `e` or `E`, optionally
followed by `+` or `-`, followed by an integer.

There are no limits on the size of numbers in SCAM.  The `num` library
implements arbitrary precision decimal floating point arithmetic.

    > (word 2 "a b c")
    "b"
    > (+ 12 34)
    46
    > (^ -1.2e2000 7)
    -3.5831808e+14000


### Word Lists and Vectors

A word list (or simply "list") is a sequence of "words" separated by the
following two characters: space (` `, ASCII 32) and tab (`\t`, ASCII 9).  A
**word** is defined, in turn, as a contiguous sequence of non-separator
characters.  Any number of separators can appear before, between, or after
words.  Constructing a word list is as simple as concatenating words and
spaces.

Word lists are simple and often convenient, but somewhat specialized because
the elements they contain cannot include space or tab characters, and they
exclude the empty string as an element.

A vector is SCAM's general purpose data structure for storing a sequence of
values.  A vector contains an arbitrarily-long sequence of arbitrary
strings.  Of course, a vector is itself a string (as are all SCAM value), so
*bang-encoding* is employed.  Bang-encoding is a reversible transformation
that replaces space and tab characters with non-whitespace characters.
Bang-encoding preserves sort order, unless an element contains control
characters.

In normalized form, a vector consists of some number of bang-encoded
elements separated by a single space character.  Non-normalized forms (with
additional space or tab characters before, after, or between characters) are
also accepted by all vector manipulation functions.

Vector constructor syntax is provided for constructing vectors.  Enclose any
number of expressions in square brackets to construct a vector of that size.
Functions `first` and `nth` extract values from vectors.

For example:

    > (word 3 "a b c d")
    "c"
    > (nth 3 ["a" "" "c d"])
    "c d"

Here is an overview of ways to manipulate vectors, and the analogous
operations on word lists:

    +--------------------+------------------------+-----------------------------+
    | Operation          | Vectors                | Word Lists                  |
    +====================+========================+=============================+
    | Create             | `[A B C]`              | `(concat A " " B " " C)`    |
    +--------------------+------------------------+-----------------------------+
    | Get item N         | `(nth N VEC)`          | `(word N LIST)`             |
    +--------------------+------------------------+-----------------------------+
    | Get first item     | `(first VEC)`          | `(firstword LIST)`          |
    +--------------------+------------------------+-----------------------------+
    | Get last item      | `(last VEC)`           | `(lastword LIST)`           |
    +--------------------+------------------------+-----------------------------+
    | Add item to front  | `(cons ITEM VEC)`      | `(concat WORD " " LIST)`    |
    +--------------------+------------------------+-----------------------------+
    | Add item to back   | `(conj VEC ITEM)`      | `(concat LIST " " WORD)`    |
    +--------------------+------------------------+-----------------------------+
    | Append sequences   | `(append V1 V2...)`    | `(append LIST1 " " LIST2)`  |
    +--------------------+------------------------+-----------------------------+
    | Count items        | `(words VEC)`          | `(words LIST)`              |
    +--------------------+------------------------+-----------------------------+
    | Get range          | `(wordlist A B VEC)`   | `(wordlist A B LIST)`       |
    +--------------------+------------------------+-----------------------------+


### Dictionaries

A dictionary is an ordered list of pairs.  These can be used to represent
unordered associations of keys and values, filling the role of
"dictionaries" in Python, "hashes" in Perl, or "tables" in Lua, and so on.

The `core` library provides the following functions operate on pairs:

 - `(dict-get KEY DICT)` retrieves the VALUE element of the first pair
   matching KEY.

 - `(dict-set KEY VALUE DICT)` returns a new dictionary in which KEY is
   associated with VALUE, and no other pairs have a key equal to KEY.

 - `(dict-find KEY DICT)` returns the first pair matching KEY.

 - `(dict-key PAIR)` extracts the "key" element from PAIR.

 - `(dict-value PAIR)` extracts the "value" element from PAIR.

 - `(dict-compact DICT)` returns a new dictionary in which each key occurs
   only once, bound to the first value associated with it in DICT.

 - `(dict-collate DICT)` creates a new dictionary in which each key from
   DICT appears only once, bound to a vector of *all* its values in DICT.

 - `(dict-keys DICT)` returns a vector of all keys in DICT.


An empty dictionary, `{}`, is equivalent to `nil`.

Since dictionaries are word lists, the word-based functions -- `firstword`,
`word`, `wordlist`, `rest`, ... -- can be used to obtain pairs or sequences
of pairs.  Pairs or dictionaries can be combined using `append`.  Use
`foreach`, not `for`, to iterate over a dictionary or construct a
dictionary.

    > (word 2 {a: "A", b: "B", c: "C"})
    {b: "B"}
    > (append { a: nil }
    +         { b: (range 1 3) } )
    {a: "", b: [1 2 3]}
    > (dict-get "b" *1)
    [1 2 3]
    > (foreach pair { a: 17, b: 76 } (dict-value pair))
    [17 76]
    > (foreach n (range 4 6) {=n: (^ n 2)})
    {4: 16, 5: 25, 6: 36}
    > (dict-collate {a:" ", b:1, b:2})
    {a: [" "], b: [1 2]}


### Records

The `data` special form introduces a new "data type" (see the [above
discussion](#data-types)) that takes the form of a union of record types.
This is similar to the "algebraic data types" in some other languages.  Each
record can contain zero or more member values.

    > (data Shape
    +    (Square a)
    +    (Rect a b)
    +    (Triangle b h))

This example defines three constructors -- `Square`, `Rect`, and `Triangle`
-- that can be called like functions to construct a value of that type.

    > (define s (Rect 3 5))
    > s
    (Rect 3 5)

The `case` statement matches records and deconstructs them allowing us to
access their members in a type-safe manner.

    > (case s
    +   ((Square a) (+ a a))
    +   ((Rect a b) (+ a b)))
    8

The first parameter is the value to be matched, and the remainder of the
forms are `(PATTERN BODY)` pairs.  It tests these in order, and if a PATTERN
matches it will evaluate and return the corresponding BODY.  If no patterns
match, `nil` is returned.

A pattern can be a (CONSTRUCTOR-NAME ARG...) where each ARG is a symbol.
When the match is made, these symbols are bound to the corresponding members
of the record when the BODY is evaluated.

Another valid pattern is any symbol.  This kind of pattern always succeeds,
and the symbol will be bound to the record value while its corresponding
BODY is evaluated.

    > (define (area shape)
    +   (case shape
    +     ((Square a) (* a a))
    +     ((Rect a b) (* a b))
    +     ((Triangle b h) (/ (* b h) 2))
    +     (else 0)))
    > (area s)
    15
    > (area "purple")
    0
    > (area (Triangle 3 4))
    6

Constructors can be treated as first class functions:

    > (for ctor [Rect Triangle]
    +   (let ((s (ctor 3 4)))
    +     (printf "%q -> %s" s (area s))
    +     (area s)))
    (Rect 3 4) -> 12
    (Triangle 3 4) -> 6
    "12 6"

A slightly more interesting example:

    > (data NumOps (Add a b) (Sub a b) (Mul a b) (Div a b) (Exp a b))
    > (define (calc e)
    +   (case e
    +    ((Add a b) (+ (calc a) (calc b)))
    +    ((Sub a b) (- (calc a) (calc b)))
    +    ((Mul a b) (* (calc a) (calc b)))
    +    ((Div a b) (/ (calc a) (calc b)))
    +    ((Exp a b) (^ (calc a) (calc b)))
    +    (n n)))
    > (calc (Exp (Add 3 4) (Mul 5 6)))
    22539340290692258087863249

Note that the last pattern in the above `case` statement treats any value
not matching the previous patterns as a number.  Here, we could (and perhaps
should) have used a `(Num n)` constructor to distinguish numbers from
operations, but it is worth noting that record values will not be confused
with ordinary numbers.  They are also distinguished from vectors,
dictionaries, and other record types.  In fact, we could add a case for
`Triangle`:

    ...
    ((Triangle b h) (/ (* (calc b) (calc h)) 2))
    ...

... and then ask it to calculate `(calc (Add (Triangle 2 3) 4))`.

The type name that follows `data` has no significance except to distinguish
its record values from those of other record types.  It will not appear
elsewhere in your SCAM program.  For example, `(data A (Ctor a))` and `(data
B (Ctor a))` define two different constructors that have the same name
(`Ctor`) but produce different types of records.  When a constructor is
named in an expression, the meaning of the name depends on the lexical scope
in effect where it is referenced.  Here is an example that defines two
different `Orange` constructors, one a member of `data Fruit` and one a
member of `data Color`:

    > (data Fruit (Apple) (Orange))
    > (define (is-fruit fr)
    +   (case fr
    +     ((Orange) "yes")
    +     ((Apple) "yes"))
    +     (_ "no"))
    > (define o1 (Orange))
    > (is-fruit o1)
    "yes"
    > (data Color (Orange))  ; shadows the previous "Orange"
    > (is-fruit (Orange))
    "no"
    > (is-fruit o1)
    "yes"


### Functions

A *function* is a string that contains executable code. Functions are
invoked when a compound form is evaluated and the first item in the compound
form is function value.

    > (define (f x y)
    +   (concat x y))
    > (f "a" "b")
    "ab"

#### Arity Checking

When a function is called by its name, as in the `(f 1 2)` example above,
SCAM performs compile-time checking of the number of arguments being passed.

    > (f 1)
    at 1:2: "f" accepts 2 arguments, not 1
    (f 1)
     ^

This checking does not apply when the function value is obtained some less
direct manner, as in `((or f) 1 2)`.  The computed result remains the same.
Any arguments not supplied will take the value `nil`.

#### Optional Parameters

We can declare an optional parameter by prefixing its name with `?` in the
function definition or declaration:

    > (define (g a ?b) (or b a))
    > (g 1)
    1
    > (g 1 2)
    2
    > (g 1 2 3)
    at 1:2: "g" accepts 1 or 2 arguments, not 3
    (g 1 2 3)
     ^

Functions and macros can also deal with a variable number of arguments.
When the last parameter name is prefixed with `...`, that parameter will
capture in a vector zero the rest of the arguments passed to the function.
For example:

    > (define (f a b ...others)
    +   others)
    > (f 1 2 3 4 5 6)
    [3 4 5 6]

The size of the resulting vector is determined by the last non-nil argument
value.

    > (f 1 2 3 4 nil nil)
    [3 4]

"Rest" parameters (`...NAME`) may appear only as the last of the formal
parameter.  Optional `?NAME` parameters may not be followed by non-optional,
non-rest parameters.

## Variables

Variables are symbols that name already-computed values.  Variables can be
either global or local.

Global variables are defined by the `(define NAME ...)` and `(define (NAME
...ARGS) ...)` special forms.

Local variables include function parameters and names defined with `let`,
`let&`, for-expressions (`for`, `foreach`, `concat-for`, ...) and pattern
matching expressions.  Local variables are immutable. They are assigned a
value when initialized, and they cannot be assigned a different value.  The
visibility and lifetime of a local variable is limited to the expression in
which it is defined. See [`let`](#let).

Global variables are mutable and have unlimited lifetime.  They are visible to
other SCAM modules, which means that if two modules in your program declare
a global of the same name, they will be using the same variable.  For
readers comparing with other Lisp dialects, it is useful to note that
globals are equivalent to "dynamic" or "special" variables in the parlance
of Common Lisp. The following special forms deal with global variables:
[`define`](#define), [`declare`](#declare), [`let-global`](#let-global),
[`set`](#set).

Global variables occur in two "flavors": data variables and function
variables.  This flavor is established when the variable is defined or
declared.  Function variables can only store function values, and data
variables can store any value.  At the same time, calling functions stored
in function values is somewhat more efficient than calling functions stored
in data variables.

### Scope

SCAM has lexical scoping rules.  Variables names are valid expressions only
when the variables declaration is in scope.  Variables are introduced into
scope in one of the following ways:

 - A let expression -- `let`, `let&`, or `let-global` -- introduces names
   whose scope extends to the end of the let expression.

 - Each function parameter appearing in an argument list -- in a function
   definition or in a `lambda` expression -- introduces a name whose scope
   extends to the end of the function.

 - The `define` and `require` forms, when appearing in a **block** context,
   introduce names whose scope extends to the end of the block.

A block is a sequence of expressions that are evaluated in order.  The
values of these expressions are discarded except for the last expression,
which supplies the value for the entire block.  The sequence of top-level
expressions in a module are a block.  The sequence of expressions in a
function body constitute a block.  Other expressions such as the `begin`
special form contain blocks.  Lists of arguments passed to functions or
macros are *not* blocks.  One argument cannot introduce a variable name for
a subsequent expressions to use.


## Macros

Macros are user-defined syntax extensions.

There are two forms of macros: symbol macros and compound macros.

The syntax for defining a symbol macro is:

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

    (define `(NAME ARG...) BODY)

Macros are invoked just like functions:

    (NAME EXPR...)

Each invocation of the macro will be replaced by a `begin` block containing
the macro body.  Within the macro body, the macro argument names are bound
to the argument expressions.  Compound macros behave much like functions,
but macros cannot recurse, and when a macro is invoked, an expression passed
as an argument argument may be evaluated zero or more times, depending on
how many times the parameter ends up being evaluated within the macro body.
For example:

    > (define `(m a) (concat a a a) "done")
    > (m (print 1))
    1
    1
    1
    "done"

Defining a macro does not create a global function variable, but when the
macro name itself is evaluated, its value is an anonymous function.  Here is
an example in the REPL:

    > (define `(m a)
    +     (subst 2 9 a))
    > (m 123)
    193
    > (let ((f m))
    +   (f 123))
    193

The anonymous function behaves the same as the macro when invoked, only
except for the fact that functions evaluate their arguments exactly once
(while macros may evaluate them zero or more times).

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


## Special Forms

Special forms are expressions that have "special" handling -- they have
semantics that cannot be implemented in ordinary functions.  Special forms
are compound expressions, and the first element in the parenthesized list is
a symbol identifying the special form.  These symbols are not bound to
values.

Examples:

    (if a b c)

    (declare foo)

## Libraries

SCAM provides a "standard" set of modules that are bundled with the SCAM
compiler.  Programs will need to `require` the appropriate module before
using the functions exported by that module.  In the REPL environment, by
default all the standard modules are implicitly required.

See the [SCAM Libraries](libraries.md) document for full details.


## Debugging

### Call Site Tracing

The ["?" special form](libraries.md#-fn-args) allows a simple edit of the
source to enable or disable tracing at a particular call site.

### Run-time Tracing

Tracing can be activated at run-time in two different ways:

 1. In a SCAM program or in interactive mode, use the following:

     - `(tracing SPEC EXPR)` evaluates EXPR with tracing enabled.
     - `(trace SPEC)` turns on tracing for subsequent expressions.
     - `(untrace NAMES)` removes instrumentation from functions.

 2. Set the [`SCAM_TRACE`](#scam-trace) environment variable before running
    a SCAM program.  in order to trace execution that happens during the
    program's `main` function.

    `SCAM_TRACE` takes the same form as the `SPEC` argument to `trace`.

    Functions cannot be instrumented until after they have been defined, so
    `SCAM_TRACE` will activate tracing once before requiring the main module
    (chiefly so the `^load` function can be traced), and again after the
    main module is loaded, at which point all modules typically have been
    loaded.  If you want to trace execution that happens during module
    loading prior to `main` -- e.g. top-level expressions in a `-q.scm` test
    -- modify the program to call `(trace SPEC)` or `(tracing ...)`.

Refer to the [library documentation](libraries.md#tracing-spec-expr) for
details on these tracing functions and on the `SPEC` string format.

#### Tracing examples

To count all function invocations in a SCAM program:

    $ SCAM_TRACE='%:c' ./myprogram

Or:

    $ SCAM_TRACE='%:c' scam myprogram.scm

To show details for all calls into functions beginning with "foo-":

    $ SCAM_TRACE='foo-%' scam myprogram.scm

In the REPL:

    > (define (fib n)
    +    (if (> n 2)
    +       (+ (fib (- n 1)) (fib (- n 2)))
    +       (- n 1)))
    > (tracing "%" (fib 4))
    --> (fib "4")
     --> (fib "3")
      --> (fib "2")
      <-- fib: "1"
      --> (fib "1")
      <-- fib: "0"
     <-- fib: "1"
     --> (fib "2")
     <-- fib: "1"
    <-- fib: "2"
    2
    > (tracing "%:c" (fib 16))
    TRACE:     1973 fib
    610

To quantify hotspots in a program the `x` mode of tracing can be used with
`SCAM_TRACE`.  An example scenario would play out like this:

  1. Run `time SCAM_TRACE='func:x1' ./myprogram`.

  2. Run `time SCAM_TRACE='func:x11' ./myprogram`.

  3. Calculate (duration2 - duration1) / 10 to obtain the amount of time
     `func` ordinarily contributes to the program's execution.


### Profiling

You can use invocation counts (as described above) to look for potential
hot spots.

To measure the time spent in a particular function, you can use the
following approach:

 * Time execution of a use case.  For example:

       $ time ...command...

   The command can invoke your program via `scam program.scm ...args...`
   or it can invoke your program directly, assuming you have compiled it
   using `scam -o ...`.

 * Use an `x11` trace mode to multiply the time spent in an individual
   function, and run the same use case:

       $ SCAM_TRACE='function-name:x11' time ...command...

 * This second invocation should take longer to execute. Dividing the
  additional time by 10 will give the amount of time spent in that function
  during that use case.

## The SCAM Compiler

The SCAM Compiler is an executable program that can run SCAM programs and
compile SCAM programs to executable files, and it also supports an
interactive "REPL" mode.

The SCAM Compiler supports four major modes of operation:

1. Generate an executable from SCAM source.

   Usage: `scam -o EXE SOURCE`

   SCAM source file SOURCE will be compiled and linked with its dependencies
   to create and executable file EXE.

   When EXE is invoked, the SOURCE module will be loaded and then its `main`
   function (if there is one) will be called with one argument: a vector
   containing all of the command line arguments.

2. Execute a SCAM source file.

   Usage: `scam SOURCE [--] ARGS...`

   SCAM source file SOURCE will be compiled and immediately executed as in
   `scam -o`.  Command-line arguments not processed by `scam` as options
   will be passed to SOURCE's main function as arguments.

   The `--` option can be used to ensure that following words are delivered
   to the target program, and not interpreted as arguments to the SCAM
   Compiler itself.

3. Enter an interactive "REPL" mode.

   Usage: `scam` or `scam -i`

4. Execute an expression provided on the command line.

   Usage: `scam -e EXPR`

   Multiple `-e EXPR` options can appear on the command line.  When EXPR
   evaluates to `nil`, scam does not print any results.


### Cached Results

In order to support fast incremental rebuilds of modules, SCAM stores
intermediate compilation results in a directory called the **object
directory**.  The object directory is determined by one of the following
(with the earlier ones taking precedence):

  1. The value given by the `--obj-dir` command line option.
  2. If the `-o EXE` option is given, a directory named ".scam"
     within the directory containing EXE.
  3. The environment variable `SCAM_OBJDIR`.
  4. A directory named `.scam` (relative to the current directory).

SCAM uses hashes to determine the suitability of cached entries; not
modification times.  As a result, when modifying a source file and
re-compiling, you may find that the compilation finishes instantly, as if
the change were not recognized.  This can happen if the modification returns
the source file to some older state that had been previously compiled; in
that case, the compiler can quickly identify how to recreate the program.


## Hashbang

In UNIX-based systems, SCAM source files may be marked as executable files
and labeled with a hashbang (`#!`).  For example:

    #!/usr/bin/env scam --quiet --
    (define (main argv)
      (print "Hello, world!"))

The `--quiet` option ensures consistent behavior by suppressing the progress
messages (to stderr) that would otherwise appear when compilation occurs.

The `--` option ensures that remaining arguments are treated as the program
name and program arguments, and are not interpreted as options by the SCAM
compiler.
