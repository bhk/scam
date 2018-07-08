 SCAM Language Reference

## Index

 * [Overview](#overview)
 * [Syntax](#syntax)
 * [Data Types](#data-types)
   * [Booleans](#booleans)
   * [Numbers](#numbers)
   * [Vectors](#vectors)
   * [Dictionaries](#dictionaries)
   * [Records](#records)
   * [Functions](#functions)
 * [Variables](#variables)
 * [Macros](#macros)
 * [Special Forms](#special-forms)
 * [Intrinsics](#intrinsics)
 * [Libraries](#libraries)
 * [Command Line Syntax](#command-line-syntax)
 * [Debugging](#debugging)


## Overview

A SCAM program consists of a **main** module.  When the program is run, that
module will be loaded.  This module may, in turn, load other modules.  After
the main module finishes loading, if a "main" function has been defined, it
will be called with a vector that contains the command-line arguments.  The
value returned by `main` will be treated as the exit status for the program
(with `nil` being treated as `0`).  If `main` is not defined, the program
will exit with a `0` status.

SCAM modules (source files) contain sequences of **expressions**.  These
expressions are **evaluated** when the module is **loaded**.  Evaluation
computes a **value** described by the expression.  The following section,
[Syntax](#syntax), describes the different kinds of expressions in SCAM.  As
in other Lisp-family languages, everything in SCAM is an expression.
Control flow and variable "binding" and "assignment" operations are handled
in expressions.  Many expressions include sub-expressions that may be
evaluated in the course of evaluating the parent.

Expressions may occur in **block** or non-block contexts.  A block is a
sequence of expressions that are evaluated in order.  The values of these
expressions are discarded except for the last expression, which supplies the
value for the entire block.  The sequence of top-level expressions in a
module are a block.  Expressions such as the `begin` special form can
contain blocks.

Blocks are important in SCAM because expressions that occur in a block may
define or declare symbols that are visible to expressions that follow them
within the block.  These lexically scoped definitions do not require deeper
nesting.  This enables a programming style that flows down the page
vertically as new definitions are introduced, rather than down and to the
right.


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

### Numbers

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
initial `=`, and that symbol is evaulated to determine the key.  For
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
it as a Cartesian product of the set of strings.

In terms of these subordinate types, SCAM is not statically typed at all.
SCAM is mostly oblivious to them.  It provides syntax for constructing some
of these types, functions for manipulating them, and it can convert them
back to source code form.  However, if you were to pass, say, a non-vector
to the `append` function, it will perform a deterministic string
manipulation and happily succeed, even though the result may not be of any
use to you.

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
forms are `(PATTERN BODY)` pairs.  It tests these in order, and when the
first pattern matches the value it evaluates and returns BODY.  If no
patterns match, `nil` is returned.

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

Functions can also deal with a variable number of arguments.  When the last
parameter name is prefixed with `...`, that parameter will capture in a
vector zero the rest of the arguments passed to the function.  For example:

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

Rest parameters may not be used with macros.


## Variables

Variables are symbols that name already-computed values.  Variables can be
either local or global.

Local variables are created with `let` expressions, or as function
parameters in a `lambda` expression or function definition.  Local variables
are immutable. They are assigned a value when initialized, and they cannot
be assigned a different value.  The visibility and lifetime of a local
variable is limited to the expression in which it is defined. See
[`let`](#let).

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
but (1) they do not bind global variables to a function value, as function
definitions do, and (2) when invoked, their argument expressions may be
evaluated zero or more times.  When the value of a compound macro is
requested, an equivalent function definition results.  Here is an example in
the REPL:

    > (define `(m a)
    +     (subst 2 9 a))
    > (m 123)
    193
    > m
    "$(subst 2,9,$1)"

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

## Intrinsics

The following symbols are defined by the SCAM language.  They call into
three different categories:

 - Special forms
 - Manifest functions
 - Manifest macros

Manifest functions are like other functions in SCAM, except that they are
provided by the language itself.  Special forms, built-ins, and macros are
not functions, so they do not have values and cannot be passed to other
functions; they can only be invoked like a function.

### Control Flow

#### `(if COND THEN-EXPR [ELSE-EXPR])`

First, COND is evaluated.  If non-nil, THEN-EXPR will be evaluated and used
are the value for the `if` expression.  Otherwise, if ELSE-EXPR is present
it is evaluated and used, and if not `nil` is used.

#### `(cond (CONDITION BODY)... [(else BODY)] )`

`cond` expresses conditional execution of an arbitrary number of possible
blocks of code.

Example:

    (cond ((filter "foo" a)  "Found FOO")
          ((filter "bar" a)  "Found BAR")
          ((filter "baz" a)  (print "BAZ") "Found BAZ")
          (else              "Found NOTHING"))

#### `(and EXPR...)

Expressions in `EXPR...` are evaluated sequentially until a `nil` value is
encountered.  The value of the `and` expression is that of the last
sub-expression evaluated, or `nil` if no expressions were evaluated.

#### `(or EXPR...)

Expressions in `EXPR...` are evaluated sequentially until a non-`nil` value
is encountered.  The value of the `and` expression is that of the last
sub-expression evaluated, or `nil` if no expressions were evaluated.

#### `(error MESSAGE)`

Terminate execution of the program with a non-zero status code, writing
`MESSAGE` to stderr.

#### `(begin EXPR...)

Encloses a *block* of expressions.  A block is a sequence of expressions
that are evaluated in order.  The result of that last expression is
returned (or nil if no expressions are given).



### Declarations and Definitions

#### `(require MODULE &private?)`

The `require` special form provides access to functionality defined in other
modules.  `MODULE` is a literal string that names either a SCAM source file
or a standard SCAM module.  Symbols are exported by the module (those
declared "&public") will be are visible to all expressions that follow in
the current block.  At run-time, the required module will be loaded and
executed, unless it has already been required by the program.

The `&private` flag is intended for use by unit test modules.  When present,
private symbols will be imported in addition to `&public` ones, and the
qualification step will not be required.  (See "qualification", below.)

When `MODULE` is one of the [standard library names](#libraries), the
standard library will be supplied by the compiler.  Otherwise, `MODULE` is
names a SCAM source file.  If it is a relative path, it is treated as
relative to the directory containing the requiring file, or, if no such file
exists, the directories listed in `SCAM_LIBPATH` (colon-delimited) until a
matching file is found.

When `MODULE` identifies a source file, that source file will be compiled to
determine its exports before compilation can continue.  In turn, modules
required by `MODULE` will also have to be compiled in order to build
`MODULE`, and so on.

**Qualification:** Each module can be accompanied by a qualification test: a
module with the same name except for an added `-q` before the extension.
For example, `foo-q.scm` is the qualification test for `foo.scm`.  When a
module is required, its qualification test (if present) will be built and
executed before compilation of the requiring module continues.  If the
qualification test terminates with a non-zero exit code, it is considered a
test failure and compilation stops.  (Note that qualification test files
must use the `&private` flag when requiring the module they test in order to
avoid a dependency loop.)

**Object directory:** During compilation, SCAM writes intermediate build
results under a directory called the object directory, and on subsequent
compilations it will reuse those files if they remain valid, compiling and
testing modules only when necessary.  The object directory is determined as
follows:

  - The object directory defaults to `./.scam`.
  - If `scam -o EXE SRC` is invoked and `EXE` is *not* in the current
    working directory, the object directory will be set to the directory
    containing `EXE`.
  - If the option `--obj-dir=DIR` is given, it will override the above two
    possibilities.

#### `define`

    (define NAME FLAG... VALUE)            ; global data variable
    (define (NAME ARG...) FLAG... BODY)    ; global function variable
    (define `NAME EXPR)                    ; symbol macro
    (define `(NAME ARG...) FLAG... BODY)   ; compound macro

The `define` special form adds a name to the environment and associates
it with a definition.  The defined name will be visible to subsequent
expressions in the same block, and the definition supersedes any
previous definitions associated with the same name.

The `&public` flag may be included in `FLAG...`.  This indicates that the
symbol should be visible outside of the file in which it is declared.


#### `declare`

    (declare NAME FLAG...)             ; global data variable
    (declare (NAME ARG...) FLAG...)    ; global function variable

The `declare` special form declares a global variable without assigning
a value.  This is usually used to access non-SCAM functions, or when
mutually recursive functions are defined.


#### `(defmacro (NAME ARGNAME) BODY)`

`defmacro` declares an *executable macro*.  An executable macro is a
function that transforms syntax.  It takes one argument, a form, and returns
a different form.

#### `(let ( (NAME VALUE)... ) BODY)`

The `let` special form assigns names to values.  The VALUE expressions are
evaluated, in order.  Then BODY (a block of expressions) is evaluated in an
environment in which each NAME is bound to its corresponding value.  The
`let` expression returns the value of the last form in BODY.

`let` is implemented in terms of `lambda` in this manner:

    ((lambda (NAME...) BODY) (VALUE...))

#### `(let& ( (NAME EXPR)... ) BODY)`

`let&` is a "lazy" let.  It binds the names to symbol macros instead of
local variables.  It also differs from `let` in that each expression is
evaluated in the context of the previous bindings -- more like Scheme's
`let*`.  It is equivalent to the following:

    (begin
      (define `NAME EXPR)...
      BODY)

Since `let&` constructs symbol macros, each bound expression is not
always evaluated exactly once, as with `let`.  Instead, each expression
is evaluated once each time its associated name is evaluated within
`BODY` -- perhaps zero times, perhaps many more.

Aside from the potential for multiple re-evaluations of expressions, `let&`
generally has lower overhead than `let`, since it does not involve an
anonymous function call (as does `let`).

#### `(let-global ( (NAME VALUE)... ) BODY)`

This form modifies the value of some number of global variables *during
the execution of BODY*. Afterwards, the original values are restored.

This expression evaluates to the value of the last expression in BODY.

#### `(set NAME VALUE RETVAL)`

The `set` special form assigns a value to a previously declared global
variable.

NAME is given as a symbol, not a string. For example:

    (set var 12 nil)

The `set` expression returns `RETVAL` (or "" if `RETVAL` is not provided).


### Constructing and Using Functions

#### `(lambda (ARG...) BODY)`

A `lambda` expression evaluates to a function value.

`ARG...` is zero or more symbols that name the formal arguments.

`BODY` is a block of one or more expressions (see [`begin`](#begin) )
that will be executed when the function is called. The initial environment
of `BODY` contains bindings for the arguments names to the values passed
to the function.

#### `(apply LAMBDA VEC)`

Call LAMBDA, passing as arguments the members of the vector VEC.

Example:

    (apply nth [3 "a b c d"])    ;; --> c



### Generic String Manipulation

#### `(concat VALUE...)`

This special form concatenates all of its arguments.

#### `(subst FROM TO {FROM TO}... VALUE)`

This special form replaces substrings with replacement strings within the
given VALUE.  For example:

    > (subst 2 3 1 2 12)
    23

#### `(findstring SUB STR)`

If SUB occurs within STR, return SUB.  Otherwise return the empty string.



### Word List and Vector Manipulation

#### `(foreach VAR LIST BODY)`

The `foreach` special form iterates over a list, evaluates BODY (a sequence
of expressions) once for each word, and constructs a new word list from the
results of each evaluation.

Each word is bound to the name `VAR` while `BODY` is evaluated.

    > (foreach x "1 2 3" (1+ x))
    "2 3 4"

#### `(for VAR VECTOR BODY)`

`for` iterates over items in a vector, evaluating BODY for with VAR bound to
an item, constructing a new vector with the results of BODY. Example:

    > (for x [[1 2] [3 4]]
    +     (reverse x))
    [[2 1] [4 3]]

#### `(append-for VAR VECTOR BODY)`

`append-for` is similar to `for` but it appends together all of the (vector)
values of BODY.  This is functionally similar to what is called `concat-map`
in some other languages.

    > (append-for x [[1 2] [3 4]]
    +     x)
    "1 2 3 4"
    > (append-for x [7 1 5 3]
    +    (if (> x 3) [x]))
    "7 5"

#### `(concat-for VAR VECTOR DELIM BODY)`

`concat-for` is similar to `for` but it concatenates the values of BODY.

    > (concat-for x [1 2 3] ";" (wordlist 1 x "a b c"))
    "a;a b;a b c"

#### `(demote VALUE)`

`demote` encodes any value as a word so that it may be embedded in a word
list.  It is used internally to construct vectors.

#### `(promote VALUE)`

`promote` reverses the encoding done by `demote`.

#### `(nth INDEX VEC)`

Returns the value stored at index INDEX (1-based) in vector VEC.

#### `(vector A B C ...)`

This constructs a vector.  It is equivalent to `[A B C ...]`.

### Other List Functions

The following functions are equivalent to the GNU Make builtins by the same
name: `addprefix`, `addsuffix`, `basename`, `dir`, `filter`, `filter-out`,
`firstword`, `join`, `lastword`, `notdir`, `patsubst`, `sort`, `suffix`,
`word`, `word`, `wordlist`, `words`.

### Others

#### `(print VALUE...)`

Concatenate all values and write them to stdout.

#### `(? FUNCNAME ARG...)`

This performs call-site tracing of `FUNCNAME`, described below in the
[Debugging](#debugging) section.

#### `(at-exit FUNC)`

Add FUNC to a list of functions that will be run when the program exits.
Functions will be run in the reverse of the order that they were registered.

#### `(current-env)`

This special form evaluates to the data structure that describes the lexical
environment at the point where it is invoked.  (See `gen.scm` for details of
this structure.)

#### `(current-file-line)`

This special form evaluates to the file name and line number of where it was
invoked, in this format:  `FILENAME:LINE`


## Libraries

SCAM provides a "standard" set of modules that are available to SCAM
programs.  Programs will need to `require` the appropriate module before
using the functions exported by that module.  In the REPL environment, by
default all the standard modules are implicitly required.  This is a list of
the standard libraries:

 - `core`: generic, commonly-used functions
 - `num`: an arbitrary-precision decimal floating point numeric library
 - `io`: generic I/O functions
 - `string`: string manipulation functions
 - `getopts`: command line option processing
 - `compile`: functions for compiling SCAM sources
 - `trace`: functions for tracing, debugging, and profiling
 - `utf8`: functions for encoding and decoding UTF-8

For documentation, refer to the corresponding `.scm` files in the SCAM
project.  (Look for `&public` functions.)


## Command Line Syntax

The `scam` command support four major modes of operation:

1. Generate an executable from SCAM source.

   Usage: `scam -o EXE SOURCE`

   SCAM source file SOURCE will be compiled and linked with its dependencies
   to create and executable file EXE.

   When EXE is invoked, the SOURCE module will be loaded and then its `main`
   function (if there is one) will be called with one argument: a vector
   containing all of the command line arguments.

2. Execute a SCAM source file.

   Usage: `scam -x SOURCE ARGS...`

   SCAM source file SOURCE will be compiled and immediately executed as in
   `scam -o`.  Command-line arguments not processed by `scam` as options
   will be passed to SOURCE's main function as arguments.  The `--` option
   can be used to ensure that following words are delivered to the target
   program.

3. Enter an interactive "REPL" mode.

   Usage: `scam` or `scam -i`

4. Execute an expression provided on the command line.

   Usage: `scam -e EXPR`

   Multiple `-e EXPR` options can appear on the command line.  When EXPR
   evaluates to `nil`, scam does not print any results.


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

Tracing can be activated at run-time in two different ways:

 1. In a SCAM program or in interactive mode, use the following:

     - `(trace SPEC)` instruments functions immediately.
     - `(untrace NAMES)` removes instrumentation from functions.
     - `(tracing SPEC EXPR)` instruments functions only during
       evaluation of EXPR.

    SPEC is a string that specifies the kind of tracing to be done, as
    described below.

 2. Set the `SCAM_TRACE` environment variable before running a SCAM program.
    Its value takes the same form as the SPEC argument to `trace`.

    Functions cannot be instrumented until after they have been defined, so
    `SCAM_TRACE` will activate tracing once before requiring the main module
    (chiefly so the `^load` function can be traced), and again after the
    main module is loaded, at which point all modules typically have been
    loaded.  If you want to trace execution that happens during module
    loading -- e.g. top-level expressions in a `-q.scm` test -- use `trace`
    or `tracing`.

The text string used to specify tracing is, in its simplest form, a list of
function names.  Further, these may be suffixed with a `:` and then an
"mode" to select alternative instrumentation.  Possible modes include:

 - `t` : Print the function name and arguments when it is called and its
         name and return value on exit.  This is the default mode.

 - `f` : Print just the function name on entry and exit.

 - `c` : Count the number of times that the function is invoked.  Function
         counts will be written to stdout when tracing is removed.  This can
         occur when `(tracing ...)` completes, or when `(untrace ...)` is
         called, or after `main` returns otherwise.

 - `x<N>` : Evaluate the function body N times each time the function is
         invoked.  <N> must be a positive number or the empty string (which
         is treated as 11).

 - `-` : Exclude the function(s) from instrumentation.  Any functions
         matched by this entry will be skipped even when they match other
         entries in the specification string.  This does not depend on the
         ordering of entries.  For example, `(trace "a% %z:-")` will
         instrument all functions whose names begin with `a` except for
         those whose names end in `z`.

In place of a function name a pattern may be provided to match multiple
functions.  In a pattern, the `%` character is a wildcard that will match
any sequence of characters.  Some caution must be exercised in general with
tracing, especially when using wildcards: Do not instrument any function
while it is currently running.  SCAM prevents this from happening when you
use `SCAM_TRACE`, or when you call `trace` or `tracing` at the top level of
a source file, the REPL, or your `main` function.  However, if while nested
in one or more other user-defined functions, you trace any of those
functions, then undefined behavior will result.

The intent of `x` instrumentation is to cause the function to consume more
time by a factor of N (for profiling purposes).  If your code is purely
functional, or at least limits its side effects to idempotent operations,
repetition of expressions should not alter the behavior of the program.
This can be used to identify and quantify hotspots in a program.  For
example:

  1. Run `time SCAM_TRACE='func:x1' ./myprogram`.

  2. Run `time SCAM_TRACE='func:x11' ./myprogram`.

  3. Calculate (duration2 - duration1) / 10.  This gives the amount of time
     `func` ordinarily contributes to the program's execution.


#### Tracing examples

To count all function invocations in a SCAM program:

    $ SCAM_TRACE='%:c' ./myprogram

Or:

    $ SCAM_TRACE='%:c' scam -x myprogram.scm

To show details for all calls into functions beginning with "foo-":

    $ SCAM_TRACE='foo-%' scam -x myprogram.scm

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

 * Use an `x11` trace mode to multiply the time spent in an individual
   function, and run the same use case:

       $ SCAM_TRACE='function-name:x11' time ...command...

 * This second invocation should take longer to execute. Dividing the
  additional time by 10 will give the amount of time spent in that function
  during that use case.
