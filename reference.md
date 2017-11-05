# SCAM Language Reference

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
 * [Manifest Symbols](#manifest-symbols)
 * [Make Features](#make-features)
 * [Debugging](#debugging)
 * [Libraries](#libraries)


## Overview

A SCAM program consists of *expressions*. Some examples of expressions are
constants, variable definitions, and function calls.

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
values of these top-level expressions are discarded.

## Syntax


### String Literals

String literals are enclosed in double quote characters (`"`).

The sequences `\n` and `\t` represent newline and tab. Use a backslash
`\` to escape another backslash or a double quote. String literals
*may* contain embedded (unquoted) newlines (except in the REPL).

    "abc"
    "Hello, World!\nAnd one more line"

String literals evaluate to the content of the string.


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
following: ``()[]{},;:%$\`'"``.

    a
    this-is-a-long-symbol-with/perhaps/*unusual*-characters!

Symbols can identify:

- [variables](#variables) (including functions)
- [macros](#macros)
- [special forms](#special-forms)
- [record constructors](#records)
- [GNU Make built-in functions](#built-ins)

In a few cases they designate keywords, like `define` or `let`.


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

Vector constructors are written as a pair of sqaure brackets enclosing zero
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

Keys can be specified as a symbol, a symbol prefixed with `=`, or any
expression.  A symbol not beginning with `=` is treated literally; the name
of the symbol becomes the key.  When a symbol is prefixed with `=` the value
of the symbol will be used as the key.  For example, `{ a: 1 }` is
equivalent to `{ "a": 1 }`, and `{ =a: 1 }` is equivalent to `{ (if 1 a): 1
}`.

Dictionary values and functions that operate on them are [described
below](#dictionaries).


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
is as a Cartesian product of the set of strings.

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

The empty string (written `""` or `nil`) is used to represent "false" when
functions, macros, or special forms operate on or return boolean values.
Any non-nil value is considered "true"; the value "1" is often used to
represent true.

    > (if nil 1 2)
    2
    > (if "false" 1 2)   ; not false...
    1

Make built-ins `and` and `or` interpret strings in this manner.  `or` returns
the first non-nil argument, and `and` returns the last argument if all
arguments are true.

SCAM's core library provides `not` and `xor` logical operators.

   > (xor "a" "b")
   > (xor "a" nil)
   a
   > (not "x")
   > (not nil)
   1


### Numbers

Numbers are typically represented as a string of decimal digits. GNU Make
built-in functions and the the `num` module supplied with SCAM expect numbers
in this format.

    > (word 2 "a b c")
    "b"
    > (+ 12 34)
    46

The `numeric?` function test whether a string is a number, and `word-index?`
tests whether a stirng is safe to pass as a word index to Make built-in
functions (a positive integer represented with only digits -- no exponent or
decimal).

    > (numeric? "1e20")
    1e20
    > (if (word-index? "1e20") "yes" "no")
    no
    > (if (word-index? 0) "yes" "no")
    no
    > (if (word-index? 1) "yes" "no")
    yes


### Vectors

A vector is a sequence of *word-encoded* values delimited by space
characters.  Word-encoding is a reversible transformation that replaces
space and tab characters with non-whitespace characters.  By encoding
vectors in this manner we can employ Make built-in functions that provide
random access into a word list, such as `word`, `wordlist`, and `lastword`.

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

#### Word-Encoding Details

The following details are implementation-specific.  This is not to say that
these details are anything to be afraid of -- after all, there is only one
implementation of SCAM -- but just that this knowledge is not necessary for
writing SCAM programs.  It may be helpful, however, when deciphering values
while debugging.

SCAM's current implementation of word-encoding might be called bang-encoding:

    `!1` encodes `!`
    `!0` encodes ` `
    `!+` encodes TAB
    `!.` represents the empty string

One nice property of this encoding is that lexicographical sorting order is
preserved for all printable characters and TAB.  This is because ` ` and `!`
are the first two printable characters (code points 0x20 and 0x21) in ASCII
and ASCII-derived encodings (such as Unicode).

Note that in a word-encoded value -- and therefore, in a vector -- `!` is
always followed by one of four characters.  Other two-character sequences
can be combined with word-encoded values to designate separators or markers.
In dictionaries, `!=` delimits word-encoded keys from values.


### Dictionaries

Dictionaries are associative arrays, similar to "a-lists" in Lisp,
"dictionaries" in Python, "hashes" in Perl, "tables" in Lua, and so on.
Each dictionary is a list of key-value pairs.

A number of functions are provided for working with dictionaries:

 - `(dict-get KEY DICT)` retrieves the VALUE element of the first pair
   matching KEY.

 - `(dict-find KEY DICT)` returns the first pair matching KEY.

 - `(dict-key PAIR)` extract the "key" element from PAIR.

 - `(dict-value PAIR)` extract the "value" element from PAIR.

 - `(dict-compact DICT)` removes pairs from DICT that are preceded by
   another pair sharing the same KEY.

 - `(dict-keys DICT)` returns a vector of all keys in DICT.

An empty dictionary, `{}`, is equivalent to `nil`.

A dictionary with one item is equivalent to a single pair.  Pairs can be
combined using `append`.

    > (append { a: nil }
    +         { b: (range 1 3) } )
    {a: "", b: [1 2 3]}
    > (dict-get "b" *1)
    [1 2 3]


### Records

The `data` special form introduces a new "data type" (see the [above
discussion](#data-types)) that takes the form of a union of record types.
This is similar to the "algebraic data types" in some other languages.  Each
record can contain zero or more values.

    > (data Shape
    +    (Square a)
    +    (Rect a b)
    +    (Triangle b h))

This example defines three constructors -- `Square`, `Rect`, and `Triangle`
-- that can be called like functions to construct a value of that type.

    > (define s (Rect 3 5))
    > s
    (Rect 3 5)

The interactive REPL displays values using `format`, which recognizes record
types and displays them readably.  We can use `print` to show the "raw"
string value, but we would normally never have to see this.

    > (print s)
    !:Shape1 3 5

The `case` statement performs pattern matching.  It allows us to use
construction expressions on the receiving end of an assignment.

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

When writing entirely in SCAM, you will not need to examine or otherwise
deal with the compiled form of functions.  For the sake of completeness,
however, we now describe executable code and how it compiles to GNU make.
The value of `f` as described above is the string `"$1$2"`.  This is GNU
Make syntax for concatenating the first and second arguments.  The above
definition of `f` will compile to the following line in a SCAM-generated
executable:

    f = $1$2

Typically, functions are stored in global variables, as in the example
above, but functions are first class values in SCAM, so they can be passed
to functions, returned from functions, and constructed without assigning
them a name:

    > f
    "$1$2"
    > (lambda (a b) (concat a b))
    "$1$2"

Given these facts -- functions are first class values, and function values
are in GNU Make syntax -- the logical (if somewhat surprising) implication
is the following behavior:

    > ("$1$2" "a" "b")
    "ab"

#### Arity Checking

When a function is called by its name, as in the `(f 1 2)` example above,
SCAM performs compile-time checking of the number of arguments being passed.

    > (f 1)
    line 1: "f" accepts 2 arguments, not 1
    at: (*f* 1)

This checking does not apply when the function value is obtained some other
way, as in `((or f) 1 2)`.  The computed result remains the same.  Any
arguments not supplied will take the value `nil`.

#### Optional Parameters

In order to declare an optional parameter, prefix its name with `?` in the
function definition or declaration:

    > (define (g a ?b) (or b a))
    > (g 1)
    1
    > (g 1 2)
    2
    > (g 1 2 3)
    line 1: "g" accepts 1 or 2 arguments, not 3
    at: (*g* 1 2 3)

In order to capture an arbitrary number of arguments in one variable, prefix
the name of the last parameter with `...`.  The variable will evaluate to a
vector containing the rest of the arguments passed to the function.  For
example:

    > (define (f a b ...others)
    +   others)
    > (f 1 2 3 4 5 6)
    [3 4 5 6]

The size of the resulting vector is determined by the last non-nil argument
value.

    > (f 1 2 3 4 nil nil)
    [3 4]

"Rest" parameters (`...NAME`) may appear only as the last of the formal
parameter.  Optional parameters may not be followed by other optional
parameters or a rest parameter.

Rest parameters may not be used with macros or inline functions.

#### Function Variables

When a function value is defined

Since functions are values, `(f a b)` is equivalent to `(let ((g f)) (g a
b))`.


### Syntax Trees

The result of parsing an expression is a SCAM syntax tree. Each node in the
syntax tree is called a "form".

In ordinary Lisps, various syntactic constructs (symbol, string, number,
list) correspond to distinct primitive types. In SCAM, there is only one
primitive type (string), so the different syntactic constructs are
represented as vectors.

Forms are not ordinarily manipulated by SCAM code, except in the compiler
source code. Refer to `parse.scm` for more information on forms.

## Variables

Variables are symbols that evaluate to values.

There are three kids of variables in SCAM:

 * local
 * global
 * automatic

Local variables are created with `let` expressions, or as function
parameters in a `lambda` expression or function definition. The visibility
of a local variable is limited to the expression in which it is defined, and
the lifetime of the local variable is limited to the execution of that
expression. Local variables are immutable. They are assigned a value when
when their containing expression is evaluated, and they cannot be assigned a
different value during the evaluation of that expression.  See [`let`](#let).

Global variables are ordinary GNU Make variables. Their lifetime is
unlimited, and they are visible to other SCAM modules and Makefiles (SCAM
globals are Make variables). However, to reference a global variable with a
symbol in SCAM, you must first introduce the binding using the `declare` or
`define` special forms.

For readers comparing with other Lisp dialects, it is useful to note that
globals are functionally equivalent to "dynamic" or "special" variables in
the parlance of Common Lisp. The following special forms deal with global
variables: [`define`](#define), [`declare`](#declare),
[`let-global`](#let-global), [`set`](#set), [`set-global`](#set-global),
[`set-rglobal`](#set-rglobal).

Automatic variables are the variables created by GNU Make's `foreach`
built-in or SCAM's `for` macro. Automatic variables are immutable (as are
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


### Variable Flavor

One interesting aspect of GNU Make is the notion of flavor: global variables
can be "recursive" or "simple".  See the [GNU Make
manual](https://www.gnu.org/software/make/manual/html_node/Flavors.html#Flavors)
for more details.

When programming entirely in SCAM one does not need to be aware of variable
flavor.  In SCAM, the distinction between "expand X" and "return the value
of X" is explicitly expressed in syntax.  The expression `X` means "the
value of X", and will not expand X (even if X is a recursive variable).  The
expression `(X)` will expand X (even if X is a simple variable).

In order to make interoperability with raw Make code more convenient, SCAM
variables that are defined *as* functions will be stored in Make recursive
variables, and other values will be stored in simple variables.

    > (define (f) o1)
    > (flavor "f")
    "recursive"
    > (define x 1)
    > (flavor "x")
    "simple"


### Namespaces

Since Make supports only one variable namespace, combining different
programs or libraries can result in errors when they contain variable names
that conflict.  In order to avoid naming conflicts between SCAM itself and
the code it is compiling, the SCAM compiler is built with a different
namespace: a prefix is added to each global variable and function.  A
namespace-prefixed name is called a "global" name.  The global names for
functions obtained from a bundled library may differ from the name seen in
SCAM source.  The following language features expose namespace
functionality:

 - `(global-name SYMBOL)` evaluates to the global name for SYMBOL.  You can
   use this to obtain the string value of a name, suitable for passing to
   (call NAME,...), (origin NAME,...), etc.

 - The `&global` flag can be used with a `declare` or `define` expression to
   avoid namespace prefixing.  In this case, the global name of the symbol
   will be equal to the local name.

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

## Manifest Symbols

The following symbols are defined by the SCAM language.  They call into
three different categories:

 - Special forms
 - Manifest functions
 - Manifest macros

Manifest functions are like other functions in SCAM, except that they are
provided by the language itself.  Special forms, built-ins, and macros are
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

    (lambda (ARG...) BODY)

A `lambda` expression evaluates to a function value.

`ARG...` is zero or more symbols that name the formal arguments.

`BODY` is a block of one or more expressions (see [`begin`](#begin) )
that will be executed when the function is called. The initial environment
of `BODY` contains bindings for the arguments names to the values passed
to the function.

### `require`

[Special form]

    (require MODULE &private?)

The `require` form provides access to functionality defined in other
modules.  `MODULE` identifies either a SCAM source file, minus its `.scm`
extension, relative to the directory containing the requiring file) or a
bundled module.

Ordinarily, only the `&public` declarations and definitions in MODULE will
be visible in the requiring file.  The `&private` flag, when present changes
this behavior, and makes every symbol visible in MODULE visible in the
requiring module -- just as if the source text of MODULE were included in
the requiring module.

`MODULE` must be a literal string value, because `require` is processed at
build time and compile time in addition to run time.

When a `require` directive is executed, the required module will be loaded
and executed, unless it has already been required.

**Dependency Resolution:**

When compiling with the `scam -o EXE SOURCE` command, SCAM first discovers
dependencies by scanning source files for `require` directives.  For each
required module, SCAM looks for a file to satisfy the dependency, in the
following order:

 1. Source file: A source file name is constructed by appending the `.scm`
    suffix to the module name and treating that as a path relative to the
    requiring file.

 2. Object file: An object file is constructed, just as for a source file,
    but using the `.min` suffix instead of `.scm`.

 3. Bundle: An object file that has been bundled into the compiler itself
    can be used to satisfy dependencies.  A matching bundle is one that has
    the same module name (directory and suffix portions are ignored).

SCAM must first compile each *required* module before it can compile the
*requiring* module.  This is because the required module may define macros,
functions, and variables that contribute to the environment of the requiring
module and affect the way subsequent expressions are compiled.

If a source file is found, SCAM will attempt to compile that file before
compiling the requiring file.  The compilation step will output an object
file (named `.min`) in the "object directory", which is the directory that
contains the target executable.

Outside of the context of a `scam -o` command, such as during `scam -e` or
`scam -x`, dependency scanning is not performed.  Dependencies must be
satisfied by bundled modules.

### `use`

[Special form]

    (use MODULE)

The `use` form specifies a module that defines "executable macros".
Executable macros are functions written in scam that define syntax
extensions.  When such a macro is encountered in the "using" source file,
the executable macro is given the syntax tree as an argument, and it has the
opportunity to transform it.

`MODULE` must be a literal string value, because `require` is processed at
build time and compile time in addition to run time.

Dependencies are discovered just as with the `require` directive.

Unlike the `require` directive, `use` does not introduce any run-time
dependencies.  The specified module is instead loaded by the compiler, at
compile-time.

### `let`

[Special form]

    (let ( (NAME VALUE)... ) BODY)

The `let` special form assigns values to a number of local variables.
The VALUE expressions are evaluated, in order.  Then BODY (a block of
expressions) is evaluated in an environment in which each NAME is bound
to its corresponding value.  The `let` expression returns the value of
the last form in BODY.

`let` is implemented in terms of `lambda` in this manner:

    ((lambda (NAME...) BODY) (VALUE...))

### `let&`

[Special form]

    (let& ( (NAME EXPR)... ) BODY)

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

Aside from the potential for multiple re-evaluations of expressions,
`let&` generally has lower overhead than `let`, since it does not involve
a Make function call (as does `let`).

### `define`

[Special form]

    (define NAME FLAG... VALUE)             ; global variable
    (define (NAME ARG...) FLAG... BODY)     ; global function
    (define `NAME EXPR)                     ; symbol macro
    (define `(NAME ARG...) FLAG... BODY)    ; compound macro

The `define` special form adds a name to the environment and associates
it with a definition.  The defined name will be visible to subsequent
expressions in the same block, and the definition supersedes any
previous definitions associated with the same name.

`FLAGS` consists of zero or more of the symbols `&public` and `&inline`.

  * `&public` indicates that the symbol should be visible outside of the
    file in which it is declared.

  * `&inline` indicates that the function is an inline function.  This flag
    can be used only with `define` (not with `declare`) and only for
    function definitions.

    An inline function definition defines both a function variable and a
    macro associated with the same symbol.  When the name is used in a
    function call expression, the macro will be expanded in place.  Unlike a
    compound macro, however, an inline function can be recursive (the first
    invocation will be as a macro, recursive invocations will be via the
    function).

    Beware that macro invocations (and inline function invocations) can
    differ semantically from ordinary function invocations in that arguments
    may be evaluated more than once or not at all.

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

An inline definition being used as a function and a macro:

    > (define (f a) &inline (concat "!" a))
    > f
    "!$1"
    > (lambda (x) (f x x))
    "!$1"                    ;; versus: "$(call f,$1)" for non-inline

### `declare`

[Special form]

    (declare NAME FLAG...)             ; declare global variable
    (declare (NAME ARG...) FLAG...)    ; declare global function variable

The `declare` special form declares a global variable without assigning
a value.  This is usually used to access non-SCAM functions, or when
mutually recursive functions are defined.

In GNU Make terms, these forms do not actually *create* a variable since
they perform no assignment. They do however cause SCAM to *assume* a
flavor of "simple" (in the first case) or "recursive" (in the function
form). This assumption will affect subsequent references and assignments
performed within SCAM.

### `defmacro`

[Special form]

    (defmacro (NAME ARGNAME) BODY)

`defmacro` declares an *executable macro*.  An executable macro is a
function that transforms syntax.  It takes one argument, a form, and returns
a different form.


### `set`

[Special form]

    (set NAME VALUE RETVAL)

The `set` special form assigns a value to a previously declared global
variable.

NAME is given as a symbol, not a string. For example:

    (set var 12 nil)

The `set` expression returns `RETVAL` (or "" if `RETVAL` is not provided).


### `let-global`

[Special form]

    (let-global ( (NAME VALUE)... ) BODY)

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

Note: using an empty string for the variable name will cause Make to exit
with an error.

### `set-rglobal`

[Manifest function]

    (set-rglobal NAME-EXPR VALUE)

This is equivalent to [`set-global`](#set-global), except that is executes
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
given VALUE.  It is equivalent to the GNU Make built-in `subst` except that
it accepts multiple pairs of replacement strings.  For example:

    > (subst 2 3 1 2 12)
    23
    > (lambda () (subst 2 3 1 2 12))
    "$(subst 1,2,$(subst 2,3,12))"


### `foreach`

[Special form]

    (foreach VAR LIST BODY)

The `foreach` special form iterates over a list, evaluates BODY (a sequence
of expressions) once for each word, and constructs a new word list from the
results of each evaluation.

Each word is bound to the name `VAR` while `BODY` is evaluated.

    > (foreach x "1 2 3" (1+ x))
    "2 3 4"

### `for`

[Special form]

    (for VAR VECTOR BODY)

`for` iterates over items in a vector, evaluating BODY for with VAR bound to
an item, constructing a new vector with the results of BODY. Example:

    > (for x [[1 2] [3 4]]
    +     (reverse x))
    [[2 1] [4 3]]


### `append-for`

[Special form]

    (append-for VAR VECTOR BODY)

`append-for` is similar to `for` but it appends together all of the (vector)
values of BODY.  This is functionally similar to what is called `concat-map`
in some other languages.

    > (append-for x [[1 2] [3 4]]
    +     x)
    "1 2 3 4"
    > (append-for x [7 1 5 3]
    +    (if (> x 3) [x]))
    "7 5"


### `concat-for`

[Special form]

    (concat-for VAR VECTOR DELIM BODY)

`concat-for` is similar to `for` but it concatenates the values of BODY.

    > (concat-for x [1 2 3] ";" (wordlist 1 x "a b c"))
    "a;a b;a b c"


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


### `*file*`

[Manifest global]

This symbol evaluates to the file currently being loaded via `require`.
This is evaluated at run-time, not compile-time, so it does not necessarily
return the name of the file in which it appears.

### Built-Ins

Every Make [built-in
function](http://www.gnu.org/software/make/manual/make.html#Functions) is
available from SCAM as a special form. For example:

    > (word 2 "a b c")
    "b"
    > (addsuffix ".c" "x")
    "x.c"

When the value of a built-in is requested, an equivalent function value is
provided:

    > shell
    "$(shell $1)"

Built-ins may differ from ordinary functions in the way arguments are
evaluated. Ordinarily, all arguments are evaluated in order *before* a
function is called. In the case of `if`, `and`, `or`, and `foreach`,
however, arguments may not be evaluated at all, and in the case of
`foreach`, they may be evaluated more than once.

    > (if "" (error "unexpected") 1)
    1

Some built-ins accept variable *names*. These built-ins -- `value` and `call`
-- are potentially confusing because they cross layers of abstraction. Be
aware that they use string values to refer to the variables, whereas in SCAM
you ordinarily reference variables by symbols. Using an unquoted symbol
name can produce unexpected results. The following example demonstrates
`value` and `call`:

    > (define (g x) (concat x x))
    > (define name-of-g "g")
    > (value name-of-g)    ; NOT (value g)
    "$1$1"
    > (call name-of-g "x") ; NOT (call g "x")
    "xx"
    > (define f name-of-g)
    > (value f)
    "$1$1"
    > (call f "x")
    "xx"

The names [`foreach`](#foreach) and [`subst`](#subst) built-ins are
actually special forms that invoke the true built-ins, which are available
in their raw forms by the names `.foreach` and `.subst`.

Directly using the `.foreach` built-in can be awkward, because the referenced
variable name is unknown to SCAM and will trigger an error unless a declaration
is used:

    > (.foreach "x" "1 2 3" (1+ x))
    line 1: undefined variable: x
    at: (.foreach "x" "1 2 3" (1+ *x*))
    > (.foreach "x" "1 2 3" (begin (declare x) (1+ x)))
    "2 3 4"
    > (foreach x "1 2 3" (1+ x))
    "2 3 4"

The [`foreach`](#foreach) special form avoids this problem.  The
[`subst`](#subst) special form enhances `.subst` by allowing multiple
substitutions to be performed.


## Make Features

Variables and built-in functions defined by Make are available to SCAM
programs.  See <http://www.gnu.org/software/make/manual/make.html#Name-Index>
for a complete list.

### Make Variables

The following Make-defined variables are pre-declared for SCAM programs:

    .DEFAULT_GOAL MAKEFILE_LIST

Other variables can be accessed using the `value` function, or by declaring
the variable and then referencing its name:

    (declare MAKELEVEL &global)
    (print MAKELEVEL)

### Make Built-In Functions

Make built-in functions are directly usable by SCAM programs:

    abspath basename dir error eval firstword flavor foreach info lastword
    notdir origin realpath shell sort strip suffix value warning wildcard
    words addprefix addsuffix filter filter-out findstring join word
    patsubst subst wordlist if and or call

In the case of [`foreach`](#foreach) and [`subst`](#subst), those
names are bound to equivalent, but more friendly wrappers.

### Rule Processing

After a SCAM program's `main` executes, make will proceed to perform rule
processing.  During execution of `main`, the program can create rules using
`eval`.  For example:

    (eval "foo.o: foo.c ; cc -o $@ $<")

If the program defines rules, then Make will proceed to process them as
usual, treating the first-defined rule as the default goal.

After the program-defined rules complete successfully (or if no rules were
specified by the program) a SCAM-defined "exit" rule will execute.  This
runs any "exit" hooks that have been registered by the program, and exits
the process with the exit code that the program's `main` function has
returned.  One such hook displays tracing results (see `trace.scm`).

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
The following command line counts function invocations within the SCAM
compiler itself as it compiles a module, and then lists all called functions
ranked by frequency:

    $ SCAM_TRACE=':c' bin/scam num.scm

The `SCAM_DEBUG` variable causes certain debug information to be written to
stdout based on the presence or absence of certain substrings:

  * "O" ==> "OK: ..." when an `expect` macro succeeds. (See [the core library](core.scm).)
  * "U" ==> display compile-time "warnings" for lambda captures
  * "R" ==> "require: <filename>" when a file is included by `require`.
  * "Rx" ==> "R" + "exited: <filename>" after a required file exits.
  * "S" ==> "shell: <command>" when io.scm executes a shell command.
  * "B" ==> "Eval: ..." when Make rules are eval'ed by build.scm.

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


## Libraries

SCAM contains several modules that can be used by SCAM programs or from the
SCAM REPL.  Programs will need to `require` the appropriate module; these
are by default included in the REPL environment.

For documentation, refer to the corresponding `.scm` files in the SCAM
project.  (Look for `&public` functions.)

The following are general-purpose modules:

 - `core`: generic, commonly-used functions.
 - `num`: arbitrary-precision decimal floating point numbers.
 - `io`: generic I/O functions.
 - `trace`: functions for tracing, debugging, and profiling.
