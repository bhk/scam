# SCAM Libraries

| Module | Exports |
| :-- | :-- |
| [core](#core-general-purpose-functions) | [`1+`](#1-n) [`append`](#append-a-b-c-d-e-f-g-h-others) [`assert`](#assert-cond) [`assoc`](#assoc-key-vecv) [`assoc-initial`](#assoc-initial-prefix-vec) [`assoc-vec`](#assoc-vec-key-vec-vecv) [`butlast`](#butlast-vec) [`concat-vec`](#concat-vec-vec-delim) [`conj`](#conj-vec-item) [`cons`](#cons-item-vec) [`dict-collate`](#dict-collate-pairs) [`dict-compact`](#dict-compact-dict-result) [`dict-find`](#dict-find-key-dict) [`dict-get`](#dict-get-key-dict-default) [`dict-key`](#dict-key-pair) [`dict-keys`](#dict-keys-dict) [`dict-remove`](#dict-remove-key-dict) [`dict-set`](#dict-set-key-value-dict) [`dict-value`](#dict-value-pair) [`dict-values`](#dict-values-dict) [`eq?`](#eq-a-b) [`expect`](#expect-a-b) [`expect-x`](#expect-x-a-b-file-line) [`fexpect`](#fexpect-a-b) [`foldl`](#foldl-f-z-v) [`foldr`](#foldr-f-z-v) [`format`](#format-str) [`format-add`](#format-add-func) [`identity`](#identity-a) [`index-of`](#index-of-vec-item) [`indices`](#indices-lst) [`intersperse`](#intersperse-value-vec) [`last`](#last-vec) [`memoize`](#memoize-funcname) [`numeric?`](#numeric-s) [`printf`](#printf-fmt-values) [`reverse`](#reverse-list) [`see`](#see-substr-str) [`select-vec`](#select-vec-fn-list) [`select-words`](#select-words-fn-list) [`sort-by`](#sort-by-key-func-values) [`split`](#split-delim-str) [`sprintf`](#sprintf-fmt-values) [`strip`](#strip-vec) [`trace-expect`](#trace-expect-a-b) [`uniq`](#uniq-vec) [`vec-intersect`](#vec-intersect-a-b) [`vec-or`](#vec-or-vec) [`vec-subtract`](#vec-subtract-a-b) [`vsprintf`](#vsprintf-fmt-values) [`while`](#while-pred-do-initial) [`word-index?`](#word-index-n) [`xor`](#xor-a-b) |
| [getopts](#getopts-command-line-options-parser) | [`getopts`](#getopts-argv-opts) |
| [intrinsics](#intrinsics) | [`abspath`](#abspath-paths) [`addprefix`](#addprefix-prefix-list) [`addsuffix`](#addsuffix-suffix-paths) [`and`](#and-expr) [`append-for`](#append-for-var-vector-body) [`apply`](#apply-lambda-vec) [`at-exit`](#at-exit-func) [`basename`](#basename-paths) [`begin`](#begin-expr) [`concat`](#concat-values) [`concat-for`](#concat-for-var-vector-delim-body) [`current-env`](#current-env) [`current-file-line`](#current-file-line) [`declare`](#declare-) [`declare`](#declare-) [`demote`](#demote-value) [`dir`](#dir-paths) [`do-not-trace`](#do-not-trace-names) [`error`](#error-message) [`filter`](#filter-patterns-list) [`filter-out`](#filter-out-patterns-list) [`filtersub`](#filtersub-pat-repl-str) [`findstring`](#findstring-sub-str) [`first`](#first-vec) [`firstword`](#firstword-list) [`for`](#for-var-vector-body) [`foreach`](#foreach-var-list-body) [`if`](#if-cond-then-expr-else-expr) [`join`](#join-list-a-list-b) [`lastword`](#lastword-list) [`not`](#not-a) [`notdir`](#notdir-paths) [`nth`](#nth-index-vec) [`nth-rest`](#nth-rest-n-list) [`or`](#or-expr) [`patsubst`](#patsubst-pat-repl-list) [`print`](#print-value) [`promote`](#promote-value) [`realpath`](#realpath-paths) [`require`](#require-module-private) [`rest`](#rest-vec) [`rrest`](#rrest-vec) [`set`](#set-name-value-retval) [`shell`](#shell-command) [`sort`](#sort-list) [`subst`](#subst-from-to-from-to-value) [`suffix`](#suffix-paths) [`trace`](#trace-specs) [`tracing`](#tracing-spec-expr) [`untrace`](#untrace-names-retval) [`vector`](#vector-a-b-c-) [`wildcard`](#wildcard-patterns) [`word`](#word-n-list) [`wordlist`](#wordlist-a-b-list) [`words`](#words-list) |
| [io](#io-file-io-and-shell-interaction) | [`chmod-file`](#chmod-file-filename-mode) [`clean-path`](#clean-path-path) [`cp-file`](#cp-file-src-dst-make-dst-dir) [`echo-command`](#echo-command-str) [`escape-path`](#escape-path-path) [`file-exists?`](#file-exists-filename) [`fprintf`](#fprintf-fd-format-values) [`getline`](#getline-prompt) [`hash-file`](#hash-file-filename) [`hash-files`](#hash-files-filenames) [`hash-output`](#hash-output-cmd) [`ioshell`](#ioshell-cmd) [`mkdir-p`](#mkdir-p-dir) [`quote-sh-arg`](#quote-sh-arg-arg) [`quote-sh-file`](#quote-sh-file-filename) [`read-file`](#read-file-filename) [`read-lines`](#read-lines-filename-start-end) [`resolve-path`](#resolve-path-dir-path) [`save-blob`](#save-blob-obj-dir-data) [`shell!`](#shell-cmd) [`unescape-path`](#unescape-path-loc) [`write`](#write-fd-data) [`write-file`](#write-file-filename-data) |
| [math](#math-numeric-operations) | [`!=`](#-x-y) [`*`](#-x-y) [`*~`](#-x-y-p) [`+`](#-x-y) [`-`](#--x-y) [`/`](#-x-y-p) [`//`](#-x-y) [`0-`](#0--x) [`<`](#-x-y) [`<=`](#-x-y) [`=`](#-x-y) [`>`](#-x-y) [`>=`](#-x-y) [`^`](#-x-y) [`abs`](#abs-x) [`atan`](#atan-m-p) [`atan2`](#atan2-y-x-p) [`ceil`](#ceil-x) [`cos`](#cos-x-p) [`exp`](#exp-x-p) [`floor`](#floor-x) [`format-fixed`](#format-fixed-x-min-width-decimals) [`frexp10`](#frexp10-x) [`get-pi`](#get-pi-p) [`log`](#log-x-b-p) [`max`](#max-x-y) [`min`](#min-x-y) [`mod`](#mod-x-y) [`num-lex`](#num-lex-n) [`num-sort`](#num-sort-v) [`pow`](#pow-x-y-p) [`range`](#range-x-y) [`round`](#round-x-p-dir) [`sin`](#sin-x-p) [`sum`](#sum-args) [`trunc`](#trunc-x) |
| [memo](#memo-persistent-memoization) | [`memo-apply`](#memo-apply-fname-args) [`memo-blob-call`](#memo-blob-call-fname-args) [`memo-call`](#memo-call-fname-args) [`memo-chmod-file`](#memo-chmod-file-filename-mode) [`memo-drop`](#memo-drop) [`memo-hash-file`](#memo-hash-file-filename) [`memo-io`](#memo-io-fname-args) [`memo-on`](#memo-on-dbfile-expr) [`memo-read-file`](#memo-read-file-filename) [`memo-write-file`](#memo-write-file-filename-data) |
| [peg](#peg-peg-parser-generator) | [`gen-lex`](#gen-lex-tokens) [`lex`](#lex-text-tokens) [`peg-*`](#peg--pf) [`peg-+`](#peg--pf) [`peg-?`](#peg--pf) [`peg-and`](#peg-and-pfs) [`peg-at`](#peg-at-pf) [`peg-c`](#peg-c-name-pf) [`peg-empty`](#peg-empty-caps) [`peg-not`](#peg-not-pf) [`peg-or`](#peg-or-pfs) [`peg-p`](#peg-p-in-out-caps) [`un-lex`](#un-lex-subj) |
| [string](#string-string-manipulation) | [`gen-polysub`](#gen-polysub-froms-tos-input) [`string-from-bytes`](#string-from-bytes-bytes) [`string-len`](#string-len-s) [`string-lower`](#string-lower-str) [`string-repeat`](#string-repeat-str-num) [`string-slice`](#string-slice-first-last-str) [`string-to-bytes`](#string-to-bytes-str) [`string-to-chars`](#string-to-chars-s) [`string-upper`](#string-upper-str) [`strings-from-bytes`](#strings-from-bytes-bytes) |
| [utf8](#utf8-utf-8-coding) | [`utf8-decode`](#utf8-decode-bytes) [`utf8-encode`](#utf8-encode-codes) |


# core: General-Purpose Functions

The `core` library provides general-purpose functions, such as `eq?`, and
common operations on the fundamental data types in SCAM -- word lists,
vectors, and dictionaries.

## Exports

##### `(1+ n)`

Add one to N.  N must contain only decimal digits.


##### `(append ?a ?b ?c ?d ?e ?f ?g ?h ...others)`

Combine one or more (potentially empty) vectors, word lists, or
dictionaries.


##### `(assert cond)`

If COND is nil, display diagnostics and terminate execution.


##### `(assoc key vecv)`

Return the first vector in VECV whose first item is KEY.


##### `(assoc-initial prefix vec)`

Return items that match PREFIX or begin with `(concat PREFIX " ")`.


##### `(assoc-vec key-vec vecv)`

Return the first vector in VECV whose initial items match those in KEY-VEC.


##### `(butlast vec)`

Return elements in vector VEC *except* for the last one.
This may also be applied to word lists or dictionaries.


##### `(concat-vec vec ?delim)`

Concatenate strings in VEC, separating them with DELIM.


##### `(conj vec item)`

Add ITEM to end of vector VEC.


##### `(cons item vec)`

Add ITEM to the front of vector VEC.


##### `(dict-collate pairs)`

Create a new dictionary in which each key from DICT appears only once,
bound to a vector of all its values in DICT.


##### `(dict-compact dict ?result)`

Remove pairs in a dictionary that are preceded by pairs that share the
same KEY.


##### `(dict-find key dict)`

Return the first pair matching KEY.  Unlike `dict-get`, this indicates
whether a match was found (every pair is non-nil).


##### `(dict-get key dict ?default)`

Return the value bound to KEY.  If more than one pair matches KEY, only
the first is returned.  If no pair is found, DEFAULT is returned.


##### `(dict-key pair)`

Return the key portion of PAIR.


##### `(dict-keys dict)`

Return a vector of keys from DICT.


##### `(dict-remove key dict)`

Remove all pairs whose key portion is KEY from DICT.


##### `(dict-set key value dict)`

Bind KEY to VALUE in dictionary DICT, removing other entries for KEY.


##### `(dict-value pair)`

Return the value portion of PAIR.


##### `(dict-values dict)`

Return a vector of values from DICT.


##### `(eq? a b)`

Return 1 if A and B are equal, nil otherwise.


##### `(expect a b)`

Compare A to B; if they are not equal, display diagnostics and terminate
execution.


##### `(expect-x a b file-line)`

Compare A to B, and if unequal display diagnostics and terminate
execution.  This function is exported to enable clients to easily
implement variants of `expect`, since FILE-LINE data must come
from a macro in order to reflect the location of the caller.

FILE-LINE = "file:line:" prefix for the diagnostic message.


##### `(fexpect a b)`

Like `expect, but only the formatted versions of A and B are compared.
This accommodates only minor differences in the concrete layout that do
not affect the meaning in some contexts.  For example, a record ending in
a `&list` member (that is empty) will have a trailing space when
constructed, but not after being retrieved from another record (when
stored as a trailing `&list` parameter).


##### `(foldl f z v)`

Apply the two-argument function F to all elements of V, starting at the
left with `(F Z <first>)`.  If V is empty, return Z.


##### `(foldr f z v)`

Like `foldl`, but starting from the right with `(F <last> Z)`.


##### `(format str)`

Return a SCAM literal that evaluates to VALUE, using the "friendliest"
representation available.


##### `(format-add func)`

Register a formatting function that will be used by `format`.


##### `(identity a)`

Return A.


##### `(index-of vec item)`

Return the index of ITEM in VEC, or 0 if ITEM is not found.


##### `(indices lst)`

Return a vector of the indices (1, 2, ...) of words in word list (or vector)
LST.


##### `(intersperse value vec)`

Insert VALUE into VEC between every two adjacent items.  If VEC is empty,
the result is empty.  Otherwise, the result has one less than twice as
many elements as VEC.


##### `(last vec)`

Return the last item in vector VEC.


##### `(memoize funcname)`

Memoize a function that accepts up to three arguments.


##### `(numeric? s)`

Return S if S is a valid numeric literal in SCAM, nil otherwise.


##### `(printf fmt ...values)`

Display a message to stdout, followed by a newline.  See `vsprintf` for
handling of FMT and VALUES.


##### `(reverse list)`

Reverse word list (or vector) LIST.


##### `(see substr str)`

Return 1 if SUBSTR appears within STR.  Print a diagnostic otherwise.


##### `(select-vec fn list)`

Return a vector of all members of VEC for which (FN member) is non-nil.


##### `(select-words fn list)`

Return a list of words in LIST for which `(FN <word>)` is non-nil.


##### `(sort-by key-func values)`

Sort a vector VALUES in order of increasing `(KEY-FUNC i)` for each item i.


##### `(split delim str)`

Split STR at each occurrence of DELIM.  Returns vector whose length is
one more than the number of occurrences of DELIM.


##### `(sprintf fmt ...values)`

Like `vsprintf`, but values are provided as separate arguments.


##### `(strip vec)`

Remove redundant spaces from a vector or list.

Consecutive spaces and tabs will be collapsed to a single space and
leading and trailing spaces will be removed.  Newline characters are
not disturbed.


##### `(trace-expect a b)`

Like `expect`, but evaluation of A and B is done with tracing enabled.


##### `(uniq vec)`

Return the unique members of VEC *without* re-ordering.  The first
occurrence of each member is retains.  This can be applied to word lists
or dictionaries as well.

The `sort` function returns unique items and is much faster, but it does
not preserve ordering.


##### `(vec-intersect a b)`

Return entries in vector A that also appear in vector B.
This may also be applied to dictionaries.


##### `(vec-or vec)`

Return the first non-nil member of VEC.


##### `(vec-subtract a b)`

Return entries in vector A that do not appear in vector B.
This may also be used to applied to dictionaries.


##### `(vsprintf fmt values)`

Expand FMT, replacing escape sequences with values from vector VALUES,
returning the resulting string.

The following escape sequences are supported:
-  `%s` -> value
-  `%q` -> `(format value)`


##### `(while pred do initial)`

Recursively apply FN to VALUE until `(PRED result)` is nil, then return
the final value.


##### `(word-index? n)`

Return non-nil if N is safe to pass to WORD or WORDLIST.  This means it
consists only of decimal digits and is non-zero.


##### `(xor a b)`

Return the parameter that is not nil (unless both or none are nil).
# getopts: Command Line Options Parser

## Exports

##### `(getopts argv opts)`

Parse command line options.

ARGV = arguments vector\
OPTS = a string of option specifiers\
Result = a dictionary describing options, non-option arguments, and errors.

Any non-option command-line arguments will appear in the result in a
vector bound to the key `"*"`.  In other words, `(dict-get "*" RESULT)`
yields all of the non-option arguments.

Option specifiers in OPTS may begin with `"-"` or `"--"`.  Leading dashes
are not included in the dictionary keys.  Option names may not contain
`%`, `!`, `*`, or whitespace.

Options can appear multiple more times.  If not seen, the option name
will not appear in the result.  Otherwise, it will be bound to a vector
that contains one value per occurrence:

 - If an option specifier ends in `"="`, this indicates that the option
   consumes an argument.  Its values will be the consumed arguments.

 - If an option does not end in `"="`, its values will all be `1`.

If `--` is seen in ARGV, all elements following `--` are treated as
non-option arguments.  Otherwise, options can appear in any order, before
and after non-option arguments.

If errors are encountered, a `"!"` entry in the dictionary will exist,
containing `GetoptsError` records.

 - `(MissingArg OPT)` : Option specifier OPT takes an argument but was found
   in last element of argv.
 - `(BadOption ARG)` : Argument ARG began with "-" but did not match any
   option specifiers.

Example:

    (getopts ["a" "-f" "--g" "x" "b"]      ;; command line as in `argv`
             "-f --g= -h")                 ;; option description
    {f: 1, g: "x", *: ["a" "b"] }          ;; result
# Intrinsics

There is not an "intrinsics" module; all these exports are defined by the
SCAM language itself and are available to program without any `require`
statement.

Intrinsic symbols fall into three different categories:

 - Special forms
 - Manifest functions
 - Manifest macros

Manifest functions are like other functions in SCAM, except that they are
provided by the language itself.  Special forms are not functions, so
they do not have values and cannot be passed to other functions.

## Exports

##### `(abspath paths)`

For each file name in PATHS, convert relative paths to absolute paths.
The file names do not need to refer to an existing file or directory.


##### `(addprefix prefix list)`

Add PREFIX to the start of each word in LIST.


##### `(addsuffix suffix paths)`

Add SUFFIX to the end of each word in PATHS.


##### `(and expr...)`

Expressions in `EXPR...` are evaluated sequentially until a `nil` value is
encountered.  The value of the `and` expression is that of the last
sub-expression evaluated, or `nil` if no expressions were evaluated.


##### `(append-for var vector body)`

`append-for` is similar to `for` but it appends together all of the (vector)
values of BODY.  This is functionally similar to what is called `concat-map`
in some other languages.

    > (append-for x [[1 2] [3 4]]
    +     x)
    "1 2 3 4"
    > (append-for x [7 1 5 3]
    +    (if (> x 3) [x]))
    "7 5"


##### `(apply lambda vec)`

Call LAMBDA, passing as arguments the members of the vector VEC.

Example:

    > (apply nth [3 "a b c d"])
    "c"


##### `(at-exit func)`

Add FUNC to a list of functions that will be run when the program exits.
Functions will be run in the reverse of the order that they were registered.


##### `(basename paths)`

Remove the file extension from each word in PATHS.


##### `(begin expr...)`

Encloses a *block* of expressions.  A block is a sequence of expressions
that are evaluated in order.  The result of that last expression is
returned (or nil if no expressions are given).


##### `(concat ...values)`

This special form concatenates all of its arguments.


##### `(concat-for var vector delim body)`

`concat-for` is similar to `for` but it concatenates the values of BODY.

    > (concat-for x [1 2 3] ";" (wordlist 1 x "a b c"))
    "a;a b;a b c"


##### `(current-env)`

This special form evaluates to the data structure that describes the lexical
environment at the point where it is invoked.  (See `gen.scm` for details of
this structure.)


##### `(current-file-line)`

This special form evaluates to the file name and line number of where it was
invoked, in this format:  `FILENAME:LINE`


##### `(declare ...)`

Declare a function, variable, or macro.

This special form takes two forms:

    (declare NAME FLAG...)             ; global data variable
    (declare (NAME ARG...) FLAG...)    ; global function variable

The `declare` special form declares a global variable without assigning
a value.  This is usually used to access non-SCAM functions, or when
mutually recursive functions are defined.


##### `(declare ...)`

Declare and define a function, variable, or macro.

This special form takes several forms:

    (declare NAME FLAG... VALUE)            ; global data variable
    (declare (NAME ARG...) FLAG... BODY)    ; global function variable
    (declare `NAME EXPR)                    ; symbol macro
    (declare `(NAME ARG...) FLAG... BODY)   ; compound macro

The `define` special form adds a name to the environment and associates
it with a definition.  The defined name will be visible to subsequent
expressions in the same block, and the definition supersedes any
previous definitions associated with the same name.

The `&public` flag may be included in `FLAG...`.  This indicates that the
symbol should be visible outside of the file in which it is declared.


##### `(demote value)`

`demote` encodes any value as a word so that it may be embedded in a word
list.  It is used internally to construct vectors.


##### `(dir paths)`

Get the directory of each file in PATHS.

    (dir "a/b c")  -->  "a/ ./"

See also `notdir`.


##### `(do-not-trace names)`

Add NAMES to the list of functions that should not be instrumented when
`trace` or `tracing` are called.  In order to avoid undefined behavior,
we must avoid instrumenting functions *while* they are executing.


##### `(error message)`

Terminate execution of the program with a non-zero status code, writing
`MESSAGE` to stderr.


##### `(filter patterns list)`

Get all words in LIST that match PATTERNS.  Each word in PATTERNS is
compared against the words in LIST, and is considered a match when it is
equal, treating the first `%` character in a pattern as a wildcard.


##### `(filter-out patterns list)`

Get all words in LIST that do *not* match PATTERNS.


##### `(filtersub pat repl str)`

Replace PAT with REPL if STR matches PAT; return nil otherwise.


##### `(findstring sub str)`

If SUB occurs within STR, return SUB.  Otherwise return `nil`.


##### `(first vec)`

Get the first element of VEC.


##### `(firstword list)`

Get the first word in LIST.


##### `(for var vector body)`

`for` iterates over items in a vector, evaluating BODY for with VAR bound to
an item, constructing a new vector with the results of BODY. Example:

    > (for x [[1 2] [3 4]]
    +     (reverse x))
    [[2 1] [4 3]]


##### `(foreach var list body)`

The `foreach` special form iterates over a list, evaluates BODY (a sequence
of expressions) once for each word, and constructs a new word list from the
results of each evaluation.

Each word is bound to the name `VAR` while `BODY` is evaluated.

    > (foreach x "1 2 3" (1+ x))
    "2 3 4"


##### `(if cond then-expr [else-expr])`

This special form conditionally executes either THEN-EXPR or ELSE-EXPR.
First, COND is evaluated.  If non-nil, THEN-EXPR will be evaluated and used
are the value for the `if` expression.  Otherwise, if ELSE-EXPR is present
it is evaluated and used, and if not `nil` is used.


##### `(join list-a list-b)`

Concatenate the respective words in LIST-A and LIST-B.


##### `(lastword list)`

Get the last word in LIST.


##### `(not a)`

Logically invert A.  If A is `nil`, return 1.  Otherwise, return `nil`.


##### `(notdir paths)`

Return the filename portion of each path in PATHS.

    (notdir "a/b c")  -->  "b c"

See also `dir`.


##### `(nth index vec)`

Returns the value stored at index INDEX (1-based) in vector VEC.


##### `(nth-rest n list)`

Get the tail of LIST, starting at the Nth element (1-based).


##### `(or expr...)`

Expressions in `EXPR...` are evaluated sequentially until a non-`nil` value
is encountered.  The value of the `and` expression is that of the last
sub-expression evaluated, or `nil` if no expressions were evaluated.


##### `(patsubst pat repl list)`

Replace PAT with REPL in LIST.  Patterns use the first `%` as a wildcard,
and a corresponding `%` in REPL will be replace with what the wildcard in
PAT matched.  See the GNU make documentation for subtleties.


##### `(print value...)`

Concatenate all values and write them to stdout.


##### `(promote value)`

`promote` reverses the encoding done by `demote`.


##### `(realpath paths)`

For each file name in PATHS, resolve the name to a form without `.` or
`..` path elements or symbolic links.  See the UNIX `realpath` C function
for details.


##### `(require module &private?)`

The `require` special form provides access to functionality defined in other
modules.  `MODULE` is a literal string that names either a SCAM source file
or a standard SCAM module.  Symbols are exported by the module (those
declared `&public`) will be are visible to all expressions that follow in
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


##### `(rest vec)`

Remove the first element from vector (or list) VEC.


##### `(rrest vec)`

Remove the first two elements from vector (or list) VEC.


##### `(set name value retval)`

The `set` special form assigns a value to a previously declared global
variable.

NAME is given as a symbol, not a string. For example:

    (set var 12 nil)

The `set` expression returns `RETVAL` (or "" if `RETVAL` is not provided).


##### `(shell command)`

Execute COMMAND using the default shell as specified by the SHELL
variable (which is inherited from the SHELL environment variable).  The
data written to `stdout` by the COMMAND is captured, trailing newlines
are removed, and remaining newlines are converted to spaces, and the
result is returned by `shell`.


##### `(sort list)`

Sort the words in LIST in increasing lexicographical order.


##### `(subst from to {from to}... value)`

This special form replaces substrings with replacement strings within the
given VALUE.  For example:

    > (subst 2 3 1 2 12)
    23


##### `(suffix paths)`

Return the file extensions of all file names in PATHS.


##### `(trace specs)`

Instrument functions for tracing as described by SPEC.

SPEC is as documented for `tracing`.

The return value is a list of the names of the instrumented functions,
which can be passed to `untrace` later.


##### `(tracing spec expr)`

Evaluate EXPR with tracing activated according to SPEC.

SPEC is a string used to specify which functions are traced and how they
are traced.  In its simplest form, it is a list of function names.
Beyond that, a `:` followed by a "mode" may be appended to each name.
Possible modes are:

 - `t` : Print the function name and arguments when it is called and its
         return value when it returns.  This is the default mode.

 - `f` : Print just the function name on entry and exit.

 - `c` : Count the number of times that the function is invoked.
         Function counts will be written to stdout when tracing is
         removed.  This can occur when `(tracing ...)` completes, or when
         `(untrace ...)` is called, or after `main` returns.

 - `x<N>` : Evaluate the function body N times each time the function is
         invoked.  <N> must be a positive number or the empty string
         (which is treated as 11).

 - `-` : Exclude the function(s) from instrumentation.  Any functions
         matched by this entry will be skipped even when they match other
         entries in the specification string.  This does not depend on
         the ordering of entries.  For example, `(trace "a% %z:-")` will
         instrument all functions whose names begin with `a` except for
         those whose names end in `z`.

In place of a function name a pattern may be provided to match multiple
functions.  In a pattern, the `%` character is a wildcard that will match
any sequence of characters.  Some caution must be exercised in general
with tracing, especially when using wildcards: Do not instrument any
function while it is currently running.  SCAM prevents this from
happening when you use `SCAM_TRACE`, or when you call `trace` or
`tracing` at the top level of a source file, the REPL, or your `main`
function.  However, if while nested in one or more other user-defined
functions, you trace any of those functions, then undefined behavior will
result.

The intent of `x` instrumentation is to cause the function to consume
more time by a factor of N (for profiling purposes).  If your code is
purely functional, or at least limits its side effects to idempotent
operations, repetition of expressions should not alter the behavior of
the program.  This can be used to identify and quantify hotspots in a
program.


##### `(untrace names ?retval)`

Remove instrumentation from functions listed in NAMES, or functions
matched by patterns in NAMES.


##### `(vector a b c ...)`

This constructs a vector.  It is equivalent to `[A B C ...]`.


##### `(wildcard patterns)`

Get files that match PATTERNS.  Patterns is a list of glob expressions.


##### `(word n list)`

Return the Nth word in LIST.  If N is greater than the length of the
list, return `nil`.  If N is less than 1, terminate the program with an
error message.


##### `(wordlist a b list)`

Extract the words in LIST from index A to index B, inclusive.  Extraneous
spaces between words are retained.  If B is less than A, `nil` is returned.

If A is less than 1 or B is less than zero, the program will be
terminated with an error message.


##### `(words list)`

Return the number of words in LIST.
# io: File I/O and Shell Interaction

## Exports

##### `(chmod-file filename mode)`

Modify file mode.  Return nil on success, description on error.
MODE is as defined by the `chmod` command.


##### `(clean-path path)`

Remove redundant "." and ".." path elements and repeated "/" characters
from an absolute or relative path.  PATH may include whitespace.


##### `(cp-file src dst ?make-dst-dir)`

Copy file SRC to DST.  Return nil on success, description on error.


##### `(echo-command str)`

Construct a command line that will echo STR.


##### `(escape-path path)`

Escape a relative path or absolute path, so that the result is:
 - safe as a sub-directory (has no "..", and is not an absolute path)
 - a word (contains no spaces)
 - directly usable with `filter` (contains no "%")
 - safe to include in an "include ..." directive (no globbing chars)
 - safe to use in a make rule without escaping
 - unique (the encoding can be reversed)


##### `(file-exists? filename)`

Return FILENAME if file FILENAME exists.  The `wildcard` built-in
function is a faster alternative, but it caches results and will not
reflect files created/deleted when the program is running.


##### `(fprintf fd format ...values)`

Format text and write to a file.  See `vsprintf` for handling of FORMAT
and VALUES.  Unlike `printf`, no trailing newline is appended.


##### `(getline prompt)`

Read one line from `stdin`.

Note: On MacOS, input lines longer than 1023 characters will cause bad
things to happen.


##### `(hash-file filename)`

Return the hash of one file (see `hash-files`).


##### `(hash-files filenames)`

Hash multiple files, returning a dictionary mapping file names to hash
values.  Hash values are 16 bytes long.  The selection of which hash
algorithm to use depends on what is available in PATH; it is guaranteed
to remain the same for the duration of the program's execution.


##### `(hash-output cmd)`

Execute shell command CMD, has what it writes to `stdout`, and return the
hash.


##### `(ioshell cmd)`

Perform a shell command CMD, logging results if `S` appears in SCAM_DEBUG.


##### `(mkdir-p dir)`

Create directory DIR and parent directories, if necessary.


##### `(quote-sh-arg arg)`

Quote argument ARG for POSIX shells.


##### `(quote-sh-file filename)`

Quote FILENAME for POSIX shells and ensure it does not begin with '-'.


##### `(read-file filename)`

Read the contents of file FILENAME and return it as a string.


##### `(read-lines filename ?start ?end)`

Read contents of file FILENAME and return a vector of lines.  The number
of elements in the resulting vector is one more than the number of
newlines in the file.

Return `nil` if the file is not readable.


##### `(resolve-path dir path)`

Combine a directory name and a path (relative or absolute).


##### `(save-blob obj-dir data)`

Write DATA to a file in OBJ-DIR whose name is a function of DATA.
Returns the path to the new file.


##### `(shell! cmd)`

Execute command CMD, returning data written to `stdout`.

Unlike `shell`, which trims trailing newlines and then converts newlines
to spaces, `shell!` preserves newline and space characters.  It does
guarantee complete fidelity, however: NUL characters will not be
preserved, and the last line of output will be terminated with a newline
(whether it was present or not in the command output).


##### `(unescape-path loc)`

Undo `escape-path`.


##### `(write fd data)`

Write data to a file descriptor.  Since `shell` captures `stdout` for the
command it invokes, we replace 1 with 9, which has been redirected to
*actual* `stdout` (see the prologue in build.scm).

We redirect `stderr` to `stdout`, so that `shell` will capture error
messages. Special care must be taken when fd is 2.

Result is `nil` on success; non-nil if the file descriptor is bad.


##### `(write-file filename data)`

Write DATA to file FILENAME.

In order to handle large values, the data is written line-by-line, using
multiple shell invocations, to a temporary file that is moved to FILENAME
only on success, so that if the operation is interrupted (e.g. the SCAM
process is killed) then FILENAME will not be left with partial data.

On success, nil is returned.  Otherwise, an error description is returned.
# math: Numeric Operations

The `math` library implements operations on numbers.

Arbitrary-precision arithmetic is supported.  There is no *a priori*
limit imposed on the size of numbers.

## Numbers

Numbers are represented as strings of decimal digits, with an optional
sign, decimal point, and E-notation suffix.  More precisely:

    Number  := "-"? Integer ("." Integer)? Exp?
    Integer := ( "0" | "1" | ... | "9" )+
    Exp     := ("E" | "e") ("+" | "-" | "") Integer

Strings not conforming to the above syntax are treated as non-number
values.  Functions in this library typically return `"NaN"` when a
numeric argument was given a non-number value, or when the results are
otherwise undefined.  `"NaN:P"` may be returned when an invalid precision
parameter is provided.

Comparison operators treat non-number values as less than all numeric
values, but equivalent to all other non-number values.

## Precision

For many operators -- such as `+`, `-`, `*`, `//`, `mod`, and `^` -- the
result is always numerically exact.  Some functions -- like `/`, `log`,
`sin`, etc. -- yield an approximation with a finite number of digits.
These functions provide an optional precision argument that may be
provided by the caller; otherwise the default is 16 significant digits
(slightly more precise than 64-bit IEEE-754 binary floating point) unless
otherwise specified.

Precision can be specified in two ways: significant digits, or place.

1. A positive decimal integer specifies a number of significant digits in the
   result.

2. An integer N preceded by a `+` or `-` character specifies the *place*
   (relative to the decimal point) of the least significant digit as the
   place with with value of 10^N.  (Note that SCAM numeric literals may
   not begin with "+", so places beginning with "+" must be quoted.)

Examples:

    (/ 200 3 5)    ->  66.666        5 significant digits
    (/ 200 2 -1)   ->  66.7          10⁻¹ is least significant place
    (/ 200 2 "+0") ->  67            10⁰ is least significant place
    (/ 200 2 "+1") ->  70
    (/ 200 2 "+2") -> 100            rounding to nearest 10²
    (/ 200 2 "+3") ->   0            rounding to nearest 10³

## Exports

##### `(!= x y)`

Return 1 if X != Y, nil otherwise.


##### `(* x y)`

Return X * Y.


##### `(*~ x y ?p)`

Compute X*Y to the [precision specified by P](#precision).  The result
will be within one unit of the least significant digit.


##### `(+ x y)`

Return X + Y.


##### `(- x y)`

Return X - Y.


##### `(/ x y ?p)`

Return X / Y to the [precision specified by P](#precision).  The answer
will be rounded to the *nearest* unit in the least significant digit.


##### `(// x y)`

Return floor(X / Y): the largest integer less than or equal to X/Y.


##### `(0- x)`

Negate a number.  This function assumes that X is a valid number; it does
not validate or canonicalize X.


##### `(< x y)`

Return 1 if X < Y, nil otherwise.


##### `(<= x y)`

Return 1 if X <= Y, nil otherwise.


##### `(= x y)`

Return 1 if X = Y, nil otherwise.


##### `(> x y)`

Return 1 if X > Y, nil otherwise.


##### `(>= x y)`

Return 1 if X >= Y, nil otherwise.


##### `(^ x y)`

Raise X to the power of Y.  Y must be an non-negative integer in "simple"
format (without a decimal point or E-notation).


##### `(abs x)`

Absolute value of a number.  This function assumes that X is a valid
number; it does not validate or canonicalize X.


##### `(atan m ?p)`

Return the arctangent of M to the [precision given by P](#precision).

The result is in the range (-π/2,π/2).


##### `(atan2 y x ?p)`

Return the angle between the X axis and the line from the origin to the
point (X,Y).  (**Note** that Y is the first argument and X is the
second.)

The result is in the range (-π,π).

The [precision is given by P](#precision).


##### `(ceil x)`

Return the smallest integer greater than or equal to X.


##### `(cos x ?p)`

Compute the cosine of X to the [precision given by P](#precision).


##### `(exp x ?p)`

Calculate eˣ to the [precision given by P](#precision).


##### `(floor x)`

Return the greatest integer less than or equal to X.


##### `(format-fixed x ?min-width ?decimals)`

Convert X to a fixed-point representation.

MIN-WIDTH = if non-nil, minimum field width.  Padding with spaces on the
   left will be added as necessary.\
DECIMALS = if non-nil, number of digits to the right of the decimal.


##### `(frexp10 x)`

Return the fraction and exponent portions of X.

Result = [M E] where X = M * 10ᴱ and E is an integer.
 - When X ≠ 0, 0.1 ≤ abs(M) < 1.
 - When X = 0, Result is [0 0].
 - When X is not a number, Result is nil.


##### `(get-pi ?p)`

Compute π to the [precision given by P](#precision).


##### `(log x ?b ?p)`

Calculate the logarithm of X to the [precision given by P](#precision).

B, if given, is the base; if nil, the natural logarithm of X will be
returned.

X and B must be be greater than 0.


##### `(max x y)`

Return the larger of X or Y.


##### `(min x y)`

Return the smaller of X or Y.


##### `(mod x y)`

Return the remainder of floor(X/Y).

`(mod X Y)` is equal to `(- X (* (// X Y) Y))`.


##### `(num-lex n)`

Convert a number to a string.  The *lexical* sort order of multiple
results corresponds to the *numeric* sort order of the numbers.  In other
words, for two numbers X and Y, LX = `(num-lex X)`, and LY = `(num-lex
Y)`, then:

    (< X Y)  <==>  (sort [LX LY]) == [LX LY]

This can be used with `sort-by` to obtain numeric sort order.  E.g.:

    (sort-by (lambda (i) (num-lex (nth 2 i))) ...)


##### `(num-sort v)`

Sort elements of V by the numeric order of the first sub-element.  V may
be a simple list of numbers, or vector of vectors to be sorted by the
first element of each, or a dictionary to be sorted by the numeric value
of each key.


##### `(pow x y ?p)`

Compute xʸ to the [precision given by P](#precision).

X must be non-negative.


##### `(range x y)`

Return a vector of numbers ranging from X to Y.  X and Y must be integers
in "simple" format (without a decimal point or E-notation).


##### `(round x ?p ?dir)`

Round X to the [precision given by P](#precision).

**Note:** P defaults to `"+0"`, unlike other functions accepting
precision values.

DIR is one of the following:
 - `"+"` ⇒ round up (ceiling)
 - `"-"` ⇒ round down (floor)
 - `"|"` ⇒ round towards zero (truncate)


##### `(sin x ?p)`

Compute the sine of X to the [precision given by P](#precision).


##### `(sum ...args)`

Sum all numbers in ARGS.  Each argument may be a number, or a vector of
numbers, or a vector of vectors of numbers, and so on.


##### `(trunc x)`

Return the integer portion of X (rounding towards zero).
# memo: Persistent Memoization

The `memo` module implements a form of memoization (caching of function
results) that supports IO operations and persists across program
invocations.

SCAM uses persistent memoization to rebuild SCAM programs accurately with
with minimal repeated work.

The `memo-on` macro enters a context within which memoized functions
may be called.  It loads previously cached results (if any) from a
specified file, evaluates an expression, and then stores the cache back
to the file.  Nested calls to memo-on are no-ops; only the final exit
from memo-on will save the cache.

Functions that perform IO operations can be memoized when those
operations are replayable: that is, modification of external state is
idempotent.  In order to perform IO, memoized functions may call
operations provided herein (`memo-read-file`, `memo-write-file`), or
construct more custom IO using `memo-io`.

## Exports

##### `(memo-apply fname args)`

Call `(FNAME ...ARGS)`, or return cached results.


##### `(memo-blob-call fname ...args)`

Memoize a function call that might return a large amount of data.  The
return value is stored as an blob, and only the blob paths are stored
in the database.  We assume the blobs are retained as long as the DB
file.


##### `(memo-call fname ...args)`

Call `(FNAME ...ARGS)`, or return cached results.


##### `(memo-chmod-file filename mode)`

Call `chmod-file`, logging the IO as a dependency.  MODE is formatted as
per the `chmod` command.


##### `(memo-drop)`

Discard memoization results from the current session, preventing them
from being persisted.


##### `(memo-hash-file filename)`

Return hash of file contents, logging the IO transaction for playback.


##### `(memo-io fname ...args)`

Perform an IO operation.  Log the IO as an additional input to the
function being recorded (if there is one).


##### `(memo-on dbfile expr)`

Evaluate EXPR with memoization initialized.  If a session is not active,
begin one using DBFILE as the database.  DBFILE will be evaluated only
when a session is initiated.


##### `(memo-read-file filename)`

Read data from FILENAME, logging the IO transaction for playback.


##### `(memo-write-file filename data)`

Write data to FILENAME, logging the IO transaction for playback.
# peg: PEG Parser Generator

The peg module exports *generators* that create parsing functions.

A *parsing* function attempts to "recognize" or "match" a syntactic
construct (a "pattern") within a subject (a sequence of symbols).  On
success, it may "consume" zero or more symbols.

The generators `peg-p` and `peg-empty` construct primitive parsing
functions, recognizing just 1 and 0 symbols, respectively.  Other
generators like `peg-and`, `peg-or`, and `peg-*` can be used to
construct more complicated parsing functions from these primitives.

Concretely, parsing functions have this prototype:

    (func subject start) -> [next ...captures] | nil

SUBJECT is a vector of symbols describing the string.  Before parsing,
text must be converted to a vector of symbols.  The `lex` and
`gen-lex` functions can be used for this purpose.

START and NEXT are indices into the SUBJECT vector.  The difference
between NEXT and START is the number of symbols consumed by the match.

CAPTURES is the vector of values produced by the parsing function.

## Exports

##### `(gen-lex tokens)`

Return a function that converts text to a vector of symbols.  See `lex`.


##### `(lex text tokens)`

Convert text to a vector of symbols.  TOKENS is a vector including
strings that are symbols of interest to the parsing functions and/or
strings that delimit those symbols.

See also `un-lex`.


##### `(peg-* pf)`

Construct PEG zero-or-more repetition.

Return a parsing function that will call PF sequentially until it fails,
and then succeed.  The resulting position will be what was returned by
the last successful match (or the starting position if none matched), and
the resulting captures will be all captures from successful matches,
appended.


##### `(peg-+ pf)`

Construct PEG one-or-more repetition.

See `peg-*`.


##### `(peg-? pf)`

Construct PEG optional match.

Return a parsing function that will always succeed, returning PF's results
if PF succeeds, or returning the starting position and no captures
otherwise.


##### `(peg-and ...pfs)`

Construct a PEG sequence.

Return a parsing function that will call parsing functions in PFS, one
after another, until one fails.  After each success, the next function
will be called with the remaining un-consumed symbols.  If all functions
in PFS succeed, the resulting position will will be the position
resulting from the final match, and the captures returned will be all
captures from all functions, appended.


##### `(peg-at pf)`

Construct PEG positive lookahead.

Return a parsing function that will succeed if PF succeeds, but which
will not consume any symbols or produce any captures.


##### `(peg-c name pf)`

Construct symbol capture.

Return a parsing function that succeeds when PF succeeds, and on success,
adds to the set of captures a dictionary pair whose key is NAME and whose
value is a vector of all matched symbols.


##### `(peg-empty ?caps)`

Construct PEG empty string.

Return a parsing function that always succeeds, consuming no symbols,
with captures CAPS.


##### `(peg-not pf)`

Construct PEG negative lookahead.

Return a parsing function that will succeed if PF fails and fail if PF
succeeds.


##### `(peg-or ...pfs)`

Construct a PEG prioritized choice.

Return a parsing function that will call parsing functions in PFS, one
after another until one succeeds.  It returns the first successful
result, or `nil` if none of them succeed.


##### `(peg-p in ?out ?caps)`

Construct a PEG terminal symbol.

Return a parsing function that will succeed and consume a single symbol
when `(filter IN (filter-out OUT [SYMBOL]))` is true.  On success, its
captures will be CAPS.

Examples:

 - `(peg-p "%")` matches any symbol.
 - `(peg-p "%" nil [1])` matches any symbol, returning the capture [1].
 - `(peg-p "%" "\n")` matches any symbol except `"\n"`.


##### `(un-lex subj)`

Recover original text from a string of symbols.
# string: String Manipulation

The `string` library provides some common string manipulation functions
that are not available as builtins.

## Exports

##### `(gen-polysub froms tos ?input)`

Construct a function that performs a number of substitutions.  The
returned function accepts one argument and returns the result of the
substitutions.

FROMS = a vector of substrings to be replaced.\
TOS = a vector of corresponding replacements.\
INPUT = if non-nil, a lambda expression that will transform the
  input string before the substitutions.


##### `(string-from-bytes bytes)`

Construct a string from a vector of byte values.  This reverses
`string-to-bytes`.


##### `(string-len s)`

Return the number of *characters* in S.  UTF-8 encoding is assumed.


##### `(string-lower str)`

Convert letters to lower case.  Only ASCII letters are supported.


##### `(string-repeat str num)`

Return a string that consists of NUM copies of STR concatenated.


##### `(string-slice first last str)`

Extract a substring, given start and end character indices.  The range is
inclusive and 1-based.  When LAST is less than FIRST, an empty string is
returned.

FIRST = index of first character to include (1 or greater).\
LAST = index of last character to include (0 or greater).


##### `(string-to-bytes str)`

Get the numeric indices of all *bytes* in STR.  The result is a vector of
numbers from 1 to 255.


##### `(string-to-chars s)`

Get all characters in STR.  The result is a vector of strings, each
containing one character. `concat-vec` reverses this operation.

UTF-8 encoding of STR is assumed.  If STR is not a well-formed UTF8
string, the result will contain all *bytes* in STR (so `concat-vec` will
still reverse the operation) but the bytes may not grouped at character
boundaries.


##### `(string-upper str)`

Convert letters to upper case.  Only ASCII letters are supported.


##### `(strings-from-bytes bytes)`

Convert byte values into single-byte strings.

BYTES = a vector of byte values (numbers from 0 to 255).

The result is a vector the same length as bytes.  Each zero value in
BYTES will result in a corresponding empty string in the result.
# utf8: UTF-8 Coding

## Exports

##### `(utf8-decode bytes)`

Convert UTF-8 encoded bytes to Unicode code points.

BYTES = vector of numeric byte values\
Result = vector of code points


##### `(utf8-encode codes)`

Convert Unicode character indices to UTF-8 encoded bytes.

CODES = vector of Unicode code points\
Result = vector of code points

