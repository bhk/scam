# SCAM Libraries

| Module | Exports |
| :-- | :-- |
| [compile](#compile-scam-compilation) | [`build-program`](#build-program-src-file-exe-file-build-dir-is-quiet) [`compile-text`](#compile-text-text-file-env-in-build-dir-is-quiet) [`run-program`](#run-program-src-file-argv-build-dir-is-quiet) |
| [core](#core-general-purpose-functions) | [`1+`](#1-n) [`append`](#append-a-b-c-d-e-f-g-h-others) [`assert`](#assert-cond) [`assoc`](#assoc-key-vecv) [`assoc-initial`](#assoc-initial-prefix-vec) [`assoc-vec`](#assoc-vec-key-vec-vecv) [`butlast`](#butlast-vec) [`concat-vec`](#concat-vec-vec-delim) [`conj`](#conj-vec-item) [`cons`](#cons-item-vec) [`dict-collate`](#dict-collate-pairs) [`dict-compact`](#dict-compact-dict-result) [`dict-find`](#dict-find-key-dict) [`dict-get`](#dict-get-key-dict-default) [`dict-key`](#dict-key-key-_) [`dict-keys`](#dict-keys-dict) [`dict-remove`](#dict-remove-key-dict) [`dict-set`](#dict-set-key-value-dict) [`dict-value`](#dict-value-_-value) [`dict-values`](#dict-values-dict) [`eq?`](#eq-a-b) [`expect`](#expect-a-b) [`expect-x`](#expect-x-a-b-file-line) [`fexpect`](#fexpect-a-b) [`foldl`](#foldl-f-z-v) [`foldr`](#foldr-f-z-v) [`format`](#format-str) [`format-add`](#format-add-func) [`identity`](#identity-a) [`index-of`](#index-of-vec-item) [`indices`](#indices-lst) [`intersperse`](#intersperse-value-vec) [`last`](#last-vec) [`memoize`](#memoize-funcname) [`numeric?`](#numeric-s) [`permute`](#permute-lst-zz-prefix) [`printf`](#printf-fmt-values) [`repeat-words`](#repeat-words-v-n) [`reverse`](#reverse-list) [`see`](#see-substr-str) [`select-vec`](#select-vec-fn-list) [`select-words`](#select-words-fn-list) [`sort-by`](#sort-by-key-func-values) [`split`](#split-delim-str) [`sprintf`](#sprintf-fmt-values) [`strip`](#strip-vec) [`trace-expect`](#trace-expect-a-b) [`uniq`](#uniq-vec) [`urange`](#urange-min-max) [`vec-intersect`](#vec-intersect-a-b) [`vec-or`](#vec-or-vec) [`vec-subtract`](#vec-subtract-a-b) [`vsprintf`](#vsprintf-fmt-values) [`vsprintfx`](#vsprintfx-fmt-values-codes-fmt-fn) [`while`](#while-pred-do-initial) [`word-index?`](#word-index-n) [`xor`](#xor-a-b) |
| [getopts](#getopts-command-line-options-parser) | [`getopts`](#getopts-argv-opts) |
| [intrinsics](#intrinsics) | [`..`](#-values) [`._.`](#_-values) [`?`](#-fn-args) [`abspath`](#abspath-paths) [`addprefix`](#addprefix-prefix-list) [`addsuffix`](#addsuffix-suffix-list) [`append-for`](#append-for-target-vector-body) [`apply`](#apply-lambda-vec) [`at-exit`](#at-exit-func-unique) [`basename`](#basename-paths) [`begin`](#begin-exprs) [`case`](#case-value-clauses) [`concat`](#concat-values) [`concat-for`](#concat-for-target-vector-delim-body) [`cond`](#cond-clauses) [`current-env`](#current-env) [`current-file`](#current-file) [`current-file-line`](#current-file-line) [`data`](#data-name-ctors) [`declare`](#declare-forms) [`define`](#define-forms) [`defmacro`](#defmacro-name-argname-body) [`demote`](#demote-value) [`dir`](#dir-paths) [`do-not-trace`](#do-not-trace-names) [`error`](#error-message) [`filter`](#filter-patterns-list) [`filter-out`](#filter-out-patterns-list) [`filtersub`](#filtersub-pat-repl-str) [`findstring`](#findstring-sub-str) [`first`](#first-vec) [`firstword`](#firstword-list) [`for`](#for-target-vector-body) [`foreach`](#foreach-target-list-delim-body) [`if`](#if-cond-then-expr-else-expr) [`join`](#join-list-a-list-b) [`lambda`](#lambda-params-body) [`lastword`](#lastword-list) [`let`](#let-bindings-body) [`let&`](#let-bindings-body) [`let-global`](#let-global-bindings-body) [`not`](#not-a) [`notdir`](#notdir-paths) [`nth`](#nth-index-vec) [`nth-rest`](#nth-rest-n-list) [`or`](#or-exprs) [`patsubst`](#patsubst-pat-repl-list) [`print`](#print-values) [`promote`](#promote-value) [`realpath`](#realpath-paths) [`require`](#require-module-private) [`rest`](#rest-vec) [`rrest`](#rrest-vec) [`set`](#set-name-value-retval) [`shell`](#shell-command) [`sort`](#sort-list) [`subst`](#subst-from-to-from-to-value) [`suffix`](#suffix-paths) [`trace`](#trace-spec) [`tracing`](#tracing-spec-expr) [`untrace`](#untrace-names-retval) [`when`](#when-cond-body) [`wildcard`](#wildcard-patterns) [`word`](#word-n-list) [`wordlist`](#wordlist-a-b-list) [`words`](#words-list) |
| [io](#io-file-io-and-shell-interaction) | [`chmod-file`](#chmod-file-filename-mode) [`clean-path`](#clean-path-path) [`cp-file`](#cp-file-src-dst-make-dst-dir) [`cp-file-atomic`](#cp-file-atomic-src-dst-make-dst-dir) [`escape-path`](#escape-path-path) [`file-exists?`](#file-exists-filename) [`fprintf`](#fprintf-fd-format-values) [`get-tmp-dir`](#get-tmp-dir-tmpl) [`getline`](#getline-prompt) [`hash-file`](#hash-file-filename) [`hash-files`](#hash-files-filenames) [`hash-output`](#hash-output-cmd-fmt-args) [`io-sprintf`](#io-sprintf-fmt-args) [`io-vsprintf`](#io-vsprintf-fmt-args) [`mkdir-p`](#mkdir-p-dir) [`mv-file`](#mv-file-from-to) [`path-basename`](#path-basename-path) [`path-dir`](#path-dir-path) [`path-notdir`](#path-notdir-path) [`pipe`](#pipe-stdin-fmt-args) [`quote-sh-arg`](#quote-sh-arg-arg) [`quote-sh-file`](#quote-sh-file-filename) [`read-file`](#read-file-filename) [`read-lines`](#read-lines-filename-start-end) [`resolve-path`](#resolve-path-dir-path) [`save-blob`](#save-blob-dir-name-data) [`shell-lines`](#shell-lines-cmd-fmt-args) [`shellf`](#shellf-cmd-fmt-args) [`unescape-path`](#unescape-path-loc) [`vfprintf`](#vfprintf-fd-format-values) [`write`](#write-fd-data) [`write-file`](#write-file-filename-data) [`write-file-atomic`](#write-file-atomic-file-name-data) |
| [math](#math-numeric-operations) | [`!=`](#-x-y) [`*`](#-x-y) [`*~`](#-x-y-p) [`+`](#-x-y) [`-`](#--x-y) [`/`](#-x-y-p) [`//`](#-x-y) [`0-`](#0--x) [`<`](#-x-y) [`<=`](#-x-y) [`=`](#-x-y) [`>`](#-x-y) [`>=`](#-x-y) [`^`](#-x-y) [`abs`](#abs-x) [`atan`](#atan-m-p) [`atan2`](#atan2-y-x-p) [`ceil`](#ceil-x) [`cos`](#cos-x-p) [`exp`](#exp-x-p) [`floor`](#floor-x) [`format-fixed`](#format-fixed-x-min-width-decimals) [`frexp10`](#frexp10-x) [`get-pi`](#get-pi-p) [`log`](#log-x-b-p) [`max`](#max-x-y) [`min`](#min-x-y) [`mod`](#mod-x-y) [`num-lex`](#num-lex-n) [`num-sort`](#num-sort-v) [`pow`](#pow-x-y-p) [`range`](#range-x-y) [`round`](#round-x-p-dir) [`sin`](#sin-x-p) [`sum`](#sum-args) [`trunc`](#trunc-x) |
| [memo](#memo-persistent-memoization) | [`memo-apply`](#memo-apply-fname-args) [`memo-blob-call`](#memo-blob-call-fname-args) [`memo-call`](#memo-call-fname-args) [`memo-chmod-file`](#memo-chmod-file-filename-mode) [`memo-drop`](#memo-drop) [`memo-hash-file`](#memo-hash-file-filename) [`memo-io`](#memo-io-fname-args) [`memo-on`](#memo-on-dbfile-expr) [`memo-read-file`](#memo-read-file-filename) [`memo-write-file`](#memo-write-file-filename-data) |
| [native](#native) | [`name-apply`](#name-apply-func-name-argv) [`native-bound?`](#native-bound-var-name) [`native-call`](#native-call-var-name-args) [`native-eval`](#native-eval-str) [`native-flavor`](#native-flavor-var-name) [`native-name`](#native-name-var-name) [`native-origin`](#native-origin-var-name) [`native-value`](#native-value-var-name) [`native-var`](#native-var-var-name) [`set-native`](#set-native-var-name-value-retval) [`set-native-fn`](#set-native-fn-func-name-value-retval) |
| [peg](#peg-peg-parser-generator) | [`gen-lex`](#gen-lex-tokens) [`lex`](#lex-text-tokens) [`peg-*`](#peg--pf) [`peg-+`](#peg--pf) [`peg-?`](#peg--pf) [`peg-and`](#peg-and-pfs) [`peg-at`](#peg-at-pf) [`peg-c`](#peg-c-name-pf) [`peg-empty`](#peg-empty-caps) [`peg-not`](#peg-not-pf) [`peg-or`](#peg-or-pfs) [`peg-p`](#peg-p-in-out-caps) [`un-lex`](#un-lex-subj) |
| [repl](#repl-interactive-mode-for-scam) | [`repl`](#repl-build-dir-prompts) [`repl-ep`](#repl-ep-text-build-dir-is-quiet) |
| [string](#string-string-manipulation) | [`bytes-from-bytecodes`](#bytes-from-bytecodes-codes) [`gen-polysub`](#gen-polysub-froms-tos-input) [`string-from-bytecodes`](#string-from-bytecodes-codes) [`string-len`](#string-len-s) [`string-lower`](#string-lower-str) [`string-repeat`](#string-repeat-str-num) [`string-slice`](#string-slice-first-last-str) [`string-to-bytecodes`](#string-to-bytecodes-str) [`string-to-bytes`](#string-to-bytes-s) [`string-to-chars`](#string-to-chars-s) [`string-upper`](#string-upper-str) |
| [utf8](#utf8-utf-8-coding) | [`utf8-decode`](#utf8-decode-bytes) [`utf8-encode`](#utf8-encode-codes) |



# compile: SCAM Compilation

The following diagram summarizes the stages of compiling a SCAM
expression:

                 pos                 env
                  |                   |
                  v                   v
     text    +---------+   form   +------+    IL    +------+   exe
    -------->|  parse  |--------->|  c0  |--------->|  c1  |-------->
             +---------+          +------+          +------+
                  |                   |                 |    errors
                  v                   v                 +----------->
                 pos                 env

Each expression begins at a position "pos" (a numeric index into the
sequence of tokens in the subject text).  Parsing emits a "form" (an AST
node) and a position at which to look for subsequent expressions.

The compiler front end (c0) operates on a form and an environment (a set
of symbol bindings), and emits an IL node and a new environment, since
expressions (e.g. `declare` and `define`) can alter the environment for
subsequent expressions.

The compiler back end (c1) emits executable code (Make source) and a
(hopefully empty) vector of errors.  The form and IL data structures can
convey errors as well as successful results, so the previous stages do
not need a separate error output value.

## Build Directory

During compilation, intermediate files are created under a directory
called the "build directory".  Subsequent compilations with the same
object directory can proceed faster by reusing these results.  This
defaults to ".scam/" if NIL is passed.

No two compilations should occur at the same time (e.g. in different
instances of SCAM) using the same directory.

## Exports

##### `(build-program SRC-FILE EXE-FILE ?BUILD-DIR ?IS-QUIET)`

Compile a SCAM program.

SRC-FILE = name of the SCAM source file\
EXE-FILE = name of an executable file to create.\
ARGV = a vector to pass to the program's `main` function\
BUILD-DIR = nil, or the [object directory](#object-directory)\
IS-QUIET = non-nil to suppress progress messages.

On success, return `nil`.\
On failure, display message and return 1.


##### `(compile-text TEXT FILE ?ENV-IN ?BUILD-DIR ?IS-QUIET)`

Compile SCAM source code to a function.

TEXT = a string of SCAM source code containing a sequence of one or more
    expressions.\
FILE = the file from which the source was obtained; this will be
    available to the compiled code via `(current-file)`.\
ENV-IN = the environment (symbol definitions) visible to TEXT
   at the outset.  If nil, SCAM's default environment will be used.
   Otherwise, it must be a previously returned ENV-OUT value.\
BUILD-DIR = nil, or the [object directory](#object-directory).\
IS-QUIET = non-nil to suppress progress messages.

Result = `{ code: CODE, errors: ERRORS, env: ENV-OUT, requires: MODS }`

On success, the `code` member of the result contains a SCAM function that
will execute the compiled code, returning the value of the last
expression.

Example:

    > (define result
    +   (compile-text "(print 123) (require \"math\") (+ 1 2)" "--"))
    > (define f (dict-get "code" result))
    > (define out (f))
    123
    > out
    3


##### `(run-program SRC-FILE ARGV ?BUILD-DIR ?IS-QUIET)`

Compile and execute a SCAM program.

SRC-FILE = name of the SCAM source file\
ARGV = a vector to pass to the program's `main` function\
BUILD-DIR = nil, or the [object directory](#object-directory)\
IS-QUIET = non-nil to suppress progress messages.

On success, return `nil`.\
On failure, display message and return 1.


# core: General-Purpose Functions

The `core` library provides general-purpose functions, such as `eq?`, and
common operations on the fundamental data types in SCAM -- word lists,
vectors, and dictionaries.

## Exports

##### `(1+ N)`

Add one to N.  N must contain only decimal digits.


##### `(append ?A ?B ?C ?D ?E ?F ?G ?H ...OTHERS)`

Combine one or more (potentially empty) vectors, word lists, or
dictionaries.


##### `(assert COND)`

If COND is nil, display diagnostics and terminate execution.


##### `(assoc KEY VECV)`

Return the first vector in VECV whose first item is KEY.


##### `(assoc-initial PREFIX VEC)`

Return items that match PREFIX or begin with `(.. PREFIX " ")`.


##### `(assoc-vec KEY-VEC VECV)`

Return the first vector in VECV whose initial items match those in KEY-VEC.


##### `(butlast VEC)`

Return elements in vector VEC *except* for the last one.
This may also be applied to word lists or dictionaries.


##### `(concat-vec VEC ?DELIM)`

Concatenate strings in VEC, separating them with DELIM.


##### `(conj VEC ITEM)`

Add ITEM to end of vector VEC.


##### `(cons ITEM VEC)`

Add ITEM to the front of vector VEC.


##### `(dict-collate PAIRS)`

Create a new dictionary in which each key from DICT appears only once,
bound to a vector of all its values in DICT.


##### `(dict-compact DICT ?RESULT)`

Remove pairs in a dictionary that are preceded by pairs that share the
same KEY.


##### `(dict-find KEY DICT)`

Return the first pair matching KEY.  Unlike `dict-get`, this indicates
whether a match was found (every pair is non-nil).


##### `(dict-get KEY DICT ?DEFAULT)`

Return the value bound to KEY.  If more than one pair matches KEY, only
the first is returned.  If no pair is found, DEFAULT is returned.


##### `(dict-key {=KEY: _})`

Return the key portion of PAIR.


##### `(dict-keys DICT)`

Return a vector of keys from DICT.


##### `(dict-remove KEY DICT)`

Remove all pairs whose key portion is KEY from DICT.


##### `(dict-set KEY VALUE DICT)`

Bind KEY to VALUE in dictionary DICT, removing other entries for KEY.


##### `(dict-value {=_: VALUE})`

Return the value portion of PAIR.


##### `(dict-values DICT)`

Return a vector of values from DICT.


##### `(eq? A B)`

Return 1 if A and B are equal, nil otherwise.


##### `(expect A B)`

Compare A to B; if they are not equal, display diagnostics and terminate
execution.


##### `(expect-x A B FILE-LINE)`

Compare A to B, and if they are not equal, display diagnostics and
terminate execution.  This function is exported to enable users to
implement variants of `expect` in macros that supply their own FILE-LINE
value, obtained from `(current-file-line)`.

FILE-LINE = "file:line:" prefix for the diagnostic message.


##### `(fexpect A B)`

Like `expect`, but only the formatted versions of A and B are compared.
This accommodates only minor differences in the concrete layout that do
not affect the meaning of some types; chiefly, this ignores trailing
spaces in record values.


##### `(foldl F Z V)`

Apply the two-argument function F to all elements of V, starting at the
left with `(F Z <first>)`.  If V is empty, return Z.


##### `(foldr F Z V)`

Like `foldl`, but starting from the right with `(F <last> Z)`.


##### `(format STR)`

Return a SCAM literal that evaluates to VALUE, using the "friendliest"
representation available.


##### `(format-add FUNC)`

Register a formatting function that will be used by `format`.


##### `(identity A)`

Return A.


##### `(index-of VEC ITEM)`

Return the index of ITEM in VEC, or 0 if ITEM is not found.


##### `(indices LST)`

Return a vector of the indices (1, 2, ...) of words in word list (or vector)
LST.


##### `(intersperse VALUE VEC)`

Insert VALUE into VEC between every two adjacent items.  If VEC is empty,
the result is empty.  Otherwise, the result has one less than twice as
many elements as VEC.


##### `(last VEC)`

Return the last item in vector VEC.


##### `(memoize FUNCNAME)`

Memoize a function that accepts up to three arguments.


##### `(numeric? S)`

Return S if S is a valid numeric literal in SCAM, nil otherwise.


##### `(permute LST ZZ ?PREFIX)`

Return a list of all possible concatenations words from LST.

ZZ = a string of 0's that determine how long wach concatenation is.
     "" => 1 word, "0" => 2 word, "00" => 3 words, ...
PREFIX = a string to be prepended to each resulting concatenation.

The list is ordered by the indexes into LST.  Words that appear earlier
in the concatenations are more significant.


##### `(printf FMT ...VALUES)`

Display a message to stdout, followed by a newline.  See `vsprintf` for
handling of FMT and VALUES.


##### `(repeat-words V N)`

Return a list of N words, constructed by appending copies of V.
N must be an integer; if less than one, the result is empty.


##### `(reverse LIST)`

Reverse word list (or vector) LIST.


##### `(see SUBSTR STR)`

Return 1 if SUBSTR appears within STR.  Print a diagnostic otherwise.
This is intended for use in unit tests, as follows:

    (expect 1 (see SUBSTR STR))


##### `(select-vec FN LIST)`

Return a vector of all members of VEC for which (FN member) is non-nil.


##### `(select-words FN LIST)`

Return a list of words in LIST for which `(FN <word>)` is non-nil.


##### `(sort-by KEY-FUNC VALUES)`

Sort a vector VALUES in order of increasing `(KEY-FUNC i)` for each item i.


##### `(split DELIM STR)`

Split STR at each occurrence of DELIM.  Returns vector whose length is
one more than the number of occurrences of DELIM.


##### `(sprintf FMT ...VALUES)`

Like `vsprintf`, but values are provided as separate arguments.


##### `(strip VEC)`

Remove redundant spaces from a vector or list.

Consecutive spaces and tabs will be collapsed to a single space and
leading and trailing spaces will be removed.  Newline characters are
not disturbed.


##### `(trace-expect A B)`

Like `expect`, but evaluation of A and B is done with tracing enabled.


##### `(uniq VEC)`

Return the unique members of VEC *without* re-ordering.  The first
occurrence of each member is retains.  This can be applied to word lists
or dictionaries as well.

The `sort` function returns unique items and is much faster, but it does
not preserve ordering.


##### `(urange MIN MAX)`

Return a list of integers in the range MIN..MAX (inclusive).

MIN is a positive integer.
MAXi is a non-negative integer.

MIN and MAX must be in "plain" decimal format (no scientific notation or
decimals).

Memory requirements and execution time are proportional to MAX, not
(MAX - MIN).


##### `(vec-intersect A B)`

Return entries in vector A that also appear in vector B.
This may also be applied to dictionaries.


##### `(vec-or VEC)`

Return the first non-nil member of VEC.


##### `(vec-subtract A B)`

Return entries in vector A that do not appear in vector B.
This may also be used to applied to dictionaries.


##### `(vsprintf FMT VALUES)`

Expand FMT, replacing escape sequences with values from vector VALUES,
returning the resulting string.

The following escape sequences are supported:
-  `%s` -> value
-  `%q` -> `(format value)`


##### `(vsprintfx FMT VALUES CODES FMT-FN)`

Like `vsprintf`, but without any built-in notion of format
codes.  Instead it accepts two additional arguments:

CODES = list of supported format codes, e.g. "s q" for "%s" and "%q".\
FMT-FN = a function that formats a value, given a format code.  It is
   called as (FMT-FN CODE VALUE-VEC) where VALUE-VEC is a demoted value,
   and returns a vector of strings (which will be concatenated).


##### `(while PRED DO INITIAL)`

Recursively apply FN to VALUE until `(PRED result)` is nil, then return
the final value.


##### `(word-index? N)`

Return non-nil if N is safe to pass to WORD or WORDLIST.  This means it
consists only of decimal digits and is non-zero.


##### `(xor A B)`

Return the parameter that is not nil (unless both or none are nil).


# getopts: Command Line Options Parser

## Exports

##### `(getopts ARGV OPTS)`

Parse command line options.

ARGV = argument vector, as supplied to `main`\
OPTS = a string of option specifiers\
Result = a dictionary

`ARGV` is a vector of command line arguments that will be parsed as a
sequence of options, option values, and/or non-option arguments.
Generally, options may appear in any order, before or after non-option
arguments, but an argument of `--` indicates that all subsequent
arguments are to be treated as non-option arguments.

The result is a dictionary that maps each option name to a *vector* of
values, or nil.  If an option was specified more than once in `OPTS`, the
vector will hold one eleent for each occurrence.

The key `"*"` holds all non-option arguments.

If errors were encountered, the key `"!"` holds a vector of
`GetoptsError` records:

 - `(MissingArg OPT)` : Option specifier `OPT` describes an option that
   takes an argument, but the option appeared in the last element of
   `ARGV`, so its argument is missing.
 - `(BadOption ARG)` : Argument `ARG` began with "-" but did not match any
   option specifiers.

`OPTS` describes the options that may be provided and whether or not
values are expected with them.  Each word in `OPTS` is an "option
specifier"; it begins with `-` or `--`, followed by an option name, and
optionally ending with `=`.  Option names may not contain `%`, `!`, `*`,
or whitespace.

If an option specifier ends in `"="`, it indicates that the option (when
found in `ARGV`) will be followed by another argument that contains the
value to be associated with the option.  When an option specifier does
not end in `"="`, value `1` is supplied for each occurrence of the
option.

Example:

    > (getopts ["a" "-f" "-f" "--g" "x" "b c" "--" "-f"]
    +          "-f --g= -h")
    {*: ["a" "b c" "-f"], f: [1 1], g: "x"}


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

##### `(.. ...VALUES)`

This special form concatenates all of its arguments.


##### `(._. ...VALUES)`

This special form concatenates all of its arguments, separating them with
a single space character.


##### `(? FN ...ARGS)`

Call `(FN ...ARGS)` with tracing.

On entry to FN the function name and arguments are displaced, and on exit
the return value is displayed.  For example, in the REPL:

    > (define (f x) (+ x 1))
    > (f 1)
    2
    > (? f 1)
    --> (f "1")
    <-- f: "2"
    2


##### `(abspath PATHS)`

For each file name in PATHS, convert relative paths to absolute paths.
The file names do not need to refer to an existing file or directory.


##### `(addprefix PREFIX LIST)`

Add PREFIX to the start of each word in LIST.


##### `(addsuffix SUFFIX LIST)`

Add SUFFIX to the end of each word in LIST.


##### `(append-for (TARGET VECTOR) ...BODY)`

`append-for` is similar to `for` but it appends together all of the (vector)
values of BODY.  This is functionally similar to what is called `concat-map`
in some other languages.

    > (append-for x [[1 2] [3 4]]
    +     x)
    "1 2 3 4"
    > (append-for x [7 1 5 3]
    +    (if (> x 3) [x]))
    "7 5"


##### `(apply LAMBDA VEC)`

Call LAMBDA, passing as arguments the members of the vector VEC.

Example:

    > (apply nth [3 "a b c d"])
    "c"


##### `(at-exit FUNC ?UNIQUE)`

Add FUNC to a list of functions that will be run after `main` exits.
The earliest-registered functions will be called last.

If UNIQUE is non-nil and FUNC has already been registered, it will not be
added again.


##### `(basename PATHS)`

Remove the file extension from each word in PATHS.

See also: `path-basename`


##### `(begin ...EXPRS)`

Encloses a *block* of expressions.  A block is a sequence of expressions
that are evaluated in order.  The result of that last expression is
returned (or nil if no expressions are given).


##### `(case VALUE ...CLAUSES)`

Match a record value with its constructor.

Example:

    (case shape
       ((Square x) (^ x 2))
       ((Rectangle x y) (* x y))
       ((Triangle base height) (/ (* base height) 2))
       (else 0))

See the [language reference](reference.md#records) for more examples and
details.


##### `(concat ...VALUES)`

This special form concatenates all of its arguments.  This is
equivalent to `..`.


##### `(concat-for (TARGET VECTOR ?DELIM) ...BODY)`

`concat-for` is similar to `for` but it concatenates the values of BODY.
If `DELIM` is not present in the source, then it defaults to a single
space.

    > (concat-for (x [1 2 3] ";") (wordlist 1 x "a b c"))
    "a;a b;a b c"


##### `(cond ...CLAUSES)`

This special form expresses conditional execution of an arbitrary number
of possible blocks of code.  Each CLAUSE takes the form:

    (CONDITION ...EXPRS)

Example:

    (cond ((filter "foo" a)  "Found FOO")
          ((filter "bar" a)  "Found BAR")
          ((filter "baz" a)  (print "BAZ") "Found BAZ")
          (else              "Found NOTHING"))


##### `(current-env)`

This special form evaluates to the data structure that describes the lexical
environment at the point where it is invoked.  (See `gen.scm` for details of
this structure.)


##### `(current-file)`

This special form evaluates to the current file name.  Ordinarily, this
will refer to the location in the source file where the `(current-file)`
call appears, but when it appears within a macro it refers to the
location at which the macro was expanded.


##### `(current-file-line)`

This special form evaluates to `FILENAME:LINE:COL` describing the
"current" position in the program.  Ordinarily, this will refer to the
location in the source file where the `(current-file-line)` call appears,
but when it appears within a macro it refers to the location at which the
macro was expanded.


##### `(data NAME ...CTORS)`

Declare a record data type.

CTORS is a sequence of `(NAME ...ARGS)` expressions that declare data
constructors.  Records are discussed further in the [language
reference](reference.md#records).


##### `(declare ...FORMS)`

Declare a function, variable, or macro.

This special form takes two forms:

    (declare NAME FLAG...)             ; global data variable
    (declare (NAME PARAM...) FLAG...)    ; global function variable

The `declare` special form declares a global variable without assigning
a value.  This is usually used to access non-SCAM functions, or when
mutually recursive functions are defined.


##### `(define ...FORMS)`

Declare and define a function, variable, or macro.

This special form takes several forms:

    (define NAME FLAG... VALUE)              ; global data variable
    (define (NAME PARAM...) FLAG... BODY)    ; global function variable
    (define `NAME EXPR)                      ; symbol macro
    (define `(NAME PARAM...) FLAG... BODY)   ; compound macro

The `define` special form adds a name to the environment and associates
it with a definition.  The defined name will be visible to subsequent
expressions in the same block, and the definition supersedes any
previous definitions associated with the same name.

See `lambda` for descriptions of the `PARAM` syntax.

The `&public` flag may be included in `FLAG...`.  This indicates that the
symbol should be visible outside of the file in which it is defined.


##### `(defmacro (NAME ARGNAME) BODY)`

Declare an *executable macro*.  An executable macro is a function that
transforms syntax.  It takes one argument, a form, and returns a
different form.


##### `(demote VALUE)`

`demote` encodes any value as a word so that it may be embedded in a word
list.  It is used internally to construct vectors.


##### `(dir PATHS)`

Get the directory of each file path in word list PATHS.

    (dir "a/b c")  -->  "a/ ./"

See also: `notdir`, `path-dir`.


##### `(do-not-trace NAMES)`

Add NAMES to the list of functions that should not be instrumented when
`trace` or `tracing` are called.  In order to avoid undefined behavior,
we must avoid instrumenting functions *while* they are executing.


##### `(error MESSAGE)`

Terminate execution of the program with a non-zero status code, writing
`MESSAGE` to stderr.


##### `(filter PATTERNS LIST)`

Get all words in LIST that match PATTERNS.  Each word in PATTERNS is
compared against the words in LIST, and is considered a match when it is
equal, treating the first `%` character in a pattern as a wildcard.


##### `(filter-out PATTERNS LIST)`

Get all words in LIST that do *not* match PATTERNS.


##### `(filtersub PAT REPL STR)`

Replace PAT with REPL if STR matches PAT; return nil otherwise.


##### `(findstring SUB STR)`

If SUB occurs within STR, return SUB.  Otherwise return `nil`.


##### `(first VEC)`

Get the first element of VEC.


##### `(firstword LIST)`

Get the first word in LIST.


##### `(for (TARGET VECTOR) ...BODY)`

`for` iterates over items in a vector, constructing a new vector.

`BODY` is evaluated in the scope of the variables named in TARGET, which
can be either a symbol or a [destructuring target
](reference.md#destructuring).

Example:

    > (for (x [[1 2] [3 4]])
    +    (reverse x))
    [[2 1] [4 3]]


##### `(foreach (TARGET LIST ?DELIM) ...BODY)`

The `foreach` special form iterates over a list, evaluates BODY (a sequence
of expressions) once for each word, and constructs a new word list from the
results of each evaluation.

`BODY` is evaluated in the scope of variables named in TARGET, which can
be either a symbol or a [destructuring
target](reference.md#destructuring).

    > (foreach (x "1 2 3") (1+ x))
    "2 3 4"

`DELIM` specifies the separator that will appear between elements of the
*resulting* list.  This defaults to a single space when `DELIM` is
absent.  When it is present and evaluates to `nil`, all element results
will be concatenated.


##### `(if COND THEN-EXPR [ELSE-EXPR])`

This special form conditionally executes either THEN-EXPR or ELSE-EXPR.
First, COND is evaluated.  If non-nil, THEN-EXPR will be evaluated and used
are the value for the `if` expression.  Otherwise, if ELSE-EXPR is present
it is evaluated and used, and if not `nil` is used.


##### `(join LIST-A LIST-B)`

Concatenate the respective words in LIST-A and LIST-B.


##### `(lambda (...PARAMS) BODY)`

A `lambda` expression evaluates to a function value.

`...PARAMS` is zero or more *targets* that name the formal arguments,
followed by zero or more *optional* parameters and then perhaps one
*rest* parameter.

A target is a symbol (not beginning with `...` or `?`) or a
[destructuring target](reference.md#destructuring).

An optional parameter is a symbol that begins with `?`.  Optional
parameters may be omitted by callers.

A rest parameter is a symbol that begins with `...`.  Rest parameters
capture, as a vector, all subsequent arguments passed to the function.
Callers may omit any of the arguments captured by a rest parameter.

`BODY` is a block of one or more expressions (see [`begin`](#begin) )
that will be executed when the function is called. The initial environment
of `BODY` contains bindings for the arguments names to the values passed
to the function.


##### `(lastword LIST)`

Get the last word in LIST.


##### `(let (...BINDINGS) ...BODY)`

This special form assigns names to values.  BINDINGS is a sequence of
`(TARGET VALUE)` pairs.  Each TARGET can be either a symbol or a
[destructuring target](reference.md#destructuring).

BODY (a sequence of expressions) is evaluated in an environment in which
every name in each TARGET is bound to its corresponding value.  The
value of the last expression in BODY is returned.

`let` is implemented in terms of `lambda`, and is equivalent to:

    ((lambda (NAME1 NAME2 ...) BODY) (VALUE1 VALUE2 ...))


##### `(let& (...BINDINGS) ...BODY)`

Declare symbol macros.

`let&` is a "lazy" let.  It binds the names to symbol macros instead of
local variables.  It also differs from `let` in that each expression is
evaluated in the context of the previous bindings -- more like Scheme's
`let*`.  The following expression:

    (let& ((NAME EXPR))
      BODY)

... is equivalent to the following:

    (begin
      (declare `NAME EXPR)
      BODY)

Since `let&` constructs symbol macros, each bound expression is not
always evaluated exactly once, as with `let`.  Instead, each expression
is evaluated once each time its associated name is evaluated within
`BODY` -- perhaps zero times, perhaps many more.

The `let&` form generally has lower overhead than `let`, since it does
not involve an anonymous function call (as does `let`).


##### `(let-global (...BINDINGS) ...BODY)`

This form modifies the value of some number of global variables *during
the execution of BODY*. Afterwards, the original values are restored.
Note that, unlike `let` or `let&`, the variables must already be in
scope.

BINDINGS is a sequence of `(TARGET VALUE)` pairs.  Each TARGET can be
either a symbol or a [destructuring target](reference.md#destructuring).
Each name mentioned in TARGET must identify a previously declared global
variable.

This expression evaluates to the value of the last expression in BODY.


##### `(not A)`

Logically invert A.  If A is `nil`, return 1.  Otherwise, return `nil`.


##### `(notdir PATHS)`

Return the filename portion of each path in word list PATHS.

    (notdir "a/b c")  -->  "b c"

See also: `dir`, `path-notdir`.


##### `(nth INDEX VEC)`

Returns the value stored at index INDEX (1-based) in vector VEC.


##### `(nth-rest N LIST)`

Get the tail of LIST, starting at the Nth element (1-based).


##### `(or ...EXPRS)`

Expressions in `...EXPRS` are evaluated sequentially until a non-`nil`
value is encountered.  The first non-`nil` value is returned; if all are
`nil`, `nil` is returned.


##### `(patsubst PAT REPL LIST)`

Replace PAT with REPL in LIST.  Patterns use the first `%` as a wildcard,
and a corresponding `%` in REPL will be replace with what the wildcard in
PAT matched.  See the GNU make documentation for subtleties.


##### `(print ...VALUES)`

Concatenate all values and write them to `stdout`.


##### `(promote VALUE)`

`promote` reverses the encoding done by `demote`.


##### `(realpath PATHS)`

For each file name in PATHS, resolve the name to a form without `.` or
`..` path elements or symbolic links.  See the UNIX `realpath` C function
for details.


##### `(require MODULE &PRIVATE?)`

The `require` special form provides access to functionality defined in
other modules.  When evaluated, it will load and execute the module
(unless it has already been required elsewhere in the program.)  Symbols
that have been declared as `&public` in the top-level of the required
module are "imported" into the current environment, making those symbols
visible to expressions that follow the `require` form, up to the end of
the enclosing block.

`(require MODULE &private)` is intended for use by unit test modules.
This will import all symbols defined in the top-level of the required
module (not just the `&public` ones).  Additionally, the qualification
step will not be required.  (See "qualification", below.)

When `MODULE` is one of the [standard library names](#libraries), the
standard library will be supplied by the compiler.  Otherwise, `MODULE`
must end in `.scm` and identify as SCAM source file.  If it is a relative
path, it is treated as relative to the directory containing the requiring
file.  If no such file exists, the directories listed in `SCAM_LIBPATH`
(colon-delimited) are tried, in order, until a matching file is found.

When `MODULE` identifies a source file, that source file will be compiled
to determine its exports before compilation of the requiring module can
continue.  In turn, modules required by `MODULE` will also have to be
compiled in order to build `MODULE`, and so on.  Compilation results are
[cached](reference.md#cached-results) to avoid exponential build times.

**Qualification:** Each module source file may be accompanied by a
qualification test: a source file whose name is determined by adding `-q`
prior to the `.scm` extension.  For example, `foo-q.scm` is the
qualification test for `foo.scm`.  When a module is required, its
qualification test (if present) will be built and executed before
compilation of the requiring module continues.  If the qualification test
terminates with a non-zero exit code, it is considered a test failure and
compilation stops.  (Note that qualification test files must use the
`&private` flag when requiring the module they test in order to avoid a
dependency loop.)


##### `(rest VEC)`

Remove the first element from vector (or list) VEC.


##### `(rrest VEC)`

Remove the first two elements from vector (or list) VEC.


##### `(set NAME VALUE ?RETVAL)`

The `set` special form assigns a value to a previously declared global
variable.

NAME is given as a symbol, not a string. For example:

    (set var 12 nil)

The `set` expression returns `RETVAL` (or "" if `RETVAL` is not provided).


##### `(shell COMMAND)`

Execute COMMAND using the default shell as specified by the SHELL
variable (which is inherited from the SHELL environment variable).  The
data written to `stdout` by the COMMAND is captured, trailing newlines
are removed, and remaining newlines are converted to spaces, and the
result is returned by `shell`.


##### `(sort LIST)`

Sort the words in LIST in increasing lexicographical order.


##### `(subst FROM TO {FROM TO}... VALUE)`

This special form replaces substrings with replacement strings within the
given VALUE.  For example:

    > (subst 2 3 1 2 12)
    23


##### `(suffix PATHS)`

Return the file extensions of all file names in PATHS.


##### `(trace SPEC)`

Instrument functions for tracing as described by SPEC.

SPEC is as documented for `tracing`.

The return value is a list of the names of the instrumented functions,
which can be passed to `untrace` later.


##### `(tracing SPEC EXPR)`

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

See the [reference manual](reference.md#debugging) for examples.


##### `(untrace NAMES ?RETVAL)`

Remove instrumentation from functions listed in NAMES, or functions
matched by patterns in NAMES.


##### `(when COND ...BODY)`

This special form is equivalent to `(if COND (begin ...BODY))`.


##### `(wildcard PATTERNS)`

Get files that match PATTERNS.  Patterns are delimited by whitespace
characters, except that a whitespace character preceded by a backslash
character will be treated as literal.

All matches are returned as a word list; whitespace characters within a
matching file name are *not* escaped.  (Good luck with that.)


##### `(word N LIST)`

Return the Nth word in LIST.  If N is greater than the length of the
list, return `nil`.  If N is less than 1, terminate the program with an
error message.


##### `(wordlist A B LIST)`

Extract the words in LIST from index A to index B, inclusive.  Extraneous
spaces between words are retained.  If B is less than A, `nil` is returned.

If A is less than 1 or B is less than zero, the program will be
terminated with an error message.


##### `(words LIST)`

Return the number of words in LIST.


# io: File I/O and Shell Interaction

## Exports

##### `(chmod-file FILENAME MODE)`

Modify file mode.  Return nil on success, description on error.
MODE is as defined by the `chmod` command.


##### `(clean-path PATH)`

Remove redundant "." and ".." path elements and repeated "/" characters
from an absolute or relative path.  PATH may include whitespace.


##### `(cp-file SRC DST ?MAKE-DST-DIR)`

Copy file SRC to DST.  Return nil on success, description on error.


##### `(cp-file-atomic SRC DST ?MAKE-DST-DIR)`

Copy file SRC to DST.  Return nil on success, description on error.

Operation is atomic on POSIX file systems -- that is, if DST is opened
and read by another process, it will either see the previous contents of
DST or an entire copy of SRC (never a partial copy).  A temporary file
(in the same directory as DST, and with a name based on DST) will be used
for this purpose.


##### `(escape-path PATH)`

Escape a relative path or absolute path, so that the result is:
 - safe as a sub-directory (has no "..", and is not an absolute path)
 - a word (contains no spaces)
 - directly usable with `filter` (contains no "%")
 - safe to include in an "include ..." directive (no globbing chars)
 - safe to use in a make rule without escaping
 - unique (the encoding can be reversed)


##### `(file-exists? FILENAME)`

Return FILENAME if file FILENAME exists.  The `wildcard` built-in
function is a faster alternative, but it caches results and will not
reflect files created/deleted when the program is running.


##### `(fprintf FD FORMAT ...VALUES)`

Format text and write to a file descriptor, 0 through 8.  See `vsprintf`
for handling of FORMAT and VALUES.  Unlike `printf`, no trailing newline
is appended.


##### `(get-tmp-dir ?TMPL)`

Obtain a directory underneath the build directory, or, if not running
in the context of a build (as "-q.scm" files do) then return ".scam/".
If TMPL is given, a new directory is created under the build directory
and returned, using TMPL as a template for the `mktemp` command.

Result ends in "/".


##### `(getline PROMPT)`

Read one line from `stdin`.

Note: On MacOS, input lines longer than 1023 characters will cause bad
things to happen.


##### `(hash-file FILENAME)`

Return the hash of one file (see `hash-files`).


##### `(hash-files FILENAMES)`

Hash multiple files, returning a dictionary mapping file names to hash
values.  Hash values are 16 bytes long.  The selection of which hash
algorithm to use depends on what is available in PATH; it is guaranteed
to remain the same for the duration of the program's execution.


##### `(hash-output CMD-FMT ...ARGS)`

Execute shell command, hash what it writes to `stdout`, and return the
hash.

CMD-FMT = format string as per `io-vsprintf`
ARGS = arguments references by CMD-FMT


##### `(io-sprintf FMT ...ARGS)`

[See `io-vsprintf`.]


##### `(io-vsprintf FMT ARGS)`

Format a string, similarly to vsprintf, but with the following format
sequences supported:
   `%s` : the argument is output literally
   `%A` : the argument is quoted for a POSIX shell
   `%V` : the argument is treated as a vector of strings, each to be
          quoted as an argument to a POSIX shell
   `%F` : the argument is quoted for a POSIX shell using `quote-sh-file`.


##### `(mkdir-p DIR)`

Create directory DIR and parent directories, if necessary.


##### `(mv-file FROM TO)`

Move file FROM to TO.

On success, return nil.  On failure, return an error description.


##### `(path-basename PATH)`

Remove the extension from NAME.  This is like the Make builtin `basename`
except that this properly handles a name with whitespace and does not
operate on a list of words.


##### `(path-dir PATH)`

Return the directory portion of a path.  This is like the Make builtin
`dir` except that this properly handles a path with whitespace and does
not operate on a list of words.


##### `(path-notdir PATH)`

Return the file portion of a path.  This is like the Make builtin
`notdir` except that this properly handles a path with whitespace and
does not operate on a list of words.


##### `(pipe STDIN FMT ...ARGS)`

Execute a command, providing STDIN as input, capturing `stdout` and `stderr`.
Return the exit status and output.  The output is returned unmolested,
except that NUL bytes may result in truncated lines.

STDIN = bytes to provide as input to the command.  If nil, /dev/null is
   supplied.  The size of STDIN may be limited by the maximum command size.
FMT ...ARGS = arguments passed to `io-vsprintf` to construct the command.

Result = [STATUS STDOUT STDERR]


##### `(quote-sh-arg ARG)`

Quote argument ARG for POSIX shells.


##### `(quote-sh-file FILENAME)`

Quote FILENAME for POSIX shells and ensure it does not begin with '-'.


##### `(read-file FILENAME)`

Read the contents of file FILENAME and return it as a string.


##### `(read-lines FILENAME ?START ?END)`

Read contents of file FILENAME and return a vector of lines.  The number
of elements in the resulting vector is one more than the number of
newlines in the file.

Return `nil` if the file is not readable.


##### `(resolve-path DIR PATH)`

Combine a directory name and a path (relative or absolute).


##### `(save-blob DIR-NAME DATA)`

Write DATA to a file whose name is a hash of DATA, in directory DIR-NAME.
Return the path to the new file.


##### `(shell-lines CMD-FMT ...ARGS)`

Execute command, returning data written to `stdout` as a vector of
lines, split at "\n" characters.  To obtain the original output as one
string, do the following:

    (concat-vec RESULT "\n")

CMD-FMT = format string as per `io-vsprintf`
ARGS = arguments references by CMD-FMT

Note: Zero bytes in the output may result in truncated lines.


##### `(shellf CMD-FMT ...ARGS)`

Format a command using `io-vsprintf` and execute it using `shell`.


##### `(unescape-path LOC)`

Undo `escape-path`.


##### `(vfprintf FD FORMAT VALUES)`

See `fprintf`


##### `(write FD DATA)`

Write DATA to a file descriptor FD, 0 through 8.

Result is `nil` on success; non-nil if the file descriptor is bad.


##### `(write-file FILENAME DATA)`

Write DATA to file FILENAME.

On success, return nil.  On failure, return an error description.


##### `(write-file-atomic FILE-NAME DATA)`

Write DATA to file FILENAME.  The data is first written to a temporary
file which is then renamed to FILENAME, so that another process opening
the file will see either the old contents or (all of) the new contents.

On success, return nil.  On failure, return an error description.


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
(slightly more precise than 64-bit IEEE-754 binary floating point).

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

##### `(!= X Y)`

Return 1 if X != Y, nil otherwise.


##### `(* X Y)`

Return X * Y.


##### `(*~ X Y ?P)`

Compute X*Y to the [precision specified by P](#precision).  The result
will be within one unit of the least significant digit.


##### `(+ X Y)`

Return X + Y.


##### `(- X Y)`

Return X - Y.


##### `(/ X Y ?P)`

Return X / Y to the [precision specified by P](#precision).  The answer
will be rounded to the *nearest* unit in the least significant digit.


##### `(// X Y)`

Return floor(X / Y): the largest integer less than or equal to X/Y.


##### `(0- X)`

Negate a number.  This function assumes that X is a valid number; it does
not validate or canonicalize X.


##### `(< X Y)`

Return 1 if X < Y, nil otherwise.


##### `(<= X Y)`

Return 1 if X <= Y, nil otherwise.


##### `(= X Y)`

Return 1 if X = Y, nil otherwise.


##### `(> X Y)`

Return 1 if X > Y, nil otherwise.


##### `(>= X Y)`

Return 1 if X >= Y, nil otherwise.


##### `(^ X Y)`

Raise X to the power of Y.  Y must be an non-negative integer in "simple"
format (without a decimal point or E-notation).


##### `(abs X)`

Absolute value of a number.  This function assumes that X is a valid
number; it does not validate or canonicalize X.


##### `(atan M ?P)`

Return the arctangent of M to the [precision given by P](#precision).

The result is in the range (-π/2,π/2).


##### `(atan2 Y X ?P)`

Return the angle between the X axis and the line from the origin to the
point (X,Y).  (**Note** that Y is the first argument and X is the
second.)

The result is in the range (-π,π).

The [precision is given by P](#precision).


##### `(ceil X)`

Return the smallest integer greater than or equal to X.


##### `(cos X ?P)`

Compute the cosine of X to the [precision given by P](#precision).


##### `(exp X ?P)`

Calculate eˣ to the [precision given by P](#precision).


##### `(floor X)`

Return the greatest integer less than or equal to X.


##### `(format-fixed X ?MIN-WIDTH ?DECIMALS)`

Convert X to a fixed-point representation.

MIN-WIDTH = if non-nil, minimum field width.  Padding with spaces on the
   left will be added as necessary.\
DECIMALS = if non-nil, number of digits to the right of the decimal.


##### `(frexp10 X)`

Return the fraction and exponent portions of X.

Result = [M E] where X = M * 10ᴱ and E is an integer.
 - When X ≠ 0, 0.1 ≤ abs(M) < 1.
 - When X = 0, Result is [0 0].
 - When X is not a number, Result is nil.


##### `(get-pi ?P)`

Compute π to the [precision given by P](#precision).


##### `(log X ?B ?P)`

Calculate the logarithm of X to the [precision given by P](#precision).

B, if given, is the base; if nil, the natural logarithm of X will be
returned.

X and B must be be greater than 0.


##### `(max X Y)`

Return the larger of X or Y.


##### `(min X Y)`

Return the smaller of X or Y.


##### `(mod X Y)`

Return the remainder of floor(X/Y).

`(mod X Y)` is equal to `(- X (* (// X Y) Y))`.


##### `(num-lex N)`

Convert a number to a string.  The *lexical* sort order of multiple
results corresponds to the *numeric* sort order of the numbers.  In other
words, for two numbers X and Y, LX = `(num-lex X)`, and LY = `(num-lex
Y)`, then:

    (< X Y)  <==>  (sort [LX LY]) == [LX LY]

This can be used with `sort-by` to obtain numeric sort order.  E.g.:

    (sort-by (lambda (i) (num-lex (nth 2 i))) ...)


##### `(num-sort V)`

Sort elements of V by the numeric order of the first sub-element.  V may
be a simple list of numbers, or vector of vectors to be sorted by the
first element of each, or a dictionary to be sorted by the numeric value
of each key.


##### `(pow X Y ?P)`

Compute xʸ to the [precision given by P](#precision).

X must be non-negative.


##### `(range X Y)`

Return a vector of numbers ranging from X to Y.  X and Y must be integers
in "simple" format (without a decimal point or E-notation).


##### `(round X ?P ?DIR)`

Round X to the [precision given by P](#precision).

**Note:** P defaults to `"+0"`, unlike other functions accepting
precision values.

DIR is one of the following:
 - `"+"` ⇒ round up (ceiling)
 - `"-"` ⇒ round down (floor)
 - `"|"` ⇒ round towards zero (truncate)


##### `(sin X ?P)`

Compute the sine of X to the [precision given by P](#precision).


##### `(sum ...ARGS)`

Sum all numbers in ARGS.  Each argument may be a number, or a vector of
numbers, or a vector of vectors of numbers, and so on.


##### `(trunc X)`

Return the integer portion of X (rounding towards zero).


# memo: Persistent Memoization

The `memo` module implements persistent memoization, which caches the
results of function calls to avoid re-computing them.  "Persistent" means
that results are available not just during the invocation of a program,
but during future invocations as well.  Also, persistent memoization can
be applied to functions that perform IO.  SCAM uses persistent
memoization to rebuild SCAM programs accurately with with minimal
repeated work.  [The `memo` module functionality is not to be confused
with the `memoize` function in the `core` library.]

`memo-call` and `memo-apply` perform "memoized" invocations of functions.
They take a function and a set of arguments, and either invoke the
function with the arguments or return a result cached from a previous
invocation.  When a cached result is returned, it must also be the case
that any side effects (e.g. file writes) from the previous invocation are
in effect.

In order to meet these requirements, any IO operations performed by the
memoized functions must adhere to these requirements:

* The IO operation must be logged using a mechanism provided by this
  module. The logging mechanisms provided include `memo-io`, which wraps
  arbitrary IO operations, or `memo-write-file` and `memo-read-file`,
  which provide ready-to-use logged and replayable read and write
  operations.

* IO operations must be replayable.  Performing the operation two or more
  times will result in the same external state as performing it once
  does, and will return the same values each time.

* IO operations must not be mutually-conflicting during a
  [session](#sessions).  One IO operation may not have an effect on
  external state that would un-do the effect of another IO operation, or
  that would modify the return value of a previously-executed function

## Sessions

Memoized functions must be called within the context of a **session*.
The `memo-on` function initiates a **session**, evaluates a given
expression within the context of that session, and then terminates the
session.  (If called during a session, `memo-on` is a no-op.)  At the
beginning of a session, previously cached results are read from a
specified DB file.  At the end of the session, cached results are written
to the DB file.

It is assumed that any external state that affects the program will not
change during the session.  Using the compiler as an example, when source
files are modified during compilation, the compiler cannot guarantee
valid results.  This assumption means that each unique IO operation only
needs to be performed once during a session.

It is assumed that external state *may* change *between* sessions.  When
cached results from one session apply to a memoized call in a subsequent
session, and the results depend upon IO, those IO operations will be
replayed to check the validity of the cached results.  (When external
side effects are involved, replaying the IO serves the purpose of
re-effecting the change -- so replay does more than just validation.)

If a memoized function is called when a session is not active, it is a
fatal error.

## Exports

##### `(memo-apply FNAME ARGS)`

Call `(FNAME ...ARGS)`, or return cached results.


##### `(memo-blob-call FNAME ...ARGS)`

Memoize a function call that might return a large amount of data.  The
return value is stored as an blob in the memo DB directory instead of
being stored directly in the memo DB file.  We assume the blobs are
retained as long as the DB file.


##### `(memo-call FNAME ...ARGS)`

Call `(FNAME ...ARGS)`, or return cached results.


##### `(memo-chmod-file FILENAME MODE)`

Call `chmod-file`, logging the IO as a dependency.  MODE is formatted as
per the `chmod` command.


##### `(memo-drop)`

Discard memoization results from the current session, preventing them
from being persisted.


##### `(memo-hash-file FILENAME)`

Return hash of file contents, logging the IO transaction for playback.


##### `(memo-io FNAME ...ARGS)`

Perform an IO operation.  Log the IO as an additional input to the
function being recorded (if there is one).


##### `(memo-on DBFILE EXPR)`

Evaluate EXPR with memoization initialized.  If a session is not active,
begin one using DBFILE as the database.  DBFILE will be evaluated only
when a session is initiated.


##### `(memo-read-file FILENAME)`

Read data from FILENAME, logging the IO transaction for playback.


##### `(memo-write-file FILENAME DATA)`

Write DATA to FILENAME, logging the IO transaction for playback.  The
limit on the size of DATA is system-specific, but at least 60KB for
any data and 100KB for text files.


# Native

This section describes features that are not officially part of the SCAM
language.  They provide access to lower-level ("native") Make features,
and cannot be described completely without introducing implementation
complexities of SCAM and "alien" aspects of Make.  They may be useful
when using SCAM to interoperate tightly with non-SCAM Make code.

There is not a "native" module.  These features are
[intrinsics](#intrinsics), so `(require "native")` is not required.

## Exports

##### `(name-apply FUNC-NAME ARGV)`

Call the function variable whose native name is NAME with elements of
vector ARGV as arguments.


##### `(native-bound? VAR-NAME)`

Return 1 if native variable VAR-NAME has been assigned a value (as a data
or function variable), or `nil` if unassigned.


##### `(native-call VAR-NAME ...ARGS)`

Call a function, given its native name.


##### `(native-eval STR)`

Evaluate STR as GNU Make source.


##### `(native-flavor VAR-NAME)`

Describe the flavor of native variable VAR-NAME.  One of:

 * `"simple"`
 * `"recursive"`
 * `"undefined"`


##### `(native-name VAR-NAME)`

Get the "native name" of a SCAM global variable, function, or a built-in
function.


##### `(native-origin VAR-NAME)`

Describe the GNU make origin of native variable VAR-NAME.  Origin values
include: `"undefined"`, `"file"`, `"command line"`, `"automatic"`, ... .
See GNU Make docs for more.


##### `(native-value VAR-NAME)`

Return the value of native variable VAR-NAME.


##### `(native-var VAR-NAME)`

Evaluate a GNU Make variable.  Unlike `value`, native-var will trigger
expansion of recursive variables.


##### `(set-native VAR-NAME VALUE ?RETVAL)`

Assign VALUE to the native variable VAR-NAME as a data variable.  This
will replace any data or function variable binding for VAR-NAME.


##### `(set-native-fn FUNC-NAME VALUE ?RETVAL)`

Assign VALUE to the native variable FUNC-NAME as a function variable.
This will replace any data or function variable binding for FUNC-NAME.

Function variables cannot store values with complete fidelity -- the
value read back using `(native-value FUNC-NAME)` may not be exactly the
same.  However, it is guaranteed that when a SCAM function value is
assigned to a function, the variable's *behavior* (when called as a
function) will be as expected; furthermore, the value read back will be
functionally equivalent to the original SCAM function.

Function names cannot begin or end in spaces, and other limitations may
apply depending on the version of Make, but any valid SCAM symbol will
work.


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

##### `(gen-lex TOKENS)`

Return a function that converts text to a vector of symbols.  See `lex`.


##### `(lex TEXT TOKENS)`

Convert text to a vector of symbols.  TOKENS is a vector including
strings that are symbols of interest to the parsing functions and/or
strings that delimit those symbols.

See also `un-lex`.


##### `(peg-* PF)`

Construct PEG zero-or-more repetition.

Return a parsing function that will call PF sequentially until it fails,
and then succeed.  The resulting position will be what was returned by
the last successful match (or the starting position if none matched), and
the resulting captures will be all captures from successful matches,
appended.


##### `(peg-+ PF)`

Construct PEG one-or-more repetition.

See `peg-*`.


##### `(peg-? PF)`

Construct PEG optional match.

Return a parsing function that will always succeed, returning PF's results
if PF succeeds, or returning the starting position and no captures
otherwise.


##### `(peg-and ...PFS)`

Construct a PEG sequence.

Return a parsing function that will call parsing functions in PFS, one
after another, until one fails.  After each success, the next function
will be called with the remaining un-consumed symbols.  If all functions
in PFS succeed, the resulting position will will be the position
resulting from the final match, and the captures returned will be all
captures from all functions, appended.


##### `(peg-at PF)`

Construct PEG positive lookahead.

Return a parsing function that will succeed if PF succeeds, but which
will not consume any symbols or produce any captures.


##### `(peg-c NAME PF)`

Construct symbol capture.

Return a parsing function that succeeds when PF succeeds, and on success,
adds to the set of captures a dictionary pair whose key is NAME and whose
value is a vector of all matched symbols.


##### `(peg-empty ?CAPS)`

Construct PEG empty string.

Return a parsing function that always succeeds, consuming no symbols,
with captures CAPS.


##### `(peg-not PF)`

Construct PEG negative lookahead.

Return a parsing function that will succeed if PF fails and fail if PF
succeeds.


##### `(peg-or ...PFS)`

Construct a PEG prioritized choice.

Return a parsing function that will call parsing functions in PFS, one
after another until one succeeds.  It returns the first successful
result, or `nil` if none of them succeed.


##### `(peg-p IN ?OUT ?CAPS)`

Construct a PEG terminal symbol.

Return a parsing function that will succeed and consume a single symbol
when `(filter IN (filter-out OUT [SYMBOL]))` is true.  On success, its
captures will be CAPS.

Examples:

 - `(peg-p "%")` matches any symbol.
 - `(peg-p "%" nil [1])` matches any symbol, returning the capture [1].
 - `(peg-p "%" "\n")` matches any symbol except `"\n"`.


##### `(un-lex SUBJ)`

Recover original text from a string of symbols.


# repl: Interactive mode for SCAM

REPL mode reads lines of text from `stdin`, evaluating expressions and
displaying results and errors.  Typing `:q<RETURN>` or `Ctrl-D` will
exit REPL mode.  Typing `?<RETURN>` will show a command reference.

## Exports

##### `(repl ?BUILD-DIR ?PROMPTS)`

Enter REPL mode, and return to caller when the user exits with `:q` or
`Ctrl-D`.

BUILD-DIR = [build directory](#build-directory); `nil` for default.\
PROMPTS = [P1 P2]; P1 is shown when awaiting an expression; P2 is
  shown when awaiting completion of an expression.  If `nil`, default
  values will be used.


##### `(repl-ep TEXT ?BUILD-DIR ?IS-QUIET)`

Evaluate TEXT and print results and errors as REPL mode does.

TEXT = SCAM source text containing zero or more expressions.\
BUILD-DIR = [build directory](#build-directory); `nil` for default.\
IS-QUIET = When non-nil, suppresses compilation progress messaged.\

Result = non-nil on error.


# string: String Manipulation

The `string` library provides some common string manipulation functions
that are not available as builtins.

## Exports

##### `(bytes-from-bytecodes CODES)`

Convert byte codes into a vector of single-byte strings.

CODES = a vector of byte values (numbers from 1 to 255).  Non-negative
   values out of range will be ignored.  Negative values will result in a
   fatal error.


##### `(gen-polysub FROMS TOS ?INPUT)`

Construct a function that performs a number of substitutions.  The
returned function accepts one argument and returns the result of the
substitutions.

FROMS = a vector of substrings to be replaced.\
TOS = a vector of corresponding replacements.\
INPUT = if non-nil, a lambda expression that will transform the
  input string before the substitutions.


##### `(string-from-bytecodes CODES)`

Construct a string from a vector of byte values.  This reverses
`string-to-bytecodes`.

CODES = a vector of byte values (numbers from 1 to 255).  Non-negative
   values out of range will be ignored.  Negative values will result in
   fatal error.


##### `(string-len S)`

Return the number of *characters* in S.  UTF-8 encoding is assumed.


##### `(string-lower STR)`

Convert letters to lower case.  Only ASCII letters are supported.


##### `(string-repeat STR NUM)`

Return a string that consists of NUM copies of STR concatenated.


##### `(string-slice FIRST LAST STR)`

Extract a substring, given start and end character indices.  The range is
inclusive and 1-based.  When LAST is less than FIRST, an empty string is
returned.

FIRST = index of first character to include (1 or greater).\
LAST = index of last character to include (0 or greater).


##### `(string-to-bytecodes STR)`

Get the numeric indices of all *bytes* in STR.  The result is a vector of
numbers from 1 to 255.


##### `(string-to-bytes S)`

Get all bytes in STR.  The result is a vector of strings, each containing
one byte. `concat-vec` reverses this operation.


##### `(string-to-chars S)`

Get all characters in STR.  The result is a vector of strings, each
containing one character. `concat-vec` reverses this operation.

UTF-8 encoding of STR is assumed.  If STR is not a well-formed UTF8
string, the result will contain all *bytes* in STR (so `concat-vec` will
still reverse the operation) but the bytes may not grouped at character
boundaries.


##### `(string-upper STR)`

Convert letters to upper case.  Only ASCII letters are supported.


# utf8: UTF-8 Coding

## Exports

##### `(utf8-decode BYTES)`

Convert UTF-8 encoded bytes to Unicode code points.

BYTES = vector of numeric byte values\
Result = vector of code points

Example:

    > (string-to-bytecodes "π²")
    [207 128 194 178]
    > (utf8-decode [207 128 194 178])
    [960 178]


##### `(utf8-encode CODES)`

Convert Unicode character indices to UTF-8 encoded bytes.

CODES = vector of Unicode code points\
Result = vector of code points

Example:

    > (utf8-encode [960 178])
    [207 128 194 178]
    > (string-from-bytecodes [207 128 194 178])
    "π²"

