An Interactive Introduction to SCAM
====

To enter interactive mode, run bin/scam:

    % bin/scam
    SCAM interactive mode. Type '?' for help.
    >

The `>` prompt indicates that it is waiting to accept a SCAM expression.
Let's define a function.

    > (define (hello ?name)
    +    (print "Hello " (or name "world") "!"))

The `+` prompt indicates that more input is required to complete the SCAM
expression. The `print` function is a SCAM builtin that outputs its
arguments to stdout.  The `?` before `name` indicates that it is an optional
argument.  Now that we have defined a function, let's call it.

    > (hello)
    Hello world!
    > (hello "from SCAM")
    Hello from SCAM!

The semantics of SCAM have been chosen to allow seamless inter-op with Make,
in which all values are strings. GNU Make builtin functions are directly
available from SCAM code. Functions defined in SCAM are callable from Make,
and vice versa. To demonstrate this we can call the `eval` builtin, which
executes a makefile fragment that in turn calls our SCAM function:

    > (eval "$(call hello,from Make)")
    Hello from Make!

Having only one data type does not limit the language as you might at first
imagine.  In SCAM we can represent complex data types -- vectors,
associative arrays, and first-class functions -- as strings.  For example:

    > (for f [- + * ^]
    +    (f 3 2))
    [1 5 6 9]

SCAM's compiled nature allows for more useful error reporting than what Make
provides:

* syntax error reporting

        > (define (f a b)
        +    [a b)]
        at 2:8: unmatched ")"
           [a b)]
               ^

* undefined function/variable references

        > (infoo 1)
        at 1:2: undefined symbol: "infoo"
        (infoo 1)
         ^

        > (info xxx)
        at 1:7: undefined variable "xxx"
        (info xxx)
              ^

* counting arguments to functions (at compile time)

        > (subst 1 2)
        at 1:2: (subst {FROM TO}+ STR) accepts 2n+1 arguments, not 2
        (subst 1 2)
         ^


If you type an expression that returns a value (other than the empty string)
REPL displays the returned value, formatting it as valid SCAM source in the
simplest form:

    > 12
    12
    > "1 2"
    "1 2"
    > (cons "a b" [2 3])
    ["a b" 2 3]

The global variable `*1` holds the most recent return value, and `*2` holds
the previous return value.

    > 7
    7
    > 9
    9
    > (* *1 *2)
    63

For more details, proceed to the [language reference](reference.md).
