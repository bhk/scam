An Interactive Introduction to SCAM
====

To enter interactive mode, run bin/scam:

    % bin/scam
    SCAM interactive mode. Type '?' for help.
    >

The `>` prompt indicates that it is waiting to accept a SCAM expression.
Let's define a function.

    > (define (hello name)
    +    (print "Hello " (or name "world") "!"))

The `+` prompt indicates that more input is required to complete the SCAM
expression. The `print` function is a SCAM builtin that outputs its
arguments to stdout. Now that we have defined a function, let's call it.

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
    "1 5 6 9"

SCAM's compiled nature allows for more error reporting than Make programmers
are used to:

* syntax error reporting

        > (define (f a b)
        +    [a b)]
        Line 2: unmatched ")"
        at:     [a b*)*]

* undefined function/variable references

        > (infoo 1)
        Line 1: Undefined symbol: infoo
        at: (*infoo* 1)
        > (info xxx)
        Line 1: Undefined variable: xxx
        at: (info *xxx*)

* counting arguments to builtin functions (at compile time)

        > (subst 1 2)
        Line 1: Wrong number of arguments: 'subst' accepts 3
        at: (*subst* 1 2)


If you type an expression that returns a value (other than the empty string)
REPL displays the returned value, formatting it as valid SCAM source in the
simplest form:

    > 12
    12
    > "1 2"
    "1 2"
    > (concat 1 " " [2 3])
    [1 "2 3"]

The global variable `*1` holds the most recent return value, and `*2` holds
the previous return value.

    > 7
    7
    > 9
    9
    > (* *1 *2)
    63

