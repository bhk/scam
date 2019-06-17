An Interactive Introduction to SCAM
====

To enter interactive mode, run `bin/scam`:

    % bin/scam
    SCAM v2.0 interactive mode. Type '?' for help.
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

Having only one data type does not limit the language as you might at first
imagine.  In SCAM we can represent complex data types -- vectors,
associative arrays, and first-class functions -- as strings.  For example:

    > (for f [- + * ^]
    +    (f 3 2))
    [1 5 6 9]

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
