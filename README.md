# SCAM

SCAM stands for "Scheme Compiler Atop Make", and it is all of the following:

 * A Lisp/Scheme dialect. Some notable characteristics are:

    - SCAM is a string-oriented Lisp variant. Having only one data type
      presents interesting challenges to adapting the traditional lisp
      read/evaluate/print model and supporting first-class functions.
      It also means that SCAM provides automatic memory management without
      garbage collection (since values cannot reference other values).

    - SCAM combines lexical scoping and modularity. Included modules
      inject symbols into the lexical scope of the including module.

 * An implementation of the SCAM language that targets GNU Make.

   The SCAM compiler coverts SCAM source files to makefiles that use the
   text manipulation language facilities of GNU Make: `$(subst ...)`,
   `$(filter ...)`, `$(if ...)`, and so on. The SCAM compiler itself is
   written in SCAM, and executes in Make.

 * An easier way to develop complex makefiles.

   Makefiles can easily make use of functions written in SCAM. When writing
   in SCAM instead of Make syntax, programmers can benefit from:

     - Readable syntax, including the ability to include comments within a
       function body.

     - Local variables with lexical scoping.

     - First-class functions.

     - Compile-time warnings for many coding errors, such as references to
       undefined variables or functions.

     - Tracing facilities.

     - An interactive mode (REPL).


 * An esoteric programming language.

   SCAM's appeal lies not just in its power but in its limitations. When
   someone teaches a duck to play chess, we don't question *how well* it
   plays chess.


Take a look at an [interactive tour of SCAM](intro.md), or the [SCAM
language reference](reference.md), or just peruse the compiler source files.


## Project Structure

SCAM is a self-hosting compiler.

The SCAM project consists of the SCAM compiler sources and a "golden"
executable version of the compiler, which is used to compiler newer
versions.

The executable version of `scam` is a makefile which is also a valid bash
script.  When invoked as a bash script it invokes `make` to execute itself
as a makefile, packaging all command line arguments in a Make variable.

At the top level of the project tree, you can type `make` to compile the
SCAM compiler source. This invokes `bin/scam` to generate `.v1/scam`. Typing
`make v2` will invoke `.v1/scam` to build `.v2/scam`, typing `make v3` will
use `.v2/scam` to build `.v3/scam`.

`make promote` will replace the golden compiler, `bin/scam`, with `.v2/scam`
after verifying that `.v3/scam` and `.v2/scam` are identical.

