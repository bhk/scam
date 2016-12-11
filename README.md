# SCAM

SCAM stands for "Scheme Compiler Atop Make", and it is all of the following:

 * A Lisp/Scheme dialect. Some notable characteristics are:

    - All values are immutable character strings.  A rich set of subordinate
      data types are projected onto this set: numbers, vectors, hash maps,
      algebraic types, and first class (anonymous) functions.

    - SCAM combines lexical scoping and modularity.  Included modules
      inject symbols into the lexical scope of the including module.

 * An implementation of the SCAM language that targets GNU Make.

   The SCAM compiler coverts SCAM source files to makefiles that use the
   text manipulation language facilities of GNU Make 3.81: `$(subst ...)`,
   `$(filter ...)`, `$(if ...)`, and so on. The SCAM compiler itself is
   written in SCAM, and executes in Make.

   GNU Make presents an odd set of building blocks.  While it does provide a
   number of string manipulation functions, it does not provide primitives
   for addressing characters by index, or taking the length of a string.  It
   lacks arithmetic operators and even basic comparison operators, although
   those can be constructed.

 * An easier way to develop complex makefiles.

   Makefiles can easily make use of functions written in SCAM. When writing
   in SCAM instead of Make syntax, programmers can benefit from:

     - Readable syntax, including the ability to include comments within a
       function body.

     - Local variables with lexical scoping.

     - Rich data structuring and pattern matching.

     - An interactive command-line interpreter.

     - First-class functions.

     - Hygienic macros.

     - Compile-time warnings for many coding errors, such as references to
       undefined variables or functions.

     - Tracing facilities. (See [trace.scm](trace.scm).)

 * An esoteric programming language.

   SCAM's appeal lies not just in its power but in its limitations. When
   someone teaches a duck to play chess, we don't question *how well* it
   plays chess.


Take a look at an [introduction to SCAM](intro.md), the [SCAM language
reference](reference.md), or some [examples](examples). When looking at the
SCAM sources, [scam.scm](scam.scm) is a good top-down starting point, and
[core.scm](core.scm) is a good bottom-up starting point.


## Project Structure

SCAM is a self-hosting compiler.

The SCAM project consists of the SCAM compiler sources and a "golden"
compiler executable that is used to compile them.

The executable version of `scam` is a makefile which is also a valid bash
script.  When invoked as a bash script it invokes `make` to execute itself
as a makefile, packaging all command line arguments in a Make variable.

At the top level of the project tree, you can type `make` to compile the
SCAM compiler source.  See the makefile for more details.
