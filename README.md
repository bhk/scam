# SCAM

SCAM (Scheme Compiler Atop Make) is a compiler for a string-based
Lisp/Scheme dialect.  A SCAM program can be interpreted as a script, or
compiled to an executable file.

SCAM uses GNU Make 3.81 as its virtual machine.  Each SCAM executable file
consists mostly of GNU Make variable definitions and begins with a
BASH-compatible preamble that invokes Make with itself as the makefile.
This, and its stringly-typed semantics, facilitate interoperation with
Make-based projects.

SCAM includes libraries for IO, string manipulation, arbitrary-precision
floating point arithmetic, PEG-based parsing, compilation and interpretation
of SCAM sources, and other capabilities not generally available within GNU
Make 3.81.

For more information:

- A [short introduction to SCAM](intro.md).
- The [SCAM language reference](reference.md).
- [SCAM standard libaries](libraries.md).
- Any of the programs in the [examples](examples) directory.


## Project Structure

SCAM is a self-hosting compiler.  The SCAM project consists of the SCAM
compiler sources and a "golden" compiler executable (at `bin/scam`) that is
used to compile them.

At the top level of the project tree, you can type `make` to compile the
SCAM compiler sources.  See the makefile for more details.
