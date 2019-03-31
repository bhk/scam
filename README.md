# SCAM

SCAM (Scheme Compiler Atop Make) is a compiler for a string-based
Lisp/Scheme dialect.  SCAM uses GNU Make 3.81 as its virtual machine by
compiling SCAM programs to makefiles.

For more information:

- A [short introduction to SCAM](intro.md).
- The [SCAM language reference](reference.md).
- [Functions, macros, and special forms in SCAM](libraries.md).
- Any of the programs in the [examples](examples) directory.


## Project Structure

SCAM is a self-hosting compiler.

The SCAM project consists of the SCAM compiler sources and a "golden"
compiler executable that is used to compile them.

The executable version of `scam` is a makefile which is also a valid bash
script.  When invoked as a bash script it invokes `make` to execute itself
as a makefile, packaging all command line arguments in a Make variable.

At the top level of the project tree, you can type `make` to compile the
SCAM compiler source.  See the makefile for more details.
