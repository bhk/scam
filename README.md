# SCAM

SCAM (Scheme Compiler Atop Make) is a compiler for a string-based
Lisp/Scheme dialect.  SCAM uses GNU Make 3.81 as its virtual machine.  Each
SCAM executable file -- including the SCAM Compiler itself -- is a makefile
and a `bash` script.  First invoked as a bash script, a SCAM program invokes
`make` to execute itself as a makefile.

SCAM supports arbitrary-precision floating point arithmetic.

For more information:

- A [short introduction to SCAM](intro.md).
- The [SCAM language reference](reference.md).
- [Functions, macros, and special forms in SCAM](libraries.md).
- Any of the programs in the [examples](examples) directory.


## Project Structure

SCAM is a self-hosting compiler.  The SCAM project consists of the SCAM
compiler sources and a "golden" compiler executable (at `bin/scam`) that is
used to compile them.

At the top level of the project tree, you can type `make` to compile the
SCAM compiler source.  See the makefile for more details.
