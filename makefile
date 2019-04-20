# bin/scam holds the "golden" compiler executable, which bootstraps compiler
# generation.  From the source files we build three different generations:
#
#    Compiler  Output  Runtime used by generated code
#    --------  ------  -----------------------------
#    golden    $A/*    golden (bundled in bin/scam)
#    $A/scam   $B/*    current ($B/runtime.min)
#    $B/scam   $C/*    current (bundled in $B/scam)
#
# Generated code will implicitly depend on a runtime, because the compiler's
# code generation phase emits references to runtime functions.  When we
# modify the compiler sources we can change the runtime code as long as we
# change the corresponding code generation code.  However, to support this
# we need to avoid mismatches at run time.  Code generated by the golden
# compiler must use the golden runtime, and code generates by a current
# compiler (e.g. a/scam) must use a runtime compiled from current sources.
# The combinations are summarized above, and these imply some complications
# that should be called out:
#
#  1. $A/scam contains and uses the golden runtime.  However, code that IT
#     GENERATES must use a current runtime.  As a result, it cannot call
#     functions that it compiles itself, so it cannot cupport REPL mode or
#     and exectuable macros.
#
#  2. In order to support `scam -o ...`, the a compiler must bundle a
#     current runtime into the generated program ... NOT the one bundled
#     within itself.  We name `runtime.scm` on the command line as a source
#     file, which tells the program to build, test, and bundle THAT runtime,
#     not its own bundled one.
#
#  3. runtime.scm presents its own potential conflict: it cannot use any
#     other runtime because of symbol conflicts, and so building the current
#     runtime sources with a golden compiler would be problematic.  As a
#     result, we do not build and run $A/runtime.min or $A/runtime-q.min.
#     Instead, we generate `b` binaries for these using the `a` compiler.

SHELL := /bin/bash

_@ = @
A = .out/a
B = .out/b
C = .out/c

#----------------------------------------------------------------
# Phony targets (the "UI")

.PHONY: a b c aok bok cok promote install clean

all: $C.ok

a: $A/scam
b: aok $B/scam
c: bok $C/scam

aok: $A.ok
bok: $B.ok
cok: $C.ok


# Replace the "golden" compiler with a newer one.
promote: cok ; $(_@)cp $B/scam bin/scam

install: ; cp bin/scam `which scam`

clean: ; rm -rf .out .scam */.out */.scam

bench: ; bin/scam --obj-dir .out/ -- bench.scm $(ARGS)

$$%: ; 	@true $(info $$$* --> "$(call if,,,$$$*)")

#----------------------------------------------------------------
# Docs

SCAMDOC = examples/scamdoc.scm

DOCLIBS = $(patsubst %,%.scm,compile core getopts io math peg repl string utf8 memo) \
          intrinsics.txt

docs: .out/libs.txt

promote-docs: .out/libs.txt ; cp .out/libs.txt libraries.md

.out/libs.txt: $(DOCLIBS) $(SCAMDOC) ; bin/scam $(SCAMDOC) -- -o $@ $(DOCLIBS)

#----------------------------------------------------------------

qarg = '$(subst ','\'',$1)'# ' balanced for emacs
target-line = $(shell sed -n '/guard,$1,/=' makefile)

# $(call guard,ID,COMMAND) : drop stdout and print message on failure
#   ID should be distinct from all other callers of guard
guard = ( $2 ) > /dev/null || (echo 'makefile:$(target-line): $@ failed:' && /bin/echo " $$ "$(call qarg,$2) && false)

build_message = @ printf '*** build $@\n' 

# It is not always necessary to keep $A/scam up to date with sources.  Any
# working $A/scam will suffice for building $B/scam except after `make
# promote`, or when `--boot` behavior has changed, or various other changes.
# Type `make a` to update $A/scam to reflect source changes.

ifneq "" "$(filter a,$(MAKECMDGOALS))"
$A/scam: *.scm
endif


# Remember that $A/scam and $B/scam are files under test, so we do
# not implicitly trust them to overwrite the existing output file,
# and so we delete the output file first.

$A/scam: *.scm bin/scam
	$(build_message)
	bin/scam -o $@ scam.scm
	touch $@

$B/scam: *.scm $A.ok
	$(build_message)
	$(_@) rm -f $@
	$A/scam -o $@ scam.scm --boot
	$(_@) test -f $@

$C/scam: *.scm $B.ok
	$(build_message)
	$(_@) rm -f $@
	$B/scam -o $@ scam.scm --boot
	$(_@) test -f $@

# v1 tests:
#  run: validates code generation
#
$A.ok: $A/scam test/run.scm
	@ echo '... test $A/scam'
	$(_@) SCAM_LIBPATH='.' $A/scam -o .out/ta/run test/run.scm --boot
	$(_@)    .out/ta/run
	$(_@) touch $@


$B.ok: $B-o.ok $B-x.ok $B-e.ok $B-i.ok $B-io.ok
	$(_@) touch $@


# v2 tests:
#   dash-o: test program generated with "scam -o EXE"
#     Uses a bundled file, so $A/scam will not always work.
#   dash-x: compile and execute source file, passing arguments

$B-o.ok: $B/scam test/*.scm
	@ echo '... test scam -o EXE FILE'
	$(_@) $B/scam -o .out/tb/using test/using.scm
	$(_@) .out/tb/using
	$(_@) $B/scam -o .out/tb/dash-o test/dash-o.scm
	$(_@) .out/tb/dash-o 1 2 > .out/tb/dash-o.out
	$(_@) $(call guard,BO2,grep 'result=11:2' .out/tb/dash-o.out)
	$(_@) touch $@


$B-x.ok: $B/scam test/*.scm
	@ echo '... test scam FILE ARGS...'
	$(_@) SCAM_TRACE='%conc:c' $B/scam --obj-dir .out/tbx/ -- test/dash-x.scm 3 'a b' > .out/tb/dash-x.out
	$(_@) $(call guard,BX1,grep '9:3:a b' .out/tb/dash-x.out)
	$(_@) $(call guard,BX2,grep ' 4 .*conc' .out/tb/dash-x.out)
	$(_@) touch $@


$B-e.ok: $B/scam
	@ echo '... test scam -e EXPR'
	$(_@) $B/scam --obj-dir .out/tbx -e '(print [""])' -e '[""]' > .out/tb/dash-e.out
	$(_@) $(call guard,BE1,cat .out/tb/dash-e.out | tr  '\n' '/' | grep '\!\./\[\"\"\]' -)
	$(_@) touch $@


$B-i.ok: $B/scam
	@ echo '... test scam [-i]'
	$(_@) $(call guard,BI1,$B/scam <<< $$'(^ 3 7)\n:q\n' 2>&1 | grep 2187)
	$(_@) touch $@


$B-io.ok: $B/scam
	@ echo '... test io redirection'
	$(_@) $(call guard,BIO1,$B/scam -e '(write 1 "null")(write 7 "stdout\n")' 7>&1 >/dev/null | grep ^stdout)
	$(_@) $(call guard,BIO1,$B/scam -e '(write 2 "stderr")' 2>&1 >/dev/null | grep stderr)
	$(_@) touch $@


# To verify the compiler, we ensure that $B/scam and $C/scam are identical.
# $A/scam differs from bin/scam because it is built from newer source files.
# $A and $B differ because they are built by different compilers, but they
# should *behave* the same because they share the same sources ... so $B and
# $C should be identical, unless there is a bug.  We exclude exports from
# the comparison because they mention file paths, which always differ.
#
$C.ok: $B.ok $C/scam
	@echo '... compare B and C'
	$(_@)grep -v Exports $B/scam > $B/scam.e
	$(_@)grep -v Exports $C/scam > $C/scam.e
	$(_@)diff -q $B/scam.e $C/scam.e
	$(_@) touch $@
