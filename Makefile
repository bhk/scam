# bin/scam holds the "golden" compiler executable, which bootstraps compiler
# generation.  From the source files we build three different generations:
#
#    Compiler  Generates  Runtime used by generated code
#    --------  ---------  -----------------------------
#    golden    $A/*    	  golden (bundled in bin/scam)
#    $A/scam   $B/*    	  latest (from runtime.scm)
#    $B/scam   $C/*    	  latest (from runtime.scm)
#
# See notes.txt for more on these build phases.

SHELL := /bin/bash

_@ = @
A = .out/a
B = .out/b
C = .out/c

#----------------------------------------------------------------
# Phony targets (the "UI")

.PHONY: default a b c aok bok cok promote install clean bench tags


default: $B/done

# If $B/scam is the same as bin/scam, we can stop without further validation.
# Otherwise, we proceed to build and validate $C/scam as a test of $B/scam.
#
$B/done: $B/scam bok; @(diff -q bin/scam $B/scam > /dev/null && echo 'Stopping: .out/b/scam == bin/scam!' || $(MAKE) $C/done) && touch $@

$C/done: $C/scam cok; @diff -q $B/scam $C/scam && touch $@


all: $C.ok docs
	$(_@) diff -q bin/scam .out/b/scam  || echo '** "make promote" to update bin/scam'
	$(_@) diff -q libraries.md .out/libs.txt || echo '** "make promote-docs" to update libraries.md'


a: $A/scam
b: $B/scam
c: $C/scam

aok: $A.ok
bok: $B.ok
cok: $C.ok


# Replace the "golden" compiler with a newer one.
promote: cok ; $(_@)cp $B/scam bin/scam

install: ; cp bin/scam `which scam`

clean: ; rm -rf .out .scam */.out */.scam

bench: ; bin/scam --build-dir .out/ -- bench.scm $(ARGS)

$$%: ; 	@true $(info $$$* --> "$(call if,,,$$$*)")

tags: .TAGS
.TAGS: *.scm */*.scm ; etags *.scm */*.scm -o .TAGS

#----------------------------------------------------------------
# Docs

SCAMDOC = examples/scamdoc.scm

DOCLIBS = $(patsubst %,%.scm,compile core getopts io math peg repl string utf8 memo trace) \
          intrinsics.txt native.txt

docs: .out/libs.txt

promote-docs: .out/libs.txt ; cp .out/libs.txt libraries.md

.out/libs.txt: $(DOCLIBS) $(SCAMDOC) ; bin/scam $(SCAMDOC) -- -o $@ $(DOCLIBS)

#----------------------------------------------------------------

mf = $(word 1,$(MAKEFILE_LIST))
show-line = sed -n '/$1/{=;p;}' $(mf) | sed 'N;s/\n/: /;s/^/$(mf):/' >&2

# $(call ||,UNIQUEID): BASH clause to display file and line on failure
|| = || ( $(call show-line,call ||.$1) && false )

build_message = @ printf '*** build $@\n' 

foo: ; false $(call ||,FOO)


# Don't pollute user's ~/.scam
export SCAM_BUILD_DIR=.out/builddir/

# Remember that $A/scam and $B/scam are files under test, so we do
# not implicitly trust them to overwrite the existing output file,
# and so we delete the output file first.

$A/scam: bin/scam *.scm
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
#  run: validates code generation, object file loading, etc.
#
$A.ok: $A/scam test/*.scm
	@ echo '... test $A/scam'
	$(_@) SCAM_LIBPATH='.' $A/scam -o .out/ta/run test/run.scm --boot --build-dir '.out/ta/scam build dir/'   $(call ||,AOK1)
	$(_@) .out/ta/run   $(call ||,AOK2)
	$(_@) [[ -d '.out/ta/scam build dir/' ]]  $(call ||,AOK3)
	$(_@) touch $@


$B.ok: $B-o.ok $B-x.ok $B-e.ok $B-i.ok $B-io.ok $B-trace.ok
	$(_@) touch $@


# v2 tests:
#   dash-o: test program generated with "scam -o EXE"
#     Uses a bundled file, so $A/scam will not always work.
#   dash-x: compile and execute source file, passing arguments

$B-o.ok: $B/scam test/*.scm
	@ echo '... test scam -o EXE FILE'
	$(_@) $B/scam -o .out/tb/using test/using.scm  $(call ||,Bo1)
	$(_@) .out/tb/using $(call ||,Bo2)
	$(_@) $B/scam -o .out/tb/dash-o --build-dir '.out/tb/a b c/' test/dash-o.scm  $(call ||,Bo3)
	$(_@) [[ -d '.out/tb/a b c/' ]]  $(call ||,Bo4)
	$(_@) .out/tb/dash-o 1 2 > .out/tb/dash-o.out  $(call ||,Bo5)
	$(_@) grep -q 'result=11:2' .out/tb/dash-o.out  $(call ||,Bo6)
	$(_@) ( ! $B/scam test/bug.scm 2>&1 ) | grep -q assertion.failed  $(call ||,Bo7)
	$(_@) touch $@


$B-x.ok: $B/scam test/*.scm
	@ echo '... test scam FILE ARGS...'
	$(_@) $B/scam --build-dir .out/tbx/ -- test/dash-x.scm 3 'a b' > .out/tb/dash-x.out $(call ||,Bx1)
	$(_@) grep -q '9:3:a b' .out/tb/dash-x.out $(call ||,Bx2)
	$(_@) touch $@


$B-e.ok: $B/scam
	@ echo '... test scam -e EXPR'
	$(_@) $B/scam --build-dir .out/tbx -e '(print [""])' -e '[""]' > .out/tb/dash-e.out $(call ||,Be1)
	$(_@) cat .out/tb/dash-e.out | tr  '\n' '/' | grep -q '\!\./\[\"\"\]' - $(call ||,Be2)
	$(_@) touch $@


$B-i.ok: $B/scam
	@ echo '... test scam [-i]'
	$(_@) $B/scam <<< $$'(^ 3 7)\n:q\n' 2>&1 | grep -q 2187 $(call ||,Bi1)
	$(_@) touch $@


$B-io.ok: $B/scam
	@ echo '... test io redirection'
	$(_@) $B/scam -e '(write 1 "null")(write 7 "stdout\n")' 7>&1 >/dev/null | grep -q ^stdout $(call ||,Bio1)
	$(_@) $B/scam -e '(write 2 "stderr")' 2>&1 >/dev/null | grep -q stderr $(call ||,Bio2)
	$(_@) touch $@


$B-trace.ok: $B/scam test/tracing*
	@ echo '... test tracing'
	$(_@) SCAM_TRACE='%' $B/scam test/tracing.scm f > $B/tracing-f.out $(call ||,Bt1)
	$(_@) diff -q test/tracing-f.out $B/tracing-f.out $(call ||,Bt2)
	$(_@) $B/scam test/tracing.scm g > $B/tracing-g.out $(call ||,Bt3)
	$(_@) diff -q test/tracing-g.out $B/tracing-g.out $(call ||,Bt4)
	$(_@) SCAM_TRACE='f:c' $B/scam test/tracing.scm f > $B/tracing-fc.out $(call ||,Bt5)
	$(_@) diff -q test/tracing-fc.out $B/tracing-fc.out $(call ||,Bt6)
	$(_@) touch $@


# To verify the compiler, we ensure that $B/scam and $C/scam are identical.
# $A/scam always differs from bin/scam because it uses a different namespace.
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
