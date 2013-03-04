_@=@

.PHONY: v1 v2 v3 v3ok promote clean

v1: ; $(_@)bin/scam -o .v1/scam scam.scm

v2: v1 ; $(_@).v1/scam -o .v2/scam scam.scm

v3: v2 ; $(_@).v2/scam -o .v3/scam scam.scm

# To verify the compiler, we ensure that .v2/scam and .v3/scam are
# identical.  .v1/scam may differ from bin/scam because it is built from
# newer source files.  v1 and v2 may *differ* because they are built by
# different compilers, but they should *behave* the same because they share
# the same sources ... so v2 and v3 should be identical, unless there is a
# bug.  We exclude exports from the comparison because they mention file
# paths, which always differ.
v3ok: v1 v2 v3
	$(_@)grep -v Exports .v2/scam > .v2/scam.e
	$(_@)grep -v Exports .v3/scam > .v3/scam.e
	$(_@)diff -q .v2/scam.e .v3/scam.e

promote: v3ok
	$(_@)cp .v2/scam bin/scam

clean:
	rm -rf .v1 .v2 .v3

$$%:
	@true $(info $$$* --> "$(call if,,,$$$*)")
