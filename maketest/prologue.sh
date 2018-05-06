#!/bin/bash
:; for v in "${@//!/!1}" ; do v=${v// /!0} ; v=${v//	/!+}; a[++n]=${v:-!.} ; done ; SCAM_ARGS=${a[*]} exec make -Rr --no-print-directory -j ${SCAM_JOBS:-9} -f"$0"

.PHONY: A B

# 1. ./prologue.sh generates a warning.
#        make[1]: warning: -jN forced in submake: disabling jobserver mode.
#     ...which seems to be triggered by the sequence "make -> bash -> make -jN".
#
# 2. Swap "A" and "B" below, and the warning goes away.
#

A: ; ../bin/scam -e '(+ 1 2)'
B: ; SCAM_ARGS='-e (+!01!02)' $(MAKE) -f ../bin/scam
