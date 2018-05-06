# Tests of make.
#
#  * make -f sample.mk X='$(info CALLED?)'
#
#    `CALLED?` is displayed.  Make command-line variables are expanded by
#    `make` whether the makefile consumes them or not.
#
#  * X='$(info VAR) make -f sample.mk
#
#    `CALLED?` is not displayed.
#

all:
	@ true

