# Tests of make.
#
#  * make -f vars.mk X='$(info CALLED?)'
#
#    `CALLED?` is displayed.  Make command-line variables are expanded by
#    `make` whether the makefile consumes them or not.
#
#  * X='$(info VAR) make -f vars.mk
#
#    `CALLED?` is not displayed.
#
#  * make -f vars.mk filerec
#
#    SHELL and MAKEFLAGS appear as "file" and "recursive" variables.
#

all:
	@ true

showvars = $(foreach v,$(.VARIABLES),$(if $(filter $1,$(origin $v)$(flavor $v)),$(info $v : $(origin $v), $(flavor $v))))

filerec:
	@ true $(call showvars,filerec%)

