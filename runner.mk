# Otherwise, `echo -n` doesn't work
SHELL:=$(firstword $(shell which bash))

#$(info runner: *obj-dir*=$(*obj-dir*))
#$(info runner: MOD=$(MOD))
#$(info runner: SHELL=$(SHELL))

^testload = $(eval include $(*obj-dir*)$1.o)1
$(if $(call ^testload,runtime),)
$(call ^start,$(MOD),~main)
