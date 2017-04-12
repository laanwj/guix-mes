SHELL:=bash
QUIET:=@

default: all

MES_DEBUG:=1
CFLAGS:=--std=gnu99 -O0 -g
OUT:=out

SUBDIRS:=\
 module\
 src\
 scaffold\
 scripts\
 tests\
#

include make/common.make
-include .local.make

help: help-top

install: all
release: all

help:
	@echo

define HELP_TOP
Usage: make [OPTION]... [TARGET]...

Targets:
  all               update everything
  check             run unit tests
  clean             remove all generated stuff
  dist              create tarball in $(TARBALL)
  distclean         also clean configuration
  maintainer-clean  also clean expensive targets [$(strip $(MAINTAINER-CLEAN))]
  mescc             compile cc/main.c to a.out
  install           install in $$(DESTDIR)$$(PREFIX) [$(DESTDIR)$(PREFIX)]
  release           make a release
  update-hash       update hash in guix.scm
endef
export HELP_TOP
help-top:
	@echo "$$HELP_TOP"
