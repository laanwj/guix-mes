SHELL:=bash
export SHELL
.export: SHELL
QUIET:=@

default: all

MES_DEBUG:=1
CFLAGS:=--std=gnu99 -O0 -g --include libc-gcc.c
HEX2_FLAGS:=--LittleEndian --Architecture 1 --BaseAddress 0x1000000
OUT:=out

SUBDIRS:=\
 module\
 src\
 scaffold\
 scripts\
 stage0\
 tests\
 tinycc\
#

include make/common.make
-include .local.make

build-scripts:
	make --dry-run MES=$(OUT)/mes CC= CC32= GUILE= MES_BOOTSTRAP=1 > $(OUT)/make.sh
	make --dry-run MES=$(OUT)/mes CC= CC32= GUILE= MES_BOOTSTRAP=1 | tail +$(wc -l make.sh) > $(OUT)/make-check.sh
	make --dry-run MES=$(OUT)/mes CC= CC32= GUILE= MES_BOOTSTRAP=1 | tail +$(wc -l make.sh) > $(OUT)/make-install.sh

	make --dry-run > $(OUT)/make-dev.sh
	make --dry-run | tail +$(wc -l make.sh) > $(OUT)/make-dev-check.sh
	make --dry-run | tail +$(wc -l make.sh) > $(OUT)/make-dev-install.sh

help: help-top

install: all
release: all

help:
	@echo

define HELP_TOP
Usage: make [OPTION]... [TARGET]...

Targets:
  all               update everything
  build-scripts     generate build scripts
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
