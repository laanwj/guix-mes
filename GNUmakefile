.PHONY: all check clean default distclean help install release
default: all

.config.make: configure GNUmakefile
	./configure

GUILE:=guile
export GUILE
OUT:=out
CFLAGS:=-std=c99 -O3 -finline-functions
#CFLAGS:=-std=c99 -O0
#CFLAGS:=-pg -std=c99 -O0
#CFLAGS:=-std=c99 -O0 -g

include .config.make
include make/install.make

CPPFLAGS+=-DPREFIX='"$(PREFIX)"'
CPPFLAGS+=-DVERSION='"$(VERSION)"'

export BOOT
ifneq ($(BOOT),)
CPPFLAGS+=-DBOOT=1
endif

-include .local.make

all: mes module/mes/read-0.mo

mes.o: GNUmakefile
mes.o: mes.c
mes.o: mes.c mes.h mes.i mes.environment.i mes.symbols.i
mes.o: lib.c lib.h lib.i lib.environment.i
mes.o: math.c math.h math.i math.environment.i
mes.o: posix.c posix.h posix.i posix.environment.i
mes.o: reader.c reader.h reader.i reader.environment.i

mini-mes: doc/examples/mini-mes.c GNUmakefile
	rm -f $@
	gcc -nostdlib --std=gnu99 -m32 -g -o mini-mes '-DVERSION="0.4"' $<
	chmod +x $@

micro-mes: doc/examples/micro-mes.c GNUmakefile
	rm -f $@
	gcc -nostdlib --std=gnu99 -m32 -o micro-mes '-DVERSION="0.4"' $<
	chmod +x $@

main: doc/examples/main.c GNUmakefile
	rm -f $@
	gcc -nostdlib --std=gnu99 -m32 -o main '-DVERSION="0.4"' $<
	chmod +x $@

clean:
	rm -f mes mes.o *.environment.i *.symbols.i *.environment.h *.cat a.out

distclean: clean
	rm -f .config.make

%.h %.i %.environment.i %.symbols.i: %.c build-aux/mes-snarf.scm
	build-aux/mes-snarf.scm $<

check: all guile-check mes-check

TESTS:=\
 tests/read.test\
 tests/base.test\
 tests/closure.test\
 tests/quasiquote.test\
 tests/let.test\
 tests/vector.test\
 tests/scm.test\
 tests/display.test\
 tests/cwv.test\
 tests/srfi-1.test\
 tests/srfi-13.test\
 tests/srfi-14.test\
 tests/optargs.test\
 tests/fluids.test\
 tests/catch.test\
 tests/psyntax.test\
 tests/let-syntax.test\
 tests/record.test\
 tests/match.test\
 tests/peg.test\
#

BASE-0:=module/mes/base-0.mes
MES-0:=guile/mes-0.scm
MES:=./mes
# use module/mes/read-0.mes rather than C-core reader
MES_FLAGS:=--load
export MES_FLAGS
MES_DEBUG:=1
#export MES_DEBUG

mes-check: all
	set -e; for i in $(TESTS); do ./$$i; done
	scripts/nyacc.mes
	scripts/nyacc-calc.mes

module/mes/read-0.mo: module/mes/read-0.mes mes 
	./mes --dump < $< > $@

dump: module/mes/read-0.mo

guile-check:
	set -e; for i in $(TESTS); do\
		$(GUILE) -s <(cat $(MES-0) module/mes/test.mes $$i);\
	done
	guile/nyacc.scm
	guile/nyacc-calc.scm

MAIN_C:=doc/examples/main.c
mescc: all $(MAIN_C)
	rm -f a.out
	scripts/mescc.mes $(MAIN_C) > a.out
	./a.out; r=$$?; [ $$r = 42 ]

guile-mescc: $(MAIN_C)
	rm -f a.out
	guile/mescc.scm $(MAIN_C) > a.out
	chmod +x a.out
	./a.out; r=$$?; [ $$r = 42 ]

paren: all
	scripts/paren.mes

GUILE_GIT:=$(HOME)/src/guile-1.8
GUILE_COMMIT:=ba8a709
psyntax-import: module/mes/psyntax.ss module/mes/psyntax.pp

module/mes/psyntax.%: $(GUILE_GIT)/ice-9/psyntax.%
	git --git-dir=$(GUILE_GIT)/.git --work-tree=$(GUILE_GIT) show $(GUILE_COMMIT):ice-9/$(@F > $@

help: help-top

install: all

release: all

help:
	@echo

define HELP_TOP
Usage: make [OPTION]... [TARGET]...

Targets:
  all             update everything
  check           run unit tests
  clean           remove all generated stuff
  dist            create tarball in $(TARBALL)
  distclean       also clean configuration
  mescc           compile cc/main.c to a.out
  install         install in $$(PREFIX) [$(PREFIX)]
  release         make a release
  update-hash     update hash in guix.scm
endef
export HELP_TOP
help-top:
	@echo "$$HELP_TOP"
