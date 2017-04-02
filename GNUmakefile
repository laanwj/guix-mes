SHELL:=bash

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

MACHINE:=$(shell $(CC) -dumpmachine)
##CC:=gcc
LIBRARY_PATH=:$(dir $(shell type -p ldd))../lib
CC:=LIBRARY_PATH=$(LIBRARY_PATH) gcc

CPPFLAGS+=-I.
CPPFLAGS+=-DDATADIR='"$(DATADIR)/"'
CPPFLAGS+=-DDOCDIR='"$(DOCDIR)/"'
CPPFLAGS+=-DMODULEDIR='"$(MODULEDIR)/"'
CPPFLAGS+=-DPREFIX='"$(PREFIX)/"'
CPPFLAGS+=-DVERSION='"$(VERSION)"'

export BOOT
ifneq ($(BOOT),)
CPPFLAGS+=-DBOOT=1
endif

-include .local.make

all: mes module/mes/read-0.mo module/mes/read-0-32.mo

ifeq ($(MES_BOOTSTRAP),mes-mini-mes)
all: mes-mini-mes
endif

S:=
mes.o$(S): GNUmakefile
mes.o$(S): mes.c
mes.o$(S): mes.c mes.h mes.i mes.environment.i mes.symbols.i
mes.o$(S): lib.c lib.h lib.i lib.environment.i
mes.o$(S): math.c math.h math.i math.environment.i
mes.o$(S): posix.c posix.h posix.i posix.environment.i
mes.o$(S): reader.c reader.h reader.i reader.environment.i
mes.o$(S): gc.c gc.h gc.i gc.environment.i
mes.o$(S): vector.c vector.h vector.i vector.environment.i

clean:
	rm -f mes *.o *.o-32 *.environment.i *.symbols.i *.environment.h *.cat a.out
	rm -f mes-32
	rm -f cons-mes m main micro-mes mini-mes t tiny-mes
	rm -f guile-cons-mes guile-m guile-main guile-micro-mes guile-mini-mes guile-t guile-tiny-mes
	rm -f module/mes/*.mo

distclean: clean
	rm -f .config.make

%.h %.i %.environment.i %.symbols.i: %.c build-aux/mes-snarf.scm
	build-aux/mes-snarf.scm $<

check: all guile-check mes-check mescc-check

TESTS:=\
 tests/read.test\
 tests/base.test\
 tests/closure.test\
 tests/quasiquote.test\
 tests/let.test\
 tests/scm.test\
 tests/display.test\
 tests/cwv.test\
 tests/math.test\
 tests/vector.test\
 tests/srfi-1.test\
 tests/srfi-13.test\
 tests/srfi-14.test\
 tests/optargs.test\
 tests/fluids.test\
 tests/catch.test\
 tests/psyntax.test\
 tests/pmatch.test\
 tests/let-syntax.test\
 tests/guile.test\
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

export C_INCLUDE_PATH

mes-check: all
	set -e; for i in $(TESTS); do MES_MAX_ARENA=20000000 ./$$i; done

mini-mes-check: all mini-mes
	$(MAKE) mes-check MES=./mini-mes

module/mes/read-0.mo: module/mes/read-0.mes mes
	rm -f $@
	./mes --dump < $< > $@

dump: module/mes/read-0.mo

mes.o$(S): mes.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

mes$(S): mes.o$(S)
	$(CC) $(CFLAGS) $(LDFLAGS) $< -o $@

ifeq ($(MACHINE),i686-unknown-linux-gnu)
mes-32: mes
	ln -f $< $@
else
mes$(S)-32: GNUmakefile
mes$(S)-32: mes.c gc.c lib.c math.c posix.c vector.c
	guix environment --system=i686-linux --ad-hoc gcc-toolchain -- bash -c 'make mes-32 S=-32 CC=i686-unknown-linux-gnu-gcc LIBRARY_PATH=$${PATH%%/bin:*}/lib'
endif

module/mes/read-0-32.mo: module/mes/read-0.mes
module/mes/read-0-32.mo: module/mes/read-0.mo
module/mes/read-0-32.mo: mes-32
	rm -f $@
	MES_MINI=1 ./mes-32 --dump < $< > $@

module/mes/tiny-0-32.mo: module/mes/tiny-0.mes mes-32
	rm -f $@
	MES_TINY=1 ./mes-32 --dump < $< > $@

guile-check:
	set -e; for i in $(TESTS); do\
		$(GUILE) -s <(cat $(MES-0) module/mes/test.mes $$i);\
	done

t-check: t
	./t

mescc-check: t-check
	rm -f a.out
	guile/mescc.scm scaffold/t.c > a.out
	chmod +x a.out
	./a.out

%.h %.i %.environment.i %.symbols.i: scaffold/%.c build-aux/mes-snarf.scm GNUmakefile
	build-aux/mes-snarf.scm --mini $<

mini-%.h mini-%.i mini-%.environment.i mini-%.symbols.i: %.c build-aux/mes-snarf.scm GNUmakefile
	build-aux/mes-snarf.scm --mini $<

mini-mes.h mini-mes.i mini-mes.environment.i mini-mes.symbols.i: scaffold/mini-mes.c build-aux/mes-snarf.scm GNUmakefile
	build-aux/mes-snarf.scm --mini $<

mini-mes: mini-mes.h mini-mes.i mini-mes.environment.i mini-mes.symbols.i
mini-mes: gc.c mini-gc.h mini-gc.i mini-gc.environment.i
mini-mes: vector.c mini-vector.h mini-vector.i mini-vector.environment.i
mini-mes: mlibc.c mstart.c
mini-mes: GNUmakefile
mini-mes: module/mes/read-0-32.mo
mini-mes: scaffold/mini-mes.c
	rm -f $@
	gcc -nostdlib --std=gnu99 -m32 -g -o $@ $(CPPFLAGS) $<
	rm -f mes.o
	chmod +x $@

guile-mini-mes: mini-mes.h mini-mes.i mini-mes.environment.i mini-mes.symbols.i
guile-mini-mes: gc.c mini-gc.h mini-gc.i mini-gc.environment.i
guile-mini-mes: vector.c mini-vector.h mini-vector.i mini-vector.environment.i
guile-mini-mes: mlibc.c mstart.c
guile-mini-mes: GNUmakefile
guile-mini-mes: module/mes/read-0-32.mo
guile-mini-mes: scaffold/mini-mes.c
	rm -f $@
	guile/mescc.scm $< > $@ || rm -f $@
	chmod +x $@

mes-mini-mes: mini-mes.h mini-mes.i mini-mes.environment.i mini-mes.symbols.i
mes-mini-mes: gc.c mini-gc.h mini-gc.i mini-gc.environment.i
mes-mini-mes: vector.c mini-vector.h mini-vector.i mini-vector.environment.i
mes-mini-mes: mlibc.c mstart.c
mes-mini-mes: GNUmakefile
mes-mini-mes: module/mes/read-0-32.mo
mes-mini-mes: scaffold/mini-mes.c
	rm -f $@
#	MES_FLAGS= MES_DEBUG=1 scripts/mescc.mes $< > $@ || rm -f $@
	MES_FLAGS= MES_DEBUG=1 scripts/mescc.mes $< > $@
	chmod +x $@

mes-hello: GNUmakefile
mes-hello: mlibc.c mstart.c
mes-hello: module/mes/read-0-32.mo
mes-hello: scaffold/hello.c
	rm -f $@
	MES_FLAGS= MES_DEBUG=1 scripts/mescc.mes $< > $@ || rm -f $@
	chmod +x $@

cons-mes: module/mes/tiny-0-32.mo
cons-mes: scaffold/cons-mes.c GNUmakefile
	gcc -nostdlib --std=gnu99 -m32 -g -o $@ $(CPPFLAGS) $<
	chmod +x $@

guile-cons-mes: module/mes/tiny-0-32.mo
guile-cons-mes: scaffold/cons-mes.c
	rm -f $@
	guile/mescc.scm $< > $@ || rm -f $@
	chmod +x $@

tiny-mes: module/mes/tiny-0-32.mo
tiny-mes: scaffold/tiny-mes.c GNUmakefile
	gcc -nostdlib --std=gnu99 -m32 -g -o $@ $(CPPFLAGS) $<
	chmod +x $@

guile-tiny-mes: module/mes/tiny-0-32.mo
guile-tiny-mes: scaffold/tiny-mes.c
	rm -f $@
	guile/mescc.scm $< > $@ || rm -f $@
	chmod +x $@

m: scaffold/m.c GNUmakefile
	gcc -nostdlib --std=gnu99 -m32 -g -o $@ $(CPPFLAGS) $<
#	gcc --std=gnu99 -g -o $@ $(CPPFLAGS) $<
	chmod +x $@

guile-m: scaffold/m.c
	rm -f $@
	guile/mescc.scm $< > $@ || rm -f $@
	chmod +x $@

malloc: scaffold/malloc.c GNUmakefile
	gcc -nostdlib --std=gnu99 -m32 -g -o $@ $(CPPFLAGS) $<
	chmod +x $@

guile-malloc: scaffold/malloc.c
	guile/mescc.scm $< > $@ || rm -f $@
	chmod +x $@

micro-mes: scaffold/micro-mes.c GNUmakefile
	rm -f $@
	gcc -nostdlib --std=gnu99 -m32 -o $@ $(CPPFLAGS) $<
	chmod +x $@

guile-micro-mes: scaffold/micro-mes.c
	guile/mescc.scm $< > $@ || rm -f $@
	chmod +x $@

main: doc/examples/main.c GNUmakefile
	rm -f $@
	gcc -nostdlib --std=gnu99 -m32 -o $@ $(CPPFLAGS) $<
	chmod +x $@

guile-main: doc/examples/main.c
	guile/mescc.scm $< > $@ || rm -f $@
	chmod +x $@

t: mlibc.c
t: scaffold/t.c GNUmakefile
	rm -f $@
	gcc -nostdlib --std=gnu99 -m32 -g -o $@ $(CPPFLAGS) $<
	chmod +x $@

guile-t: scaffold/t.c
	guile/mescc.scm $< > $@ || rm -f $@
	chmod +x $@

MAIN_C:=doc/examples/main.c
mescc: all $(MAIN_C)
mescc: doc/examples/main.c all
	rm -f a.out
	MES_DEBUG=1 scripts/mescc.mes $< > a.out
	./a.out; r=$$?; [ $$r = 42 ]

guile-mescc: doc/examples/main.c
	rm -f a.out
	guile/mescc.scm $< > a.out
	chmod +x a.out
	./a.out; r=$$?; [ $$r = 42 ]

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
