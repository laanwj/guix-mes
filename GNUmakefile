.PHONY: all check clean default distclean help install release
default: all

.config.make: configure GNUmakefile
	./configure

OUT:=out
CFLAGS:=-std=c99 -O3 -finline-functions
#CFLAGS:=-std=c99 -O0
#CFLAGS:=-pg -std=c99 -O0
#CFLAGS:=-std=c99 -O0 -g

export BOOT
ifneq ($(BOOT),)
CFLAGS+=-DBOOT=1
endif

include .config.make
-include .local.make
include make/install.make


all: mes

mes.o: mes.c
mes.o: mes.c mes.environment.h mes.environment.i mes.symbols.i
mes.o: define.c define.environment.h define.environment.i
mes.o: lib.c lib.environment.h lib.environment.i
mes.o: math.c math.environment.h math.environment.i
mes.o: posix.c posix.environment.h posix.environment.i
mes.o: quasiquote.c quasiquote.environment.h quasiquote.environment.i
mes.o: string.c string.environment.h string.environment.i
mes.o: type.c type.environment.h type.environment.i

clean:
	rm -f mes mes.o *.environment.i *.symbols.i *.environment.h *.cat a.out

distclean: clean
	rm -f .config.make

%.environment.h %.environment.i %.symbols.i: %.c build-aux/mes-snarf.scm
	build-aux/mes-snarf.scm $<

check: all guile-check mes-check

TESTS:=\
 tests/base.test\
 tests/closure.test\
 tests/quasiquote.test\
 tests/let.test\
 tests/vector.test\
 tests/scm.test\
 tests/cwv.test\
 tests/record.test\
 tests/let-syntax.test\
 tests/psyntax.test\
 tests/match.test\
#

BASE-0:=module/mes/base-0.mes
MES-0:=guile/mes-0.scm
MES:=./mes

mes-check: all
	set -e; for i in $(TESTS); do ./$$i; done

guile-check:
	set -e; for i in $(TESTS); do\
		guile -s <(cat $(MES-0) module/mes/test.mes $$i);\
	done
	set -e; for i in $(TESTS); do\
		guile -s <(cat $(MES-0) $$(scripts/include.mes $$i | grep -Ev 'let.mes|quasiquote.mes|match.mes|base-0|loop-0|psyntax-|srfi-0') $$i);\
	done

MAIN_C:=doc/examples/main.c
mescc: all
	scripts/mescc.mes $(MAIN_C)
	./a.out

mescc.cat: $(MES-0) module/mes/lalr.mes module/mes/elf.mes module/mes/libc-i386.mes $(shell scripts/include.mes scripts/mescc.mes | grep -Ev '/mes/|/srfi/')
	echo '(compile)' | cat $^ - > $@

guile-mescc: mescc.cat
	cat $(MAIN_C) | guile -s $^ > a.out
	chmod +x a.out
	./a.out

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
