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
mes.o: mes.c mes.h mes.i mes.environment.i mes.symbols.i
mes.o: cache.c cache.h cache.i cache.environment.i
mes.o: define.c define.h define.i define.environment.i
mes.o: display.c display.h display.i display.environment.i
mes.o: lib.c lib.h lib.i lib.environment.i
mes.o: math.c math.h math.i math.environment.i
mes.o: posix.c posix.h posix.i posix.environment.i
mes.o: quasiquote.c quasiquote.h quasiquote.i quasiquote.environment.i
mes.o: reader.c reader.h reader.i reader.environment.i
mes.o: string.c string.h string.i string.environment.i
mes.o: type.c type.h type.i type.environment.i

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
 tests/cwv.test\
 tests/psyntax.test\
 tests/let-syntax.test\
 tests/record.test\
 tests/match.test\
#

BASE-0:=module/mes/base-0.mes
MES-0:=guile/mes-0.scm
MES:=./mes
# use module/mes/read-0.mes rather than C-core reader
MES_FLAGS:=--load
export MES_FLAGS

mes-check: all
	set -e; for i in $(TESTS); do ./$$i; done

dump: all
	./mes --dump < module/mes/read-0.mes > read-0.mo

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

paren: all
	scripts/paren.mes

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
