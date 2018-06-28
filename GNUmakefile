# Mes --- Maxwell Equations of Software
# Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of Mes.
#
# Mes is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Mes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Mes.  If not, see <http://www.gnu.org/licenses/>.

GUILE_FLAGS:=--no-auto-compile -L . -L guile -C . -C guile

include .config.make

.config.make:
	./configure --prefix=$(PREFIX)

PHONY_TARGETS:= all all-go build check clean clean-go default doc help install install-info man\
cc mes mes-gcc mes-tcc

.PHONY: $(PHONY_TARGETS)

default: all

all: build doc

build:
	./build.sh

cc:
	build-aux/build-cc.sh

mes-gcc:
	build-aux/build-cc32.sh

mes-tcc:
ifdef TCC
	CC32=$(TCC) build-aux/build-cc32.sh
else

$(warning skipping mes-tcc: no tcc)
endif

mes:
	build-aux/build-mes.sh

clean:
	git clean -dfx

all-go:
	build-aux/build-guile.sh

clean-go:
	rm -f $(shell find . -name '*.go')

check:
	./check.sh


install:
	./install.sh

.config.make: ./configure

seed: all-go mes-gcc mes-tcc
ifdef TCC
	cd $(TINYCC_SEED) && MES_PREFIX=$(PWD) ./refresh.sh
endif
	cd $(MES_SEED) && git reset --hard HEAD
	MES=$(GUILE) GUILE=$(GUILE) SEED=1 build-aux/build-mes.sh
	cd $(MES_SEED) && MES_PREFIX=$(PWD) ./refresh.sh
	MES=$(GUILE) GUILE=$(GUILE) SEED=1 build-aux/build-mes.sh
	cp lib/x86-mes/elf32-header.hex2\
           lib/x86-mes/x86.M1\
           lib/x86-mes/libc+tcc.S\
           lib/x86-mes/libc.S\
           lib/x86-mes/crt1.S\
           lib/x86-mes/libgetopt.S\
           $(MESCC_TOOLS_SEED)/libs
	cd $(MESCC_TOOLS_SEED) && MES_PREFIX=$(PWD) ./bootstrap.sh

doc/version.texi: doc/mes.texi GNUmakefile
	(set `LANG= date -r $< +'%d %B %Y'`;\
	echo "@set UPDATED $$1 $$2 $$3"; \
	echo "@set UPDATED-MONTH $$2 $$3"; \
	echo "@set EDITION $(VERSION)"; \
	echo "@set VERSION $(VERSION)") > $@

doc: doc/version.texi
ifdef MAKEINFO
doc: info
else
$(warning skipping info: no makeinfo)
endif

ifdef HELP2MAN
doc: man
else
$(warning skipping man: no help2man)
endif

info: doc/mes.info

doc/mes.info: doc/mes.texi doc/version.texi GNUmakefile
	$(MAKEINFO) -o $@ -I doc $<

install-info: info

man: doc/mes.1 doc/mescc.1

doc/mes.1: src/mes.gcc-out
	MES_ARENA=10000000 $(HELP2MAN) $< > $@

src/mes.gcc-out:
	$(MAKE) cc

doc/mescc.1: src/mes.gcc-out scripts/mescc
	MES_ARENA=10000000 $(HELP2MAN) $< > $@

define HELP_TOP
Usage: make [OPTION]... [TARGET]...

Targets:
  all             update everything
  all-go          update .go files
  cc              update src/mes.gcc-out
  doc             update documentation
  mes-gcc         update src/mes.mes-gcc-out
  mes-tcc         update src/mes.mes-tcc-out
  mes             update src/mes
  check           run unit tests
  clean           run git clean -dfx
  clean-go        clean .go files
  info            update info documentation
  install         install in $(PREFIX)
  install-info    install info docs in $(PREFIX)/share/info
  seed            update mes-seed in $(MES_SEED)
endef
export HELP_TOP
help:
	@echo "$$HELP_TOP"

include build-aux/export.make
