# GNU Mes --- Maxwell Equations of Software
# Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of GNU Mes.
#
# GNU Mes is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Mes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

GUILE_FLAGS:=--no-auto-compile -L . -L module -C . -C module

cleaning-p:=$(filter clean%, $(MAKECMDGOALS))$(filter %clean, $(MAKECMDGOALS))

ifndef cleaning-p
ifndef config.make
config.make:=.config.make
include $(config.make)
$(config.make):
	./configure --prefix=$(prefix)
endif
endif

PHONY_TARGETS:=\
 ${top_builddest}src/mes\
 TAGS\
 all-go\
 all\
 build\
 check\
 clean-go\
 clean\
 default\
 dist\
 distclean\
 doc\
 dvi\
 gcc\
 generate-ChangeLog\
 help\
 html\
 info\
 install-dvi\
 install-html\
 install-pdf\
 install-ps\
 install-strip\
 install\
 installcheck\
 installdirs\
 maintainer-clean\
 man\
 mes-gcc\
 mes-tcc\
 mes\
 mostlyclean\
 pdf\
 ps\
 uninstall\
#

.PHONY: $(PHONY_TARGETS)

default: all

all: build doc

build:
	./build.sh

gcc:
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

# Mes does not cache anything on the file system; therefore clean
distclean: clean
mostlyclean: clean
maintainer-clean: clean

TAGS:
	etags lib/*.c lib/*/*.c src/*.c include/*.h include/sys/*.h

all-go:
	build-aux/build-guile.sh

clean-go:
	rm -f $(shell find . -name '*.go')

check:
	./check.sh

# Mes does not feature post-install checks yet, so we're great!
installcheck:
	true

install: ${top_builddest}src/mes
	./install.sh

uninstall:
	./uninstall.sh

$(config.make): configure

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

${top_builddest}doc/version.texi: doc/mes.texi GNUmakefile
	@mkdir -p $(@D)
	(set `LANG= date -r $< +'%d %B %Y'`;\
	echo "@set UPDATED $$1 $$2 $$3"; \
	echo "@set UPDATED-MONTH $$2 $$3"; \
	echo "@set EDITION $(VERSION)"; \
	echo "@set VERSION $(VERSION)") > $@

doc: ${top_builddest}doc/version.texi
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

info: ${top_builddest}doc/mes.info

${top_builddest}doc/mes.info: doc/mes.texi ${top_builddest}doc/version.texi doc/images/gcc-mesboot-graph.dot GNUmakefile
	$(MAKEINFO) -o $@ -I ${top_builddest}doc -I doc $<

${top_builddest}doc/images/gcc-mesboot-graph.png: doc/images/gcc-mesboot-graph.dot
ifdef DOT
	$(DOT) -T png $< > $@
else
	touch $@
$(warning info: graphvis missing: no images)
endif

man: ${top_builddest}doc/mes.1 ${top_builddest}doc/mescc.1

${top_builddest}src/mes: build

${top_builddest}doc/mes.1: ${top_builddest}src/mes
	MES_ARENA=10000000 ${top_builddir}/pre-inst-env $(HELP2MAN) $(<F) > $@

${top_builddest}doc/mescc.1: ${top_builddest}src/mes ${top_builddest}scripts/mescc
	MES_ARENA=10000000 ${top_builddir}/pre-inst-env $(HELP2MAN) $(<F) > $@

html: ${top_builddest}doc/html/index.html

${top_builddest}doc/html/index.html: doc/mes.texi ${top_builddest}doc/version.texi ${top_builddest}doc/images/gcc-mesboot-graph.png
	$(MAKEINFO) --html -o $(@D) -I ${top_builddest}doc -I doc $<

dvi: ${top_builddest}doc/mes.dvi

${top_builddest}doc/mes.dvi: doc/mes.texi
	$(MAKEINFO) --dvi -I ${top_builddest}/doc -I doc -o doc/mes.dvi $<

pdf: ${top_builddest}doc/mes.pdf

${top_builddest}doc/mes.pdf: doc/mes.texi
	$(MAKEINFO) --pdf -I ${top_builddest}/doc -I doc -o doc/mes.pdf $<

ps: ${top_builddest}doc/mes.ps

${top_builddest}doc/mes.ps: doc/mes.texi
	$(MAKEINFO) --ps -I ${top_builddest}/doc -I doc -o doc/mes.ps $<

###  dist
COMMIT=$(shell test -d .git && (git describe --dirty 2>/dev/null) || cat .tarball-version)
TARBALL_VERSION=$(COMMIT:v%=%)
TARBALL_DIR:=$(PACKAGE)-$(TARBALL_VERSION)
TARBALL:=${top_builddest}$(TARBALL_DIR).tar.gz

${top_builddest}.tarball-version:
	echo $(COMMIT) > $@

GIT_ARCHIVE_HEAD:=git archive HEAD --
GIT_LS_FILES:=git ls-files
ifeq ($(wildcard .git),)
GIT_ARCHIVE_HEAD:=tar -cf-
GIT_LS_FILES:=find
endif

dist: $(TARBALL)

tree-clean-p:
	test ! -d .git || git diff --exit-code > /dev/null
	test ! -d .git || git diff --cached --exit-code > /dev/null
	@echo commit:$(COMMIT)

generate-ChangeLog:
	$(PERL) build-aux/gitlog-to-changelog --srcdir=${srcdir} > ChangeLog

$(TARBALL): ${top_builddest}.tarball-version | generate-ChangeLog
	($(GIT_LS_FILES)\
	    --exclude=$(TARBALL_DIR);\
	    echo $^ | tr ' ' '\n')\
	    | GZIP=-n tar --sort=name --mtime=@0 --owner=0 --group=0 --numeric-owner\
	    --transform=s,^,$(TARBALL_DIR)/,S -T- -czf $@
	git checkout ChangeLog

ifdef GUIX
update-hash: $(TARBALL)
	$(GUIX) download file://$(PWD)/$<
	sed -i -e 's,(base32 #!mes!# "[^"]*"),(base32 #!mes!# "$(shell $(GUIX) hash $<)"),' guix/git/mes.scm

else
$(warning update-hash: no guix)
endif

release: update-hash
	./pre-inst-env $(GUIX) build mes@$(VERSION) --with-source=$(TARBALL)

installdirs: mkinstalldirs
	mkdir -p\
	    $(DESTDIR)$(bindir)\
	    $(DESTDIR)$(datadir)\
	    $(DESTDIR)$(libdir)\
	    $(DESTDIR)$(infodir)\
	    $(DESTDIR)$(mandir)

install-dvi: dvi
	mkdir -p $(DESTDIR)${docdir}
	cp ${top_builddest}doc/mes.dvi $(DESTDIR)${docdir}

install-html: html
	mkdir -p $(DESTDIR)${docdir}
	tar -cf- -C ${top_builddest}doc html | tar -xf- -C $(DESTDIR)${docdir}

install-pdf: pdf
	mkdir -p $(DESTDIR)${docdir}
	cp ${top_builddest}doc/mes.pdf $(DESTDIR)${docdir}

install-ps: ps
	mkdir -p $(DESTDIR)${docdir}
	cp ${top_builddest}doc/mes.ps $(DESTDIR)${docdir}

# We do not strip binaries, binutils' strip corrupts M1+hex2-generated ELFs
install-strip: install


define HELP_TOP
Usage: make [OPTION]... [TARGET]...

Main and non-standard targets:
  all             update everything
  all-go          update .go files
  gcc             update src/mes.gcc-out
  dist            update $(TARBALL)
  doc             update documentation
  mes-gcc         update src/mes.mes-gcc-out
  mes-tcc         update src/mes.mes-tcc-out
  mes             update src/mes
  check           run unit tests
  clean           run git clean -dfx
  clean-go        clean .go files
  info            update info documentation
  install         install in $(prefix)
  install-info    install info docs in $(prefix)/share/info
  release         dist and tag
  seed            update mes-seed in $(MES_SEED)
  uninstall       uninstall from $(prefix)
endef
export HELP_TOP
help:
	@echo "$$HELP_TOP"

include build-aux/export.make
