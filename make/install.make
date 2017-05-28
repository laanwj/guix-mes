.PHONY: tree-clean-p

READMES:=\
 AUTHORS\
 COPYING\
 HACKING\
 INSTALL\
 NEWS\
 README\
#

COMMIT:=$(shell test -d .git && (git show 2>/dev/null | head -1 | cut -d' ' -f 2) || cat .tarball-version)
GUIX-HASH:=out/guix-hash
TARBALL_DIR:=$(PACKAGE)-$(VERSION)
TARBALL:=$(OUT)/$(TARBALL_DIR).tar.gz

OPT_CLEAN:=$(OPT_CLEAN) $(TARBALL) .tarball-version

GIT_ARCHIVE_HEAD:=git archive HEAD --
GIT_LS_FILES:=git ls-files
ifeq ($(wildcard .git/HEAD),)
GIT_ARCHIVE_HEAD:=tar -cf-
GIT_LS_FILES:=find
endif

ifeq ($(GUIX),)
DATADIR:=$(PREFIX)/share/mes
DOCDIR:=$(DATADIR)/doc/mes
else
DATADIR:=$(PREFIX)/share
DOCDIR:=$(DATADIR)/doc
endif
LIBDIR:=$(PREFIX)/lib

GODIR:=$(LIBDIR)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
GUILEDIR:=$(PREFIX)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
MODULEDIR:=$(DATADIR)/module

.tarball-version: tree-clean-p
	echo $(COMMIT) > $@

dist: $(TARBALL)

tree-clean-p:
	test ! -d .git || git diff --exit-code > /dev/null
	test ! -d .git || git diff --cached --exit-code > /dev/null
	@echo commit:$(COMMIT)

$(TARBALL): tree-clean-p .tarball-version ChangeLog
	mkdir -p $(OUT)
	($(GIT_LS_FILES) --exclude=$(OUT);\
		echo $^ | tr ' ' '\n' | grep -Ev 'tree-clean-p')\
		| tar --transform=s,^,$(TARBALL_DIR)/,S -T- -czf $@

ChangeLog:
	build-aux/gitlog-to-changelog > $@


#FIXME: INSTALL like CLEAN
INSTALL_SCM_FILES:=
INSTALL_GO_FILES:=
install: $(CLEAN) ChangeLog
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	install $(OUT)/mes $(DESTDIR)$(PREFIX)/bin/mes
ifeq (0,1) # No bootstrap mes.mes ATM
	install $(OUT)/mes.mes $(DESTDIR)$(PREFIX)/bin/mes.mes
endif
	install scripts/mescc.mes $(DESTDIR)$(PREFIX)/bin/mescc.mes
	install scripts/repl.mes $(DESTDIR)$(PREFIX)/bin/repl.mes
	install guile/mescc.scm $(DESTDIR)$(PREFIX)/bin/mescc.scm
	mkdir -p $(DESTDIR)$(DATADIR)
	$(GIT_ARCHIVE_HEAD) module\
		| tar -C $(DESTDIR)$(DATADIR) -xf-
	$(GIT_ARCHIVE_HEAD) guile\
		| tar -C $(DESTDIR)$(DATADIR) -xf-
	sed -i \
	    -e 's,module/,$(DATADIR)/module/,' \
	    -e 's,@DATADIR@,$(DATADIR)/,g' \
	    -e 's,@DOCDIR@,$(DOCDIR)/,g' \
	    -e 's,@GODIR@,$(GODIR)/,g' \
	    -e 's,@GUILEDIR@,$(GUILEDIR)/,g' \
	    -e 's,@MODULEDIR@,$(MODULEDIR)/,g' \
	    -e 's,@PREFIX@,$(PREFIX)/,g' \
	    -e 's,@VERSION@,$(VERSION),g' \
		$(DESTDIR)$(DATADIR)/module/mes/base-0.mes \
		$(DESTDIR)$(DATADIR)/module/language/c99/compiler.mes \
		$(DESTDIR)$(PREFIX)/bin/mescc.mes \
		$(DESTDIR)$(PREFIX)/bin/mescc.scm \
		$(DESTDIR)$(PREFIX)/bin/repl.mes
	mkdir -p $(DESTDIR)$(DOCDIR)
	$(GIT_ARCHIVE_HEAD) $(READMES) \
		| tar -C $(DESTDIR)$(DOCDIR) -xf-
	$(GIT_ARCHIVE_HEAD) doc \
		| tar -C $(DESTDIR)$(DOCDIR) --strip=1 -xf-
	cp ChangeLog $(DESTDIR)$(DOCDIR)
	mkdir -p $(DESTDIR)$(GUILEDIR)
	tar -cf- -C module $(INSTALL_SCM_FILES:module/%=%)\
		| tar -C $(DESTDIR)$(GUILEDIR) -xf-
	mkdir -p $(DESTDIR)$(GODIR)
	tar -cf- -C module $(INSTALL_GO_FILES:module/%=%)\
		| tar -C $(DESTDIR)$(GODIR) -xf-
	mkdir -p $(DESTDIR)$(PREFIX)/lib
	$(GIT_ARCHIVE_HEAD) libc/include \
		| tar -C $(DESTDIR)$(PREFIX) --strip=1 -xf-
	cp out/libc/libc-mes.guile-o $(DESTDIR)$(PREFIX)/lib/libc-mes.o

release: tree-clean-p check dist
	git tag v$(VERSION)
	git push --tags origin master
	git push origin master

$(GUIX-HASH): tree-clean-p
	rm -rf out/mes && mkdir -p out && git clone . out/mes && guix hash -rx out/mes > $@

update-hash: $(GUIX-HASH) .tarball-version
	@echo -n hash:
	cat $^
	sed -i \
		-e 's,(base32 "[^"]*"),(base32 "$(shell cat $<)"),'\
		-e 's,(commit "[^"]*"),(commit "$(shell cat .tarball-version)"),'\
		-e 's,(version "[^g][^"]*"),(version "$(VERSION)"),'\
		guix.scm
	! git diff --exit-code
	git commit -m 'guix hash: $(shell cat $<)' guix.scm

