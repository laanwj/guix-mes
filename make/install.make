.PHONY: tree-clean-p

READMES:=\
 ANNOUNCE\
 ANNOUNCE-2\
 AUTHORS\
 COPYING\
 HACKING\
 NEWS\
#

COMMIT:=$(shell test -d .git && (git show 2>/dev/null | head -1 | cut -d' ' -f 2) || cat .tarball-version)
GUIX-HASH:=out/guix-hash
TARBALL_DIR:=$(PACKAGE)-$(VERSION)
TARBALL:=$(OUT)/$(TARBALL_DIR).tar.gz

OPT_CLEAN:=$(OPT_CLEAN) $(TARBALL) .tarball-version

GIT_ARCHIVE_HEAD:=git archive HEAD --
GIT_LS_FILES:=git ls-files
ifeq ($(wildcard .git),)
GIT_ARCHIVE_HEAD:=tar -cf-
GIT_LS_FILES:=find
endif

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

install: all ChangeLog
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	install mes $(DESTDIR)$(PREFIX)/bin/mes
	install scripts/elf.mes $(DESTDIR)$(PREFIX)/bin/elf.mes
	install scripts/include.mes $(DESTDIR)$(PREFIX)/bin/include.mes
	install scripts/mescc.mes $(DESTDIR)$(PREFIX)/bin/mescc.mes
	install scripts/repl.mes $(DESTDIR)$(PREFIX)/bin/repl.mes
	install scripts/paren.mes $(DESTDIR)$(PREFIX)/bin/paren.mes
	mkdir -p $(DESTDIR)$(PREFIX)/share/mes
	$(GIT_ARCHIVE_HEAD) module\
		| tar -C $(DESTDIR)$(PREFIX)/share/mes -xf-
	cp module/mes/read-0.mo $(DESTDIR)$(PREFIX)/share/mes/module/mes
	sed -i -e 's@module/@$(PREFIX)/share/mes/module/@' \
		$(DESTDIR)$(PREFIX)/share/mes/module/mes/base-0.mes \
		$(DESTDIR)$(PREFIX)/bin/elf.mes \
		$(DESTDIR)$(PREFIX)/bin/mescc.mes \
		$(DESTDIR)$(PREFIX)/bin/repl.mes \
		$(DESTDIR)$(PREFIX)/bin/paren.mes
	mkdir -p $(DESTDIR)$(PREFIX)/share/doc/mes
	$(GIT_ARCHIVE_HEAD) $(READMES) \
		| tar -C $(DESTDIR)$(PREFIX)/share/doc/mes -xf-
	$(GIT_ARCHIVE_HEAD) doc \
		| tar -C $(DESTDIR)$(PREFIX)/share/doc/mes --strip=1 -xf-
	cp ChangeLog $(DESTDIR)$(PREFIX)/share/doc/mes

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
		-e 's,(version "[^"]*"),(version "$(VERSION).$(shell cut -b1-8 .tarball-version)"),'\
		guix.scm
	! git diff --exit-code
	git commit -m 'guix hash: $(shell cat $<)' guix.scm

