$(OUT):
	$(QUIET)mkdir -p $@

clean:
	$(QUIET)rm -rf $(CLEAN)
	$(QUIET)mkdir -p $(OUT)

dist-clean: clean
	$(QUIET)rm -rf $(DIST-CLEAN)
distclean: dist-clean

mostly-clean: dist-clean
mostlyclean: mostly-clean

maintainer-clean: dist-clean
	$(QUIET)rm -rf $(MAINTAINER-CLEAN)
maintainerclean: maintainer-clean
