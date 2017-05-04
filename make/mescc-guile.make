CLEAN+=$(OUT)/$(TARGET)
$(OUT)/$(TARGET): C_INCLUDE_PATH:=$(INCLUDES)
$(OUT)/$(TARGET): $(MAKEFILE_LIST)
$(OUT)/$(TARGET): $(INSTALL_GO_FILES)
$(OUT)/$(TARGET): $(C_FILES)
	@echo " mescc.scm	$(notdir $<) -> $(notdir $@)"
	@rm -f $@
	$(QUIET) INCLUDES=$(C_INCLUDE_PATH) guile/mescc.scm $< > $@ || rm -f $@
	@[ -f $@ ] && chmod +x $@ ||:
include make/reset.make
