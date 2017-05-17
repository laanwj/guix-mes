CLEAN+=$(OUT)/$(TARGET)
$(OUT)/$(TARGET): C_INCLUDE_PATH:=$(INCLUDES)
$(OUT)/$(TARGET): $(MAKEFILE_LIST)
$(OUT)/$(TARGET): $(INSTALL_GO_FILES)
$(OUT)/$(TARGET): $(C_FILES)
	@echo " mescc.scm	$(notdir $<) -> $(notdir $@)"
	@rm -f $@
	$(QUIET) guile/mescc.scm $(C_INCLUDE_PATH:%=-I %) -o $@ $< || rm -f $@
	@[ -f $@ ] && chmod +x $@ ||:
include make/reset.make
