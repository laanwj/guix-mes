CLEAN+=$(OUT)/$(TARGET)
ifneq ($(MES_MAX_ARENA),)
$(OUT)/$(TARGET): MES_MAX_ARENA-flag:=MES_MAX_ARENA=$(MES_MAX_ARENA)
endif
$(OUT)/$(TARGET): $(MAKEFILE_LIST)
$(OUT)/$(TARGET): module/mes/read-0.mo
$(OUT)/$(TARGET): module/mes/read-0-32.mo
$(OUT)/$(TARGET): $(INSTALL_MES_FILES)
$(OUT)/$(TARGET): scripts/mes
$(OUT)/$(TARGET): $(C_FILES)
	@echo " mescc.mes	$(notdir $<) -> $(notdir $@)"
	@rm -f $@
	$(QUIET)MES_DEBUG=$(MES_DEBUG) $(MES_MAX_ARENA-flag) MES_FLAGS=--load scripts/mescc.mes $< > $@ || rm -f $@
	@[ -f $@ ] && chmod +x $@ ||:
include make/reset.make
