ifneq ($(HEX2),)

CLEAN+=$(OUT)/0exit-42
$(OUT)/0exit-42: stage0/elf32-0header.hex2 stage0/elf32-body-exit-42.hex2 stage0/elf-0footer.hex2 | $(OUT)
	@echo "  HEX2	$(notdir $^) -> $(notdir $@)"
	$(QUIET)$(HEX2) $(HEX2_FLAGS) $(^:%=-f %) > $@ || rm -f $@
	chmod +x $@
TARGET:=0exit-42
EXPECT:=42
include make/check.make

CLEAN+=$(OUT)/exit-42
$(OUT)/exit-42: stage0/elf32-header.hex2 stage0/elf32-body-exit-42.hex2 stage0/elf32-footer-single-main.hex2 | $(OUT)
	@echo "  HEX2	$(notdir $^) -> $(notdir $@)"
	$(QUIET)$(HEX2) $(HEX2_FLAGS) $(^:%=-f %) > $@ || rm -f $@
	chmod +x $@
TARGET:=exit-42
EXPECT:=42
include make/check.make

CLEAN+=$(OUT)/exit-42.guile
$(OUT)/exit-42.guile: stage0/elf32-header.hex2 $(OUT)/mlibc/mini-libc-mes.hex2 $(OUT)/stage0/exit-42.hex2 stage0/elf32-footer-single-main.hex2 | $(OUT)
	@echo "  HEX2	$(notdir $^) -> $(notdir $@)"
	$(QUIET)$(HEX2) $(HEX2_FLAGS) $(^:%=-f %) > $@ || rm -f $@
	chmod +x $@

MESCC.scm:=guile/mescc.scm
$(OUT)/%.hex2: %.c | all-go
	@echo "  MESCC.scm	$(notdir $<) -> $(notdir $@)"
	@mkdir -p $(dir $@)
	$(QUIET) $(MESCC.scm) -c -o $@ $^

TARGET:=exit-42.guile
EXPECT:=42
include make/check.make

endif
