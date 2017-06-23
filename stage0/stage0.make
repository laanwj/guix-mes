ifneq ($(HEX2),)
CLEAN+=$(OUT)/exit42
$(OUT)/exit42: stage0/elf32-header-exit-42.hex2 stage0/elf32-body-exit-42.hex2 stage0/elf32-footer-exit-42.hex2
	@echo "  HEX2	$(notdir $^) -> $(notdir $@)"
	$(QUIET)$(HEX2) -f stage0/elf32-header-exit-42.hex2 -f stage0/elf32-body-exit-42.hex2 -f stage0/elf32-footer-exit-42.hex2 --LittleEndian --Architecture 1 --BaseAddress 0x1000000 > $@
	chmod +x $@
TARGET:=exit42
EXPECT:=42
include make/check.make
endif
