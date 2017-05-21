O_FILES := $(C_FILES:%.c=$(OUT)/%.$(CROSS)o)
D_FILES := $(O_FILES:%o=%d)

ifneq ($(DEBUG),)
$(info TARGET=$(TARGET))
$(info C_FILES=$(C_FILES))
$(info O_FILES=$(O_FILES))
$(info D_FILES=$(D_FILES))
endif

CLEAN+=$(O_FILES) $(OUT)/$(TARGET)
DIST-CLEAN+=$(D_FILES)

INCLUDES+=libc/include libc $(OUT)/$(DIR)

$(OUT)/$(TARGET): ld:=$(CROSS)LD
$(OUT)/$(TARGET): LD:=$(CROSS)$(LD)
$(OUT)/$(TARGET): CC:=$(CROSS)$(CC)
$(OUT)/$(TARGET): LDFLAGS:=$(LDFLAGS) $(LD_FLAGS) $(LINK)
$(OUT)/$(TARGET): O_FILES:=$(O_FILES)
$(OUT)/$(TARGET): $(O_FILES)
	@echo "  $(ld)	$(notdir $^) -> $(notdir $@)"
	$(QUIET)$(LINK.c) $^ $(LOADLIBES) $(LDLIBS) -o $@

include make/compile.make
