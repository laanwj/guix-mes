CROSS:=guile-
C_FILES:=libc/libc-mes.c $(C_FILES)
#C_FILES:=libc/mini-libc-mes.c $(C_FILES)
O_FILES:=$(C_FILES:%.c=$(OUT)/%.$(CROSS)o)

ifneq ($(DEBUG),)
$(info TARGET=$(TARGET))
$(info C_FILES=$(C_FILES))
$(info O_FILES=$(O_FILES))
endif

CLEAN+=$(O_FILES) $(OUT)/$(TARGET)

CLEAN+=$(OUT)/$(TARGET)

INCLUDES+=libc/include libc $(OUT)/$(DIR)
MESCC.scm:=guile/mescc.scm
MESLD.scm:=guile/mescc.scm

$(OUT)/$(TARGET): ld:=MESLD.scm
$(OUT)/$(TARGET): LD:=$(MESLD.scm)
$(OUT)/$(TARGET): CC:=$(MESCC.scm)
$(OUT)/$(TARGET): CFLAGS:=
$(OUT)/$(TARGET): LDFLAGS:=
$(OUT)/$(TARGET): O_FILES:=$(O_FILES)
$(OUT)/$(TARGET): $(O_FILES)
	@echo "  $(ld)	$(notdir $(O_FILES)) -> $(notdir $@)"
	$(QUIET)$(LINK.c) $(O_FILES) $(LOADLIBES) $(LDLIBS) -o $@

define mescc.scm-c-compile
$(OUT)/$(1:.c=.$(CROSS)o): $(MAKEFILE_LIST)
$(OUT)/$(1:.c=.$(CROSS)o): cc:=MESCC.scm
$(OUT)/$(1:.c=.$(CROSS)o): CC:=$(MESCC.scm)
$(OUT)/$(1:.c=.$(CROSS)o): CPPFLAGS:=$(2:%=-D %) $(3:%=-I %)
$(OUT)/$(1:.c=.$(CROSS)o): COMPILE.c:=$(MESCC.scm) -c $(CPPFLAGS)
$(OUT)/$(1:.c=.$(CROSS)o): $(INSTALL_GO_FILES)
$(OUT)/$(1:.c=.$(CROSS)o): $(1)
	@echo "  $$(cc)	$$(notdir $(1)) -> $$(notdir $$@)"
	@mkdir -p $$(dir $$@)
	$$(QUIET)$$(COMPILE.c) $$(CPPFLAGS) $$(OUTPUT_OPTION) $(1)
endef

ifeq ($(MLIBC.scm),)
MLIBC.scm:=DONE
else
C_FILES:=$(filter-out libc/libc-mes.c,$(C_FILES))
C_FILES:=$(filter-out libc/mini-libc-mes.c,$(C_FILES))
endif
$(foreach c-file,$(strip $(filter %.c,$(C_FILES))),$(eval $(call mescc.scm-c-compile,$(c-file),$(DEFINES),$(INCLUDES))))

include make/reset.make
