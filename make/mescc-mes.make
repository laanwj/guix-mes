ifneq ($(MES),)
CROSS:=mes-
C_FILES:=mlibc/libc-mes.c $(C_FILES)
O_FILES:=$(C_FILES:%.c=$(OUT)/%.$(CROSS)o)

ifneq ($(DEBUG),)
$(info TARGET=$(TARGET))
$(info C_FILES=$(C_FILES))
$(info O_FILES=$(O_FILES))
endif

CLEAN+=$(O_FILES) $(OUT)/$(TARGET)

CLEAN+=$(OUT)/$(TARGET)

INCLUDES+=mlibc/include mlibc $(OUT)/$(DIR)
MESCC.mes:=scripts/mescc.mes -g
MESLD.mes:=scripts/mescc.mes -g
LINK.hex2:=$(HEX2)
#ELF_HEADER:=stage0/elf32-0header.hex2
#ELF_FOOTER:=stage0/elf-0footer.hex2
ELF_HEADER:=stage0/elf32-header.hex2
ELF_FOOTER:=stage0/elf32-footer-single-main.hex2

$(OUT)/$(TARGET): ld:=MESLD.mes
$(OUT)/$(TARGET): LD:=$(MESLD.mes)
$(OUT)/$(TARGET): CC:=$(MESCC.mes)
$(OUT)/$(TARGET): CFLAGS:=
$(OUT)/$(TARGET): LDFLAGS:=
$(OUT)/$(TARGET): $(MAKEFILE_LIST)
$(OUT)/$(TARGET): scripts/mes
$(OUT)/$(TARGET): O_FILES:=$(O_FILES)
$(OUT)/$(TARGET): $(O_FILES)
	@echo "  $(ld)	$(notdir $(O_FILES)) -> $(notdir $@)"
	$(QUIET)$(LINK.hex2) $(HEX2_FLAGS) -f $(ELF_HEADER) $(O_FILES:%=-f %) $(LOADLIBES:%=-f %) $(LDLIBS:%=-f %) -f $(ELF_FOOTER) > $@ || { rm -f $@; exit 1;}
	@chmod +x $@

define mescc.mes-c-compile
$(OUT)/$(1:.c=.$(CROSS)o): CROSS:=$(CROSS)
$(OUT)/$(1:.c=.$(CROSS)o): cc:=MESCC.mes
$(OUT)/$(1:.c=.$(CROSS)o): CC:=$(MESCC.mes)
$(OUT)/$(1:.c=.$(CROSS)o): COMPILE.c:=$(MESCC.mes) -c
$(OUT)/$(1:.c=.$(CROSS)o): CPPFLAGS:=$(2:%=-D %) $(3:%=-I %)
$(OUT)/$(1:.c=.$(CROSS)o): COMPILE.c:=$(MESCC.mes) -c $(CPPFLAGS)
$(OUT)/$(1:.c=.$(CROSS)o): OUT:=$(OUT)
$(OUT)/$(1:.c=.$(CROSS)o): $(MAKEFILE_LIST)
$(OUT)/$(1:.c=.$(CROSS)o): $(OUT)/mes
$(OUT)/$(1:.c=.$(CROSS)o): scripts/mes
$(OUT)/$(1:.c=.$(CROSS)o): $(INSTALL_MES_FILES)
$(OUT)/$(1:.c=.$(CROSS)o): $(1)
	@echo "  $$(cc)	$$(notdir $(1)) -> $$(notdir $$@)"
	@mkdir -p $$(dir $$@)
	$$(QUIET)$$(COMPILE.c) $$(OUTPUT_OPTION) $(1)
endef

define mescc.mes-c-preprocess
$(OUT)/$(1:.c=.$(CROSS)E): CROSS:=$(CROSS)
$(OUT)/$(1:.c=.$(CROSS)E): cc:=MESCC.mes
$(OUT)/$(1:.c=.$(CROSS)E): CC:=$(MESCC.mes)
$(OUT)/$(1:.c=.$(CROSS)E): CPPFLAGS:=$(2:%=-D %) $(3:%=-I %)
$(OUT)/$(1:.c=.$(CROSS)E): OUT:=$(OUT)
$(OUT)/$(1:.c=.$(CROSS)E): PREPROCESS.c:=$(MESCC.mes) -E $(CPPFLAGS)
$(OUT)/$(1:.c=.$(CROSS)E): $(MAKEFILE_LIST)
$(OUT)/$(1:.c=.$(CROSS)E): $(OUT)/mes
$(OUT)/$(1:.c=.$(CROSS)E): scripts/mes
$(OUT)/$(1:.c=.$(CROSS)E): $(INSTALL_MES_FILES)
$(OUT)/$(1:.c=.$(CROSS)E): $(1)
	@echo "  $$(cc)	$$(notdir $(1)) -> $$(notdir $$@)"
	@mkdir -p $$(dir $$@)
	$$(QUIET)$$(PREPROCESS.c) $$(CPPFLAGS) $$(OUTPUT_OPTION) $(1)
endef

define mescc.mes-c-compile-E
$(1:.$(CROSS)E=.$(CROSS)o): CROSS:=$(CROSS)
$(1:.$(CROSS)E=.$(CROSS)o): cc:=MESCC.mes
$(1:.$(CROSS)E=.$(CROSS)o): CC:=$(MESCC.mes)
$(1:.$(CROSS)E=.$(CROSS)o): CPPFLAGS:=$(2:%=-D %) $(3:%=-I %)
$(1:.$(CROSS)E=.$(CROSS)o): COMPILE.c:=$(MESCC.mes) -c $(CPPFLAGS)
$(1:.$(CROSS)E=.$(CROSS)o): OUT:=$(OUT)
$(1:.$(CROSS)E=.$(CROSS)o): $(MAKEFILE_LIST)
$(1:.$(CROSS)E=.$(CROSS)o): $(OUT)/mes
$(1:.$(CROSS)E=.$(CROSS)o): scripts/mes
$(1:.$(CROSS)E=.$(CROSS)o): $(INSTALL_MES_FILES)
$(1:.$(CROSS)E=.$(CROSS)o): $(1)
	@echo "  $$(cc)	$$(notdir $(1)) -> $$(notdir $$@)"
	@mkdir -p $$(dir $$@)
	$$(QUIET)$$(COMPILE.c) $$(CPPFLAGS) $$(OUTPUT_OPTION) $(1)
endef

ifeq ($(MLIBC.mes),)
MLIBC.mes:=DONE
else
C_FILES:=$(filter-out mlibc/libc-mes.c,$(C_FILES))
endif

ifneq ($(MESC_DIRECT),)
$(foreach c-file,$(strip $(filter %.c,$(C_FILES))),$(eval $(call mescc.mes-c-compile,$(c-file),$(DEFINES),$(INCLUDES))))
else
$(foreach c-file,$(strip $(filter %.c,$(C_FILES))),$(eval $(call mescc.mes-c-preprocess,$(c-file),$(DEFINES),$(INCLUDES))))
$(foreach c-file,$(strip $(filter %.c,$(C_FILES))),$(eval $(call mescc.mes-c-compile-E,$(c-file:%.c=$(OUT)/%.$(CROSS)E),$(DEFINES),$(INCLUDES))))
endif

endif
include make/reset.make
