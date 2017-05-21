CROSS:=mes-
C_FILES:=libc/libc-mes.c $(C_FILES)
O_FILES:=$(C_FILES:%.c=$(OUT)/%.$(CROSS)o)

ifneq ($(DEBUG),)
$(info TARGET=$(TARGET))
$(info C_FILES=$(C_FILES))
$(info O_FILES=$(O_FILES))
endif

CLEAN+=$(O_FILES) $(OUT)/$(TARGET)

CLEAN+=$(OUT)/$(TARGET)

INCLUDES+=libc/include libc $(OUT)/$(DIR)
MESCC.mes:=scripts/mescc.mes
MESLD.mes:=scripts/mescc.mes

$(OUT)/$(TARGET): ld:=MESLD.mes
$(OUT)/$(TARGET): LD:=$(MESLD.mes)
$(OUT)/$(TARGET): CC:=$(MESCC.mes)
$(OUT)/$(TARGET): CFLAGS:=
$(OUT)/$(TARGET): LDFLAGS:=
$(OUT)/$(TARGET): O_FILES:=$(O_FILES)
$(OUT)/$(TARGET): $(O_FILES)
	@echo "  $(ld)	$(notdir $(O_FILES)) -> $(notdir $@)"
	$(QUIET)$(LINK.c) $(O_FILES) $(LOADLIBES) $(LDLIBS) -o $@

define mescc.mes-c-compile
$(OUT)/$(1:.c=.$(CROSS)o): $(MAKEFILE_LIST)
$(OUT)/$(1:.c=.$(CROSS)o): cc:=MESCC.mes
$(OUT)/$(1:.c=.$(CROSS)o): CC:=$(MESCC.mes)
$(OUT)/$(1:.c=.$(CROSS)o): COMPILE.c:=$(MESCC.mes) -c
$(OUT)/$(1:.c=.$(CROSS)o): CPPFLAGS:=$(2:%=-D %) $(3:%=-I %)
$(OUT)/$(1:.c=.$(CROSS)o): COMPILE.c:=$(MESCC.mes) -c $(CPPFLAGS)
$(OUT)/$(1:.c=.$(CROSS)o): all-mo
$(OUT)/$(1:.c=.$(CROSS)o): $(INSTALL_MES_FILES)
$(OUT)/$(1:.c=.$(CROSS)o): $(1)
	@echo "  $$(cc)	$$(notdir $(1)) -> $$(notdir $$@)"
	@mkdir -p $$(dir $$@)
	$$(QUIET)$$(COMPILE.c) $$(OUTPUT_OPTION) $(1)
endef

ifeq ($(MLIBC.mes),)
MLIBC.mes:=DONE
else
C_FILES:=$(filter-out libc/libc-mes.c,$(C_FILES))
endif

$(foreach c-file,$(strip $(filter %.c,$(C_FILES))),$(eval $(call mescc.mes-c-compile,$(c-file),$(DEFINES),$(INCLUDES))))

include make/reset.make
