ifneq ($(GUILE),)
CROSS:=guile-
C_FILES:=mlibc/libc-mes.c $(C_FILES)
#C_FILES:=mlibc/mini-libc-mes.c $(C_FILES)
O_FILES:=$(C_FILES:%.c=$(OUT)/%.$(CROSS)o)

ifneq ($(DEBUG),)
$(info TARGET=$(TARGET))
$(info C_FILES=$(C_FILES))
$(info O_FILES=$(O_FILES))
endif

CLEAN+=$(O_FILES) $(OUT)/$(TARGET)

CLEAN+=$(OUT)/$(TARGET)

INCLUDES+=mlibc/include mlibc $(OUT)/$(DIR)
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
$(OUT)/$(1:.c=.$(CROSS)o): CROSS:=$(CROSS)
$(OUT)/$(1:.c=.$(CROSS)o): cc:=MESCC.scm
$(OUT)/$(1:.c=.$(CROSS)o): CC:=$(MESCC.scm)
$(OUT)/$(1:.c=.$(CROSS)o): CPPFLAGS:=$(2:%=-D %) $(3:%=-I %)
$(OUT)/$(1:.c=.$(CROSS)o): COMPILE.c:=$(MESCC.scm) -c $(CPPFLAGS)
$(OUT)/$(1:.c=.$(CROSS)o): $(MAKEFILE_LIST)
$(OUT)/$(1:.c=.$(CROSS)o): $(INSTALL_GO_FILES)
$(OUT)/$(1:.c=.$(CROSS)o): $(1)
	@echo "  $$(cc)	$$(notdir $(1)) -> $$(notdir $$@)"
	@mkdir -p $$(dir $$@)
	$$(QUIET)$$(COMPILE.c) $$(CPPFLAGS) $$(OUTPUT_OPTION) $(1)
endef

define mescc.scm-c-preprocess
$(OUT)/$(1:.c=.$(CROSS)E): CROSS:=$(CROSS)
$(OUT)/$(1:.c=.$(CROSS)E): cc:=MESCC.scm
$(OUT)/$(1:.c=.$(CROSS)E): CC:=$(MESCC.scm)
$(OUT)/$(1:.c=.$(CROSS)E): CPPFLAGS:=$(2:%=-D %) $(3:%=-I %)
$(OUT)/$(1:.c=.$(CROSS)E): PREPROCESS.c:=$(MESCC.scm) -E $(CPPFLAGS)
$(OUT)/$(1:.c=.$(CROSS)E): $(MAKEFILE_LIST)
$(OUT)/$(1:.c=.$(CROSS)E): $(INSTALL_GO_FILES)
$(OUT)/$(1:.c=.$(CROSS)E): $(1)
	@echo "  $$(cc)	$$(notdir $(1)) -> $$(notdir $$@)"
	@mkdir -p $$(dir $$@)
	$$(QUIET)$$(PREPROCESS.c) $$(CPPFLAGS) $$(OUTPUT_OPTION) $(1)
endef

define mescc.scm-c-compile-E
$(1:.$(CROSS)E=.$(CROSS)o): CROSS:=$(CROSS)
$(1:.$(CROSS)E=.$(CROSS)o): cc:=MESCC.scm
$(1:.$(CROSS)E=.$(CROSS)o): CC:=$(MESCC.scm)
$(1:.$(CROSS)E=.$(CROSS)o): CPPFLAGS:=$(2:%=-D %) $(3:%=-I %)
$(1:.$(CROSS)E=.$(CROSS)o): COMPILE.c:=$(MESCC.scm) -c $(CPPFLAGS)
$(1:.$(CROSS)E=.$(CROSS)o): $(MAKEFILE_LIST)
$(1:.$(CROSS)E=.$(CROSS)o): $(INSTALL_GO_FILES)
$(1:.$(CROSS)E=.$(CROSS)o): $(1)
	@echo "  $$(cc)	$$(notdir $(1)) -> $$(notdir $$@)"
	@mkdir -p $$(dir $$@)
	$$(QUIET)$$(COMPILE.c) $$(CPPFLAGS) $$(OUTPUT_OPTION) $(1)
endef

ifeq ($(MLIBC.scm),)
MLIBC.scm:=DONE
else
C_FILES:=$(filter-out mlibc/libc-mes.c,$(C_FILES))
C_FILES:=$(filter-out mlibc/mini-libc-mes.c,$(C_FILES))
endif

ifneq ($(MESC_DIRECT),)
$(foreach c-file,$(strip $(filter %.c,$(C_FILES))),$(eval $(call mescc.scm-c-compile,$(c-file),$(DEFINES),$(INCLUDES))))
else
$(foreach c-file,$(strip $(filter %.c,$(C_FILES))),$(eval $(call mescc.scm-c-preprocess,$(c-file),$(DEFINES),$(INCLUDES))))
$(foreach c-file,$(strip $(filter %.c,$(C_FILES))),$(eval $(call mescc.scm-c-compile-E,$(c-file:%.c=$(OUT)/%.$(CROSS)E),$(DEFINES),$(INCLUDES))))
endif

endif
include make/reset.make
