define c-compile
$(OUT)/$(1:.c=.$(CROSS)o): $(MAKEFILE_LIST)
$(OUT)/$(1:.c=.$(CROSS)o): cc:=$(CROSS)CC
$(OUT)/$(1:.c=.$(CROSS)o): CC:=$(CROSS)$(CC)
$(OUT)/$(1:.c=.$(CROSS)o): CPPFLAGS:=$$(CPPFLAGS) $$(CPP_FLAGS) $(2:%=-D%) $(3:%=-I%)
$(OUT)/$(1:.c=.$(CROSS)o): CFLAGS:=$$(CFLAGS) $$(C_FLAGS)
$(OUT)/$(1:.c=.$(CROSS)o): $(1)
	@echo " $$(cc)	$$(notdir $$<) -> $$(notdir $$@)"
	@mkdir -p $$(dir $$@)
	$$(QUIET)$$(COMPILE.c) $$(OUTPUT_OPTION) -MMD -MF $$(@:%.$(CROSS)o=%.$(CROSS)d) -MT '$$(@:.%$(CROSS)o=%.$(CROSS)d)' $$<
endef

$(foreach c-file,$(strip $(filter %.c,$(C_FILES))),$(eval $(call c-compile,$(c-file),$(DEFINES),$(INCLUDES))))
include make/reset.make
