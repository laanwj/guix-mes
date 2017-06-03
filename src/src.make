MODULES:=\
 src/gc.c\
 src/lib.c\
 src/math.c\
 src/mes.c\
 src/posix.c\
 src/reader.c\
 src/vector.c
#

$(OUT)/%.h $(OUT)/%.i $(OUT)/%.environment.i $(OUT)/%.symbols.i: DIR:=$(DIR)
$(OUT)/%.h $(OUT)/%.i $(OUT)/%.environment.i $(OUT)/%.symbols.i: %.c build-aux/mes-snarf.scm
	@echo " SNARF	$(notdir $<) -> $(notdir $@)"
	@mkdir -p $(dir $@)
	$(QUIET)OUT=$(dir $@) build-aux/mes-snarf.scm $<

SNARF.GCC:=$(MODULES:%.c=$(OUT)/%.h) $(MODULES:%.c=$(OUT)/%.i) $(MODULES:%.c=$(OUT)/%.environment.i)
SNARF.GCC+=$(OUT)/$(DIR)/mes.symbols.i
CLEAN+=$(SNARF.GCC)
snarf-gcc: $(SNARF.GCC)

$(OUT)/$(DIR)/mes.o: $(SNARF.GCC)

DEFINES:=FIXED_PRIMITIVES=1 MES_FULL=1 POSIX=1 VERSION='"$(VERSION)"' MODULEDIR='"$(MODULEDIR)"' PREFIX='"$(PREFIX)"'
INCLUDES:=mlibc $(OUT)/$(DIR)
TARGET:=mes
C_FILES:=$(DIR)/mes.c
include make/bin.make

$(OUT)/%.mes.h $(OUT)/%.mes.i $(OUT)/%.mes.environment.i $(OUT)/%.mes.symbols.i: DIR:=$(DIR)
$(OUT)/%.mes.h $(OUT)/%.mes.i $(OUT)/%.mes.environment.i $(OUT)/%.mes.symbols.i: %.c build-aux/mes-snarf.scm
	@echo " SNARF	$(notdir $<) -> $(notdir $@)"
	@mkdir -p $(dir $@)
	$(QUIET)OUT=$(dir $@) build-aux/mes-snarf.scm --mes $<

SNARF.MES:=$(MODULES:%.c=$(OUT)/%.mes.h) $(MODULES:%.c=$(OUT)/%.mes.i) $(MODULES:%.c=$(OUT)/%.mes.environment.i)
SNARF.MES+=$(OUT)/$(DIR)/mes.mes.symbols.i
CLEAN+=$(SNARF.MES)
snarf-mes: $(SNARF.MES)

include make/reset.make

CROSS:=$(CC32:%gcc=%)
$(OUT)/$(CROSS)%: $(OUT)/%.mlibc
	@ln -sf $(<F) $@

TARGET:=mes.mlibc
$(OUT)/$(DIR)/mes.$(CROSS)o: $(SNARF.MES)
C_FILES:=$(DIR)/mes.c
DEFINES:=FIXED_PRIMITIVES=1 MES_FULL=1 VERSION='"$(VERSION)"' MODULEDIR='"$(MODULEDIR)"' PREFIX='"$(PREFIX)"'
include make/bin-mlibc.make

TARGET:=mes.guile
$(OUT)/mes.guile: $(SNARF.MES)
C_FILES:=$(DIR)/mes.c
include make/mescc-guile.make

ifneq ($(MES_BOOTSTRAP),)
safe-MES_MAX_ARENA:=$(MES_MAX_ARENA)
MES_MAX_ARENA:=80000000
TARGET:=mes.mes
$(OUT)/mes.mes: $(SNARF.MES)
mes.mes: $(OUT)/mes.mes
	cp $< $@
C_FILES:=$(DIR)/mes.c
include make/mescc-mes.make
MES_MAX_ARENA=$(safe-MES_MAX_ARENA)
endif
