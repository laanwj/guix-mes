.PHONY: all check clean distclean mostlyclean maintainer-clean install

cleaning?:=$(filter clean,$(MAKECMDGOALS))
ifeq ($(cleaning?),)
.config.make: configure $(filter-out .config.make,$(MAKEFILE_LIST))
	./configure
endif

CC32:=arch-gcc
-include .config.make

CLEAN:=$(OUT)
DIST-CLEAN:=.config.make
MAINTAINER-CLEAN:=
CHECK:=
all: $(OUT)

include make/install.make

define subdir
ifneq ($(DEBUG),)
$$(info SUBDIR $(1))
endif
DIR:=$(patsubst %/,%,$(dir $(1)))
DOUT:=$(OUT)/$$(DIR)
include $(1)
endef

$(foreach dir,$(SUBDIRS),$(eval $(call subdir,$(dir)/$(dir).make)))

all: $(CLEAN)

ifneq ($(DEBUG),)
$(info CLEAN=$(CLEAN))
endif

subdirs: $(CLEAN)

check: $(CLEAN) $(CHECK)

include make/clean.make

CROSS_PREFIX:=$(CC32:%gcc=%)
ifeq ($(findstring clean,$(MAKECMDGOALS)),)
ifneq ($(DEBUG),)
$(info DEPS:=$(filter %.d %.$(CROSS_PREFIX)d,$(DIST-CLEAN)))
endif
-include $(filter %.d %.$(CROSS_PREFIX)d,$(DIST-CLEAN))
endif
