GUILE:=guile
GUILE_FLAGS:=--no-auto-compile -L . -L guile -C . -C guile

include .config.make

PHONY_TARGETS:= all all-go check clean clean-go default help install
.PHONY: $(PHONY_TARGETS)

default: all

all:
	./build.sh

cc:
	build-aux/build-cc.sh

mlibc:
	build-aux/build-mlibc.sh

mes:
	build-aux/build-mes.sh

clean:
	git clean -dfx

all-go:
	build-aux/build-guile.sh

clean-go:
	rm -f $(shell find . -name '*.go')

check:
	./check.sh


install:
	./install.sh

.config.make: ./configure

seed:
	cd $(MES_SEED) && git reset --hard HEAD
	MES=$(GUILE) GUILE=$(GUILE) SEED=1 build-aux/build-mes.sh
	cd $(MES_SEED) && MES_PREFIX=$(PWD) ./refresh.sh
	MES=$(GUILE) GUILE=$(GUILE) SEED=1 build-aux/build-mes.sh
	build-aux/build-mlibc.sh
	cd $(TINYCC_SEED) && MES_PREFIX=$(PWD) ./refresh.sh

define HELP_TOP
Usage: make [OPTION]... [TARGET]...

Targets:
  all             update everything
  all-go          update .go files
  cc              update src/mes.gcc-out
  mlibc           update src/mes.mlibc-out
  mes             update src/mes
  check           run unit tests
  clean           run git clean -dfx
  clean-go        clean .go files
  install         install in $(PREFIX)
  seed            update mes-seed in $(MES_SEED)
endef
export HELP_TOP
help:
	@echo "$$HELP_TOP"

ifdef PREFIX
export PREFIX
endif

ifdef VERSION
export VERSION
endif

ifdef CC
export CC
endif

ifdef CC32
export CC32
endif

ifdef BLOOD_ELF
export BLOOD_ELF
endif

ifdef M1
export M1
endif

ifdef HEX2
export HEX2
endif

ifdef GUILE
export GUILE
endif

ifdef GUILE_TOOLS
export GUILE_TOOLS
endif

ifdef GUILE_LOAD_PATH
export GUILE_LOAD_PATH
endif

ifdef GUILE_LOAD_COMPILED_PATH
export GUILE_LOAD_COMPILED_PATH
endif

ifdef CFLAGS
export CFLAGS
endif

ifdef C32FLAGS
export C32FLAGS
endif

ifdef HEX2FLAGS
export HEX2FLAGS
endif

ifdef M1FLAGS
export M1FLAGS
endif

ifdef MESCCFLAGS
export MESCCFLAGS
endif

ifdef MES_SEED
export MES_SEED
endif

ifdef TINYCC_SEED
export TINYCC_SEED
endif
