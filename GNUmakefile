GUILE:=guile
GUILE_FLAGS:=--no-auto-compile -L . -L guile -C . -C guile

include .config.make

PHONY_TARGETS:= all all-go check clean clean-go default help install
.PHONY: $(PHONY_TARGETS)

default: all

all:
	./build.sh

clean:
	true

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
	cd ../mes-seed && git reset --hard HEAD
	MES=guile GUILE=guile SEED=1 build-aux/build-mes.sh
	cd ../mes-seed && ./refresh.sh && cd ../mes
	MES=guile GUILE=guile SEED=1 build-aux/build-mes.sh
	build-aux/build-mlibc.sh
	cd ../tinycc-seed && ./refresh.sh && cd ../mes

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

ifdef M1
export M1
endif

ifdef HEX2
export HEX2
endif

ifdef GUILE
export GUILE
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


