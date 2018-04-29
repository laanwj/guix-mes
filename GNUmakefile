GUILE:=guile
GUILE_FLAGS:=--no-auto-compile -L . -L guile -C . -C guile

include .config.make

export PREFIX
export VERSION

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
	cd ../mes-seed && ./bootstrap.sh && cd ../mes
	MES=guile GUILE=guile SEED=1 build-aux/build-mes.sh
