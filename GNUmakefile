.PHONY: all check default 
CFLAGS=-std=c99 -O3 -finline-functions
#CFLAGS=-g

default: all

all: mes

check: all
	./mes.test
	./mes.test ./mes
	./mes < scm.mes
	./mes.scm < scm.mes
