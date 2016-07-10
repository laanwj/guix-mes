.PHONY: all check default 
CFLAGS=-std=c99 -O3 -finline-functions
#CFLAGS=-g

default: all

all: mes boot.mes

#mes.o: mes.c mes.h
mes: mes.c mes.h

mes.h: mes.c GNUmakefile
#	$(info FUNCTIONS:$(FUNCTIONS))
	( echo '#if MES'; echo '#if MES' 1>&2;\
	grep -E '^(scm [*])*[a-z_]+ \(.*\)( {|$$)' $< | grep -Ev '\(.*(char |bool |int )' | sed -e 's,^scm [*],,' | sort |\
		while read f; do\
			fun=$$(echo $$f | sed -e 's,^scm [*],,' -e 's,{.*,,');\
			name=$$(echo $$fun | sed -e 's,^scm [\*],,' | grep -o '^[^ ]*');\
			scm_name=$$(echo $$name | sed -e 's,_p$$,?,' -e 's,_x$$,!,' -e 's,^builtin_,,' -re 's,(.*)_$$,c:\1,' | sed -e 's,^less?$$,<,' -e 's,^minus$$,-,' -e 's,_,-,g');\
			args=$$(echo $$fun | grep -o 'scm [\*]' | wc -l);\
			echo "scm *$$fun;";\
			echo "scm scm_$$name = {FUNCTION$$args, .name=\"$$scm_name\", .function$$args=&$$name};";\
			echo "a = add_environment (a, \"$$scm_name\", &scm_$$name);" 1>&2;\
	done; echo '#endif'; echo '#endif' 1>&2) > $@ 2>environment.i

check: all
	./mes.test
	./mes.test ./mes
	cat scm.mes test.mes | ./mes

boot.mes: mes.mes scm.mes test.mes
	cat $^ > $@

boot: all
	./mes < boot.mes

run: all
	cat scm.mes test.mes | ./mes
