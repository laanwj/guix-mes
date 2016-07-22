.PHONY: all check default 
#CFLAGS=-std=c99 -O3 -finline-functions
CFLAGS=-std=c99 -g

default: all

all: mes

mes: mes.c mes.h

mes.h: mes.c GNUmakefile
	( echo '#if MES_C'; echo '#if MES_FULL' 1>&2;\
	grep -E '^(scm [*])*[a-z0-9_]+ \(.*\)( {|$$)' $< | grep -Ev '\(.*(char |bool |int )' | sed -e 's,^scm [*],,' | sort |\
		while read f; do\
			fun=$$(echo $$f | sed -e 's,^scm [*],,' -e 's,{.*,,');\
			name=$$(echo $$fun | sed -e 's,^scm [\*],,' | grep -o '^[^ ]*');\
			scm_name=$$(echo $$name | sed -e 's,_to_,->,' -e 's,_p$$,?,' -e 's,_x$$,!,' -e 's,^builtin_,,' -re 's,(.*)_$$,c:\1,' | sed \
				-e 's,^divide$$,/,'\
				-e 's,^is?$$,=,'\
				-e 's,^greater?$$,>,'\
				-e 's,^less?$$,<,'\
				-e 's,^minus$$,-,'\
				-e 's,^multiply$$,*,'\
				-e 's,^plus$$,+,'\
				-e 's,_,-,g');\
			args=$$(echo $$fun | grep -o 'scm [\*]' | wc -l);\
			[ "$$(echo $$fun | fgrep -o ... )" = "..." ] && args=n;\
			echo "scm *$$fun;";\
			echo "scm scm_$$name = {FUNCTION$$args, .name=\"$$scm_name\", .function$$args=&$$name};";\
			echo "a = add_environment (a, \"$$scm_name\", &scm_$$name);" 1>&2;\
	done; echo '#endif'; echo '#endif' 1>&2) > $@ 2>environment.i

check: all
	./mes.test
	./mes.test ./mes
	cat scm.mes test.mes | ./mes

run: all
	cat scm.mes test.mes | ./mes

syntax: all
	cat scm.mes syntax.mes | ./mes

guile-syntax:
	guile -s syntax.mes

macro: all
	cat scm.mes macro.mes | ./mes
