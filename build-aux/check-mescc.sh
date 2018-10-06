#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of GNU Mes.
#
# GNU Mes is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Mes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

set -e
. ${srcdest}build-aux/config.sh
. ${srcdest}build-aux/trace.sh

MES=${MES-src/mes}
[ -z "$MESCC" ] && MESCC=scripts/mescc
GUILE=${GUILE-guile}
MES_PREFIX=${MES_PREFIX-mes}

HEX2=${HEX2-hex2}
M1=${M1-M1}
BLOOD_ELF=${BLOOD_ELF-blood-elf}
MES_SEED=${MES_SEED-../mes-seed}
MESCC=${MESCC-$(command -v mescc)}
[ -z "$MESCC" ] && MESCC=scripts/mescc
MES=${MES-$(command -v mes)}
[ -z "$MES" ] && MES=src/mes

if ! command -v $GUILE > /dev/null; then
    GUILE=true
fi

test_sh=${test_sh-${srcdest}build-aux/test.sh}
if [ "$arch" = "x86_64-mes" ]; then
    test_sh=${srcdest}build-aux/test64.sh
fi

tests="
t
00-exit-0
01-return-0
02-return-1
03-call
04-call-0
05-call-1
06-call-!1
06-!call-1
06-call-2
06-call-string
06-call-variable
06-return-void
07-include
08-assign
08-assign-negative
08-assign-global
10-if-0
11-if-1
12-if-==
13-if-!=
14-if-goto
15-if-!f
16-if-t
17-compare-ge
17-compare-gt
17-compare-le
17-compare-lt
17-compare-unsigned-ge
17-compare-unsigned-gt
17-compare-unsigned-le
17-compare-unsigned-lt
17-compare-unsigned-char-le
17-compare-unsigned-short-le
17-compare-unsigned-long-le
17-compare-and
17-compare-or
17-compare-and-or
17-compare-assign
17-compare-call
18-assign-shadow
20-while
21-char[]-simple
21-char[]
22-while-char[]
23-global-pointer-init-null
23-global-pointer-init
23-global-pointer-ref
23-global-pointer-pointer-ref
23-pointer-sub
23-pointer
30-strlen
31-oputs
32-call-wrap
32-compare
33-and-or
34-pre-post
35-compare-char
36-compare-arithmetic
37-compare-assign
38-compare-call-2
38-compare-call-3
38-compare-call
40-if-else
41-?
42-goto-label
43-for-do-while
44-switch
44-switch-fallthrough
44-switch-body-fallthrough
45-void-call
46-function-static
47-function-expression
48-global-static
50-assert
51-pointer-sub
51-itoa
51-strcmp
51-strncmp
53-strcpy
54-argc
54-argv
55-char-array
60-math
61-array
62-array
63-struct
63-struct-pointer
63-struct-local
63-struct-function
63-struct-assign
63-struct-array
63-struct-array-assign
63-struct-array-compare
63-struct-cell
64-make-cell
65-read
66-local-char-array
70-strchr
70-stdarg
70-printf-hello
70-printf-simple
70-printf
71-struct-array
72-typedef-struct-def
73-union-hello
73-union
74-multi-line-string
75-struct-union
76-pointer-arithmetic-pp
76-pointer-arithmetic
77-pointer-assign
78-union-struct
79-int-array-simple
79-int-array
7a-struct-char-array
7b-struct-int-array-hello
7b-struct-int-array-pointer
7b-struct-int-array
7c-dynarray
7d-cast-char
7e-struct-array-access
7f-struct-pointer-arithmetic
7g-struct-byte-word-field
7h-struct-assign
7i-struct-struct-simple
7i-struct-struct
7j-strtoull
7k-empty-for
7k-for-each-elem-simple
7k-for-each-elem
7l-struct-any-size-array-simple
7l-struct-any-size-array
7m-struct-char-array-assign
7n-struct-struct-array
7o-struct-pre-post-simple
7o-struct-pre-post
7p-struct-cast
7q-bit-field-simple
7q-bit-field
7r-sign-extend
7s-struct-short
7s-unsigned-compare
7t-function-destruct
7u-double
7u-long-long
7u-?-expression
7u-call-?
7u-inc-byte-word
7u-struct-func
7u-struct-size10
7u-vstack
80-setjmp
81-qsort
81-qsort-dupes
82-define
83-heterogenoous-init
84-struct-field-list
85-sizeof
86-strncpy
87-sscanf
88-strrchr
90-strspn
90-strpbrk
91-fseek
92-stat
93-fread-fwrite
94-unsetenv
95-signal
96-strto
97-fopen
98-fopen
99-readdir
a0-call-trunc-char
a0-call-trunc-short
a0-call-trunc-int
"

broken="$broken
17-compare-unsigned-char-le
17-compare-unsigned-short-le
66-local-char-array
"

# gcc not supported
CC=
set +e
expect=$(echo $broken | wc -w)
pass=0
fail=0
total=0
mkdir -p scaffold/tests
for t in $tests; do
    if [ -z "${t/[012][0-9]-*/}" ]; then
        LIBC=
        MES_LIBS="-l none"
    elif [ -z "${t/[34][0-9]-*/}" ]; then
        LIBC=c-mini
        MES_LIBS="-l c-mini"
    elif [ -z "${t/[78][0-9a-z]-*/}" ]; then
        LIBC=c+tcc
        MES_LIBS="-l c+tcc"
    elif [ -z "${t/9[0-9]-*/}" ]; then
        LIBC=c+gnu
        MES_LIBS="-l c+gnu"
    else
        LIBC=c
        MES_LIBS=
    fi
    sh $test_sh "scaffold/tests/$t" &> scaffold/tests/"$t".log
    r=$?
    total=$((total+1))
    if [ $r = 0 ]; then
        echo $t: [OK]
        pass=$((pass+1))
    else
        echo $t: [FAIL]
        fail=$((fail+1))
    fi
done

[ $expect != 0 ] && echo "expect: $expect"
[ $fail != 0 ] && echo "failed: $fail"
[ $fail -lt $expect ] && echo "solved: $(($expect - $fail))"
echo "passed: $pass"
echo "total:  $total"
if [ $fail != 0 -a $fail -gt $expect ]; then
    echo FAILED: $fail/$total
    exit 1
elif [ $fail != 0 ]; then
    echo PASS: $pass/$total
else
    echo PASS: $total
fi
