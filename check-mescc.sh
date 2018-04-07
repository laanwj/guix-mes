#! /bin/sh

# Mes --- Maxwell Equations of Software
# Copyright © 2017,2018 Jan Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of Mes.
#
# Mes is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Mes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Mes.  If not, see <http://www.gnu.org/licenses/>.

export MES=${MES-src/mes}
export MESCC=${MESCC-scripts/mescc.mes}
#export MES_ARENA=${MES_ARENA-200000000} > 12GB mem

GUILE=${GUILE-guile}
MES=${MES-src/mes}
M1=${M1-M1}
HEX2=${HEX2-hex2}
MESCC=${MESCC-guile/mescc.scm}
MES_PREFIX=${MES_PREFIX-.}

# $MESCC -E -o lib/crt1.E lib/crt1.c
# $MESCC -c -o lib/crt1.M1 lib/crt1.E
# $M1 --LittleEndian --Architecture=1 \
#     -f stage0/x86.M1\
#     -f lib/crt1.M1\
#     > lib/crt1.hex2
# $MESCC -E -o lib/libc-mes.E lib/libc-mes.c
# $MESCC -c -o lib/libc-mes.M1 lib/libc-mes.E
# $M1 --LittleEndian --Architecture=1\
#     -f stage0/x86.M1\
#     -f lib/libc-mes.M1\
#     > lib/libc-mes.hex2

tests="
t
00-exit-0
01-return-0
02-return-1
03-call
04-call-0
05-call-1
06-call-!1
10-if-0
11-if-1
12-if-==
13-if-!=
14-if-goto
15-if-!f
16-if-t
20-while
21-char[]
22-while-char[]
23-pointer
30-strlen
31-eputs
32-compare
33-and-or
34-pre-post
35-compare-char
36-compare-arithmetic
37-compare-assign
38-compare-call
40-if-else
41-?
42-goto-label
43-for-do-while
44-switch
45-void-call
50-assert
51-strcmp
52-itoa
53-strcpy
54-argv
60-math
61-array
63-struct-cell
64-make-cell
65-read
70-printf
71-struct-array
72-typedef-struct-def
73-union
74-multi-line-string
75-struct-union
76-pointer-arithmetic
77-pointer-assign
78-union-struct
79-int-array
7a-struct-char-array
7b-struct-int-array
7c-dynarray
7d-cast-char
7e-struct-array-access
7f-struct-pointer-arithmetic
7g-struct-byte-word-field
7h-struct-assign
7i-struct-struct
7j-strtoull
7k-for-each-elem
7l-struct-any-size-array
7m-struct-char-array-assign
7n-struct-struct-array
80-setjmp
81-qsort
82-define
"

if [ ! -x ./i686-unknown-linux-gnu-tcc ]; then
    tests=$(echo "$tests" | grep -Ev "02-return-1|05-call-1|80-setjmp|81-qsort")
fi

set +e
fail=0
total=0
for t in $tests; do
    sh test.sh "$t" &> scaffold/tests/$t.log
    r=$?
    total=$((total+1))
    if [ $r = 0 ]; then
        echo $t: [OK]
    else
        echo $t: [FAIL]
        fail=$((fail+1))
    fi
done
if [ $fail != 0 ]; then
    echo FAILED: $fail/$total
    exit 1
else
    echo PASS: $total
fi
