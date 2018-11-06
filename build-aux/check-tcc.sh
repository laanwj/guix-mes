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
. ./config.status
. ${srcdest}build-aux/config.sh
. ${srcdest}build-aux/trace.sh

tests="
00_assignment
01_comment
02_printf
03_struct
04_for
05_array
06_case
07_function
08_while
09_do_while

10_pointer
11_precedence
12_hashdefine
13_integer_literals
14_if
15_recursion
16_nesting
17_enum
18_include
19_pointer_arithmetic

20_pointer_comparison
21_char_array
22_floating_point
23_type_coercion
24_math_library
25_quicksort
26_character_constants
27_sizeof
28_strings
29_array_address

30_hanoi
31_args
32_led
33_ternary_op
34_array_assignment
35_sizeof
36_array_initialisers
37_sprintf
38_multiple_array_index
39_typedef

40_stdio
41_hashif
42_function_pointer
43_void_param
44_scoped_declarations
45_empty_for
47_switch_return
48_nested_break
49_bracket_evaluation

50_logical_second_arg
51_static
52_unnamed_enum
54_goto
55_lshift_type
"

broken="$broken
18_include

22_floating_point
23_type_coercion
24_math_library
26_character_constants
27_sizeof
28_strings

34_array_assignment
39_typedef

40_stdio
42_function_pointer
49_bracket_evaluation
55_lshift_type
"

#22_floating_point       ; float
#23_type_coercion        ; float
#24_math_library         ; float
#27_sizeof               ; float
#28_strings              ; TODO: strncpy strchr strrchr memset memcpy memcmp
#30_hanoi                ; fails with GCC
#34_array_assignment     ; fails with GCC
#39_typedef              ;unsupported: (decl (decl-spec-list (stor-spec (typedef)) (type-spec (typename "MyFunStruct"))) (init-declr-list (init-declr (ptr-declr (pointer) (ident "MoreFunThanEver")))))

#40_stdio                ; f* functions
#42_function_pointer     ; f* functions
#49_bracket_evaluation   ; float


LIBC=c+gnu
MES_LIBS="-l c+gnu"

expect=$(echo $broken | wc -w)
ARGS="arg1 arg2 arg3 arg4 arg5"
export ARGS
pass=0
fail=0
total=0
mkdir -p scaffold/tinycc
set +e
for t in $tests; do
    if [ ! -f $TINYCC_PREFIX/tests/tests2/"$t.c" ]; then
        echo ' [SKIP]'
        continue;
    fi
    cp $TINYCC_PREFIX/tests/tests2/$i* scaffold/tinycc
    sh ${srcdest}build-aux/test.sh "scaffold/tinycc/$t" &> scaffold/tinycc/"$t".log
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
