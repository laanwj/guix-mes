#! /bin/sh

GUILE=${GUILE-guile}
MES=${MES-./mes}
M1=${M1-M1}
HEX2=${HEX2-hex2}
MESCC=${MESCC-guile/mescc.scm}
MES_PREFIX=${MES_PREFIX-.}

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
"

if [ ! -x ./i686-unknown-linux-gnu-tcc ]; then
    tests=$(echo "$tests" | grep -Ev "02-return-1|05-call-1")
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
