#! /bin/sh

export GUILE=${GUILE-guile}
export MES=${MES-./mes}

tests="
tests/read.test
tests/base.test
tests/closure.test
tests/quasiquote.test
tests/let.test
tests/scm.test
tests/display.test
tests/cwv.test
tests/math.test
tests/vector.test
tests/srfi-1.test
tests/srfi-13.test
tests/srfi-14.test
tests/optargs.test
tests/fluids.test
tests/catch.test
tests/psyntax.test
tests/pmatch.test
tests/let-syntax.test
tests/guile.test
tests/record.test
"

slow="
tests/match.test
tests/peg.test
"

tests=$(for t in $tests; do echo $t-guile; echo $t; done)

set +e
fail=0
total=0
for t in $tests; do
    sh "$t" &> $t.log
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

sh check-mescc.sh
