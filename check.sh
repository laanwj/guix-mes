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

export GUILE=${GUILE-guile}
export MES=${MES-src/mes}
#export MES_ARENA=${MES_ARENA-200000000} #9GiB

set -e
bash check-boot.sh

tests="
tests/boot.test
tests/read.test
tests/base.test
tests/quasiquote.test
tests/let.test
tests/closure.test
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
tests/record.test
tests/getopt-long.test
tests/guile.test
tests/syntax.test
tests/let-syntax.test
tests/pmatch.test
tests/match.test
tests/psyntax.test
"

slow_or_broken="
tests/peg.test
"

tests=$(for t in $tests; do echo $t-guile; echo $t; done)

set +e
fail=0
total=0
for t in $tests; do
    if [ ! -f $t ]; then
        echo $t: [SKIP];
        continue
    fi
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
