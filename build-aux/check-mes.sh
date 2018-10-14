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

if [ "$MES" = guile ]; then
    mes=guile-
fi
BASH=${BASH-bash}
GUILE=${GUILE-guile}
MES=${MES-src/mes}
MES_ARENA=${MES_ARENA-100000000}

tests="
tests/boot.test
tests/read.test
tests/srfi-0.test
tests/macro.test
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
tests/srfi-9.test
tests/srfi-13.test
tests/srfi-14.test
tests/srfi-43.test
tests/optargs.test
tests/fluids.test
tests/catch.test
tests/getopt-long.test
tests/guile.test
tests/syntax.test
tests/let-syntax.test
tests/pmatch.test
tests/match.test
tests/psyntax.test
"

mkdir -p tests
set +e
fail=0
total=0
for t in $tests; do
    if [ ! -f $t ]; then
        echo $t: [SKIP];
        continue
    fi
    ${top_builddir}/pre-inst-env sh "$t" &> $t.${mes}log
    r=$?
    total=$((total+1))
    if [ $r = 0 ]; then
        echo $t: [${mes}OK]
    else
        echo $t: [${mes}FAIL]
        fail=$((fail+1))
    fi
done
if [ $fail != 0 ]; then
    echo FAILED: $fail/$total
    exit 1
else
    echo PASS: $total
fi
