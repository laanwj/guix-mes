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

MES_ARENA=100000000

GUILE=${GUILE-$MES}
DIFF=${DIFF-$(command -v diff)} || true
[ -z "$DIFF" ] && DIFF="sh scripts/diff.scm"

t=${1-scaffold/tests/t}
o="$t"
rm -f "$o".mes-out

rm -f "$o".gcc-out
if [ -n "$CC" ]; then
    sh ${srcdest}build-aux/cc.sh "$t"

    r=0
    [ -f "$t".exit ] && r=$(cat "$t".exit)
    set +e
    "$o".gcc-out $ARGS > "$o".gcc-stdout
    m=$?
    cat "$o".gcc-stdout
    set -e

    [ $m = $r ]
    if [ -f "$t".expect ]; then
        $DIFF -ub "$t".expect "$o".gcc-stdout;
    fi
fi

rm -f "$t".mes-gcc-out
if [ -n "$CC32" ]; then
    sh ${srcdest}build-aux/cc32-mes.sh "$t"

    r=0
    [ -f "$t".exit ] && r=$(cat "$t".exit)
    set +e
    "$o".mes-gcc-out $ARGS > "$o".mes-gcc-stdout
    m=$?
    cat "$t".mes-gcc-stdout
    set -e

    [ $m = $r ]
    if [ -f "$t".expect ]; then
        $DIFF -ub "$t".expect "$o".mes-gcc-stdout;
    fi
fi

rm -f "$o".mes-out
sh ${srcdest}build-aux/cc-mes.sh "$t"

r=0
[ -f "$t".exit ] && r=$(cat "$t".exit)
set +e
"$o".mes-out $ARGS > "$o".mes-stdout
m=$?
cat "$o".mes-stdout
set -e

[ $m = $r ]
if [ -f "$t".expect ]; then
    $DIFF -ub "$t".expect "$o".mes-stdout;
fi
