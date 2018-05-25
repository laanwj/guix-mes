#! /bin/sh

# Mes --- Maxwell Equations of Software
# Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

if [ -n "$BUILD_DEBUG" ]; then
    set -x
fi

export LIBC MESCCLIBS

GUILE=${GUILE-$MES}
DIFF=${DIFF-$(command -v diff)}
[ -z "$DIFF" ] && DIFF="sh scripts/diff.scm"

t=${1-scaffold/tests/t}
rm -f "$t".mes-out
shift

set -e

rm -f "$t".mlibc-out
if [ -n "$CC32" ]; then
    sh build-aux/cc-mlibc.sh "$t"

    r=0
    [ -f "$t".exit ] && r=$(cat "$t".exit)
    set +e
    "$t".mlibc-out > "$t".mlibc-stdout
    m=$?
    cat "$t".mlibc-stdout
    set -e

    [ $m = $r ]
    if [ -f "$t".expect ]; then
        $DIFF -ub "$t".expect "$t".mlibc-stdout;
    fi
fi

rm -f "$t".mes-out
sh build-aux/cc-mes.sh "$t"

r=0
[ -f "$t".exit ] && r=$(cat "$t".exit)
set +e
"$t".mes-out > "$t".mes-stdout
m=$?
cat "$t".mes-stdout
set -e

[ $m = $r ]
if [ -f "$t".expect ]; then
    $DIFF -ub "$t".expect "$t".mes-stdout;
fi
