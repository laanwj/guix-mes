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

set -ex

t=${1-scaffold/tests/t}
#rm -f "$t".i686-unknown-linux-gnu-out
rm -f "$t".mes-out

sh build-aux/cc-mes.sh "$t"

r=0
set +e
"$t".mes-out | tee "$t".stdout
m=$?

[ $m = $r ]
if [ -f "$t".expect ]; then
    diff -u "$t".expect "$t".stdout;
fi
