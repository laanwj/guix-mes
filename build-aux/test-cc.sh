#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

if [ ! "$config_status" ]; then
    . ./config.status
fi

. ${srcdest}build-aux/config.sh
. ${srcdest}build-aux/trace.sh
. ${srcdest}build-aux/cc.sh

t=${1-scaffold/tests/t}
o="$t"

rm -f "${program_prefix}$o"
compile "$t"
link "$t"

r=0
[ -f "$t".exit ] && r=$(cat "$t".exit)
set +e
$(dirname "$o")/${program_prefix}$(basename "$o") $ARGS > "$o".${program_prefix}stdout
m=$?
cat "$o".${program_prefix}stdout
set -e

[ $m = $r ]
if [ -f "$t".expect ]; then
    $DIFF -ub "$t".expect "$o".${program_prefix}stdout
fi
