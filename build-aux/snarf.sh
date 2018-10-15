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

. ${srcdest}build-aux/config.sh
. ${srcdest}build-aux/trace.sh

snarf="    "
if [ -n "$1" ]; then
    snarf=.mes
fi
trace "SNARF$snarf  gc.c"     ${srcdest}build-aux/mes-snarf.scm $1 src/gc.c
trace "SNARF$snarf  hash.c"   ${srcdest}build-aux/mes-snarf.scm $1 src/hash.c
trace "SNARF$snarf  lib.c"    ${srcdest}build-aux/mes-snarf.scm $1 src/lib.c
trace "SNARF$snarf  math.c"   ${srcdest}build-aux/mes-snarf.scm $1 src/math.c
trace "SNARF$snarf  mes.c"    ${srcdest}build-aux/mes-snarf.scm $1 src/mes.c
trace "SNARF$snarf  module.c" ${srcdest}build-aux/mes-snarf.scm $1 src/module.c
trace "SNARF$snarf  posix.c"  ${srcdest}build-aux/mes-snarf.scm $1 src/posix.c
trace "SNARF$snarf  reader.c" ${srcdest}build-aux/mes-snarf.scm $1 src/reader.c
trace "SNARF$snarf  struct.c" ${srcdest}build-aux/mes-snarf.scm $1 src/struct.c
trace "SNARF$snarf  vector.c" ${srcdest}build-aux/mes-snarf.scm $1 src/vector.c
