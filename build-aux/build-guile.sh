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

if [ ! "$config_status" ]; then
    . ./config.status
fi

. ${srcdest}build-aux/config.sh
. ${srcdest}build-aux/trace.sh
GUILE_AUTO_COMPILE=0

set -e

SCM_FILES="
${srcdest}module/mes/getopt-long.scm
${srcdest}module/mes/guile.scm
${srcdest}module/mes/misc.scm
${srcdest}module/mes/test.scm
${srcdest}module/mescc/M1.scm
${srcdest}module/mescc/as.scm
${srcdest}module/mescc/bytevectors.scm
${srcdest}module/mescc/compile.scm
${srcdest}module/mescc/i386/as.scm
${srcdest}module/mescc/i386/info.scm
${srcdest}module/mescc/x86_64/as.scm
${srcdest}module/mescc/x86_64/info.scm
${srcdest}module/mescc/info.scm
${srcdest}module/mescc.scm
${srcdest}module/mescc/mescc.scm
${srcdest}module/mescc/preprocess.scm
"

SCRIPTS="
${srcdest}build-aux/mes-snarf.scm
"

export host=$($GUILE -c "(display %host-type)")

abs=$srcdest
if [ "$GUILE_EFFECTIVE_VERSION" = "2.0" ]; then
    srcdest=$abs_top_srcdir/
fi

for i in $SCM_FILES $SCRIPTS; do
    b=$(basename $i)
    go=${i%%.scm}.go
    if [ $i -nt $go ]; then
        trace "GUILEC     $i" $GUILD compile -L ${srcdest}module -L ${srcdest}build-aux -L ${srcdest}scripts -o $go $i
    fi
done
