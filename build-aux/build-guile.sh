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

. build-aux/trace.sh

export GUILE
export GUILE_AUTO_COMPILE
GUILE=${GUILE-$(command -v guile)}
GUILE_TOOLS=${GUILE_TOOLS-$(command -v guile-tools)}
GUILE_AUTO_COMPILE=0

set -e

SCM_FILES="
module/mes/getopt-long.scm
module/mes/guile.scm
module/mes/misc.scm
module/mes/test.scm
module/mescc/M1.scm
module/mescc/as.scm
module/mescc/bytevectors.scm
module/mescc/compile.scm
module/mescc/i386/as.scm
module/mescc/info.scm
module/mescc/mescc.scm
module/mescc/preprocess.scm
"

SCRIPTS="
build-aux/mes-snarf.scm
scripts/mescc
"

export srcdir=.
export host=$($GUILE -c "(display %host-type)")

if [ "$GUILE_EFFECTIVE_VERSION" = "2.0" ]; then
    abs=$PWD/
fi

GUILE_AUTO_COMPILE=0

for i in $SCM_FILES $SCRIPTS; do
    go=${top_builddest}${i%%.scm}.go
    if [ $i -nt $go ]; then
        trace "GUILEC $i" $GUILE_TOOLS compile -L ${abs}module -L ${abs}/build-aux -L ${abs}scripts -o $go $i
    fi
done
