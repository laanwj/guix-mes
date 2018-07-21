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

export srcdir=.
export host=$($GUILE -c "(display %host-type)")

if [ "$GUILE_EFFECTIVE_VERSION" = "2.0" ]; then
    abs=$PWD/
fi

#$GUILE --no-auto-compile -L guile -C guile -s build-aux/compile-all.scm $SCM_FILES

for i in $SCM_FILES; do
    go=${i%%.scm}.go
    if [ $i -nt $go ]; then
        echo "  GUILEC $i"
        $GUILE_TOOLS compile -L ${abs}module -L ${abs}scripts -o $go $i
    fi
done

SCRIPTS="
scripts/mescc
"

for i in $SCRIPTS; do
    go=${i%%.scm}.go
    if [ $i -nt $go ]; then
        echo "  GUILEC $i"
        $GUILE_TOOLS compile -L ${abs}module -L ${abs}scripts -o $go $i
    fi
done
