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

. build-aux/config.sh
. build-aux/trace.sh

MES_SEED=${MES_SEED-../mes-seed}
GUILE=${GUILE-$(command -v guile)}
MES_ARENA=${MES_ARENA-100000000}

if [ -n "$GUILE" -a "$GUILE" != true ]; then
    sh build-aux/build-guile.sh
fi

if [ -n "$CC" ]; then
    sh build-aux/build-cc.sh
    cp ${top_builddest}src/mes.gcc-out ${top_builddest}src/mes
fi

if [ -n "$CC32" ]; then
    sh build-aux/build-cc32.sh
    cp ${top_builddest}src/mes.mes-gcc-out ${top_builddest}src/mes
fi

if [ -n "$TCC" ]; then
    CC32=$TCC sh build-aux/build-cc32.sh
    cp ${top_builddest}src/mes.mes-tcc-out ${top_builddest}src/mes
fi

sh build-aux/build-mes.sh
