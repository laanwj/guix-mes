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

# dash does not export foo=${foo-bar} for some values
export prefix datadir moduledir
export CC CC32 TCC GUILE MESCC MES_SEED
export MES_ARENA MES_DEBUG
export CPPFLAGS CFLAGS C32FLAGS MESCCFLAGS
export BUILD_DEBUG

CC=${CC-$(command -v gcc)}
CC32=${CC32-$(command -v i686-unknown-linux-gnu-gcc)}
MESCC=${MESCC-$(command -v mescc)}
MES_SEED=${MES_SEED-../mes-seed}
GUILE=${GUILE-$(command -v guile)}
MES_ARENA=${MES_ARENA-100000000}
MES_DEBUG=${MES_DEBUG-1}

prefix=${prefix-/usr/local}
datadir=${datadir-$prefix/share/mes}
moduledir=${moduledir-${datadir}${datadir:+/}module}
set -e

CPPFLAGS=${CPPFLAGS-"
-D VERSION=\"$VERSION\"
-D MODULEDIR=\"$moduledir\"
-D PREFIX=\"$prefix\"
-I src
-I lib
-I include
"}
CFLAGS=${CFLAGS-"
--std=gnu99
-O0
-g
"}
C32FLAGS=${C32FLAGS-"
--std=gnu99
-O0
-fno-stack-protector
-g
-m32
-nostdinc
-nostdlib
"}
MESCCLAGS=${MESCCFLAGS-"
"}
M1FLAGS=${M1FLAGS-"
--LittleEndian
--Architecture 1
"}
HEX2FLAGS=${HEX2FLAGS-"
--LittleEndian
--Architecture 1
--BaseAddress 0x1000000
"}

if [ -n "$GUILE" ]; then
    sh build-aux/build-guile.sh
fi

if [ -n "$CC" ]; then
    sh build-aux/build-cc.sh
    cp src/mes.gcc-out src/mes
fi

if [ -n "$CC32" ]; then
    sh build-aux/build-cc32.sh
    cp src/mes.mes-gcc-out src/mes
fi

if [ -n "$TCC" ]; then
    CC32=$TCC sh build-aux/build-cc32.sh
    cp src/mes.mes-tcc-out src/mes
fi

sh build-aux/build-mes.sh
