#! /bin/sh

# Mes --- Maxwell Equations of Software
# Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

export BLOOD_ELF GUILE HEX2 M1 MES MESCC
export M1FLAGS HEX2FLAGS PREPROCESS

HEX2=${HEX2-hex2}
M1=${M1-M1}
BLOOD_ELF=${BLOOD_ELF-blood-elf}
MESCC=${MESCC-$(command -v mescc)}
[ -z "$MESCC" ] && MESCC=scripts/mescc
MES=${MES-$(command -v mes)}
[ -z "$MES" ] && MES=src/mes

MES_CPPFLAGS=${MES_CPPFLAGS-"
-D VERSION=\"$VERSION\"
-D MODULEDIR=\"$moduledir\"
-D PREFIX=\"$prefix\"
-I src
-I lib
-I include
"}
MES_CCFLAGS=${MES_CCFLAGS-"
"}

if [ -n "$BUILD_DEBUG" ]; then
    MES_CFLAGS="$MES_CFLAGS -v"
fi

c=$1

set -e

if [ -z "$ARCHDIR" ]; then
    o="$c"
    p="mes-"
else
    b=${c##*/}
    d=${c%/*}
    o="$d/x86-mes/$b"
    mkdir -p $d/x86-mes
fi

if [ -n "$PREPROCESS" ]; then
    bash $MESCC $MES_CPPFLAGS $MES_CFLAGS -E -o "$o.E" "$c".c
    bash $MESCC $MES_CFLAGS -S "$o".E
    bash $MESCC $MES_CFLAGS -c -o "$o".${p}o "$o".S
    if [ -z "$NOLINK" ]; then
        bash $MESCC $MES_CFLAGS -o "$o".${p}out "$o".${p}o $MES_LIBS
    fi
elif [ -n "$COMPILE" ]; then
    bash $MESCC $MES_CPPFLAGS $MES_CFLAGS -S -o "$o.S" "$c".c
    bash $MESCC $MES_CFLAGS -c -o "$o".${p}o "$o".S
    if [ -z "$NOLINK" ]; then
        bash $MESCC $MES_CFLAGS -o "$o".${p}out "$o".${p}o $MES_LIBS
    fi
elif [ -z "$NOLINK" ]; then
    bash $MESCC $MES_CPPFLAGS $MES_CFLAGS -o "$o".${p}out "$c".c $MES_LIBS
else
    bash $MESCC $MES_CPPFLAGS $MES_CFLAGS -c -o "$o".${p}o "$c".c
fi
