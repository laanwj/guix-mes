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

CPPFLAGS=${CPPFLAGS-"
-D VERSION=\"$VERSION\"
-D MODULEDIR=\"$MODULEDIR\"
-D PREFIX=\"$PREFIX\"
-I src
-I lib
-I include
"}
MESCCFLAGS=${MESCCFLAGS-"
"}

if [ -n "$BUILD_DEBUG" ]; then
    MESCCFLAGS="$MESCCFLAGS -v"
fi

c=$1

set -e

if [ -n "$PREPROCESS" ]; then
    sh $MESCC $MESCCFLAGS $CPPFLAGS -E "$c".c
    sh $MESCC $MESCCFLAGS -S "$c".E
    sh $MESCC $MESCCFLAGS -c -o "$c".mes-o "$c".S
    if [ -z "$NOLINK" ]; then
        sh $MESCC $MESCCFLAGS -o "$c".mes-out "$c".mes-o $MESCCLIBS
    fi
elif [ -n "$COMPILE" ]; then
    sh $MESCC $MESCCFLAGS $CPPFLAGS -S "$c".c
    sh $MESCC $MESCCFLAGS -c -o "$c".mes-o "$c".S
    if [ -z "$NOLINK" ]; then
        sh $MESCC $MESCCFLAGS -o "$c".mes-out "$c".mes-o $MESCCLIBS
    fi
elif [ -z "$NOLINK" ]; then
    sh $MESCC $MESCCFLAGS $CPPFLAGS -o "$c".mes-out "$c".c $MESCCLIBS
else
    sh $MESCC $MESCCFLAGS $CPPFLAGS -c -o "$c".mes-out "$c".c
fi
