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

. build-aux/config.sh
. build-aux/trace.sh

MESCC=${MESCC-$(command -v mescc)}
[ -z "$MESCC" ] && MESCC=scripts/mescc
MES=${MES-$(command -v mes)}
[ -z "$MES" ] && MES=${top_builddest}src/mes

if [ "$V" = 2 ]; then
    MES_CFLAGS="$MES_CFLAGS -v"
fi

c=$1

set -e

if [ -z "$ARCHDIR" ]; then
    o="${top_builddest}$c"
    d=${top_builddest}${c%%/*}
    p="mes-"
else
    b=${c##*/}
    d=${top_builddest}${c%%/*}/x86-mes
    o="$d/$b"
fi
mkdir -p $d

if [ -n "$PREPROCESS" ]; then
    trace "MESCPP $c.c" ${top_builddir}/pre-inst-env bash $MESCC $MES_CPPFLAGS $MES_CFLAGS -E -o "$o.E" "$c".c
    trace "MESCC $c.E" ${top_builddir}/pre-inst-env bash $MESCC $MES_CFLAGS -S "$o".E
    trace "MESAS $c.S" ${top_builddir}/pre-inst-env bash $MESCC $MES_CFLAGS -c -o "$o".${p}o "$o".S
    if [ -z "$NOLINK" ]; then
        trace "MESLD $c.o" ${top_builddir}/pre-inst-env bash $MESCC $MES_CFLAGS -o "$o".${p}out "$o".${p}o $MES_LIBS
    fi
elif [ -n "$COMPILE" ]; then
    trace "MESCC $c.c" trace "MESCC $c.c" ${top_builddir}/pre-inst-env bash $MESCC $MES_CPPFLAGS $MES_CFLAGS -S -o "$o.S" "$c".c
    trace "MESAS $c.S" ${top_builddir}/pre-inst-env bash $MESCC $MES_CFLAGS -c -o "$o".${p}o "$o".S
    if [ -z "$NOLINK" ]; then
        trace "MESLD $c.o" ${top_builddir}/pre-inst-env bash $MESCC $MES_CFLAGS -o "$o".${p}out "$o".${p}o $MES_LIBS
    fi
elif [ -z "$NOLINK" ]; then
    trace "MESLD $c.c" ${top_builddir}/pre-inst-env bash $MESCC $MES_CPPFLAGS $MES_CFLAGS -o "$o".${p}out "$c".c $MES_LIBS
else
   trace "MESCC $c.c" ${top_builddir}/pre-inst-env bash $MESCC $MES_CPPFLAGS $MES_CFLAGS -c -o "$o".${p}o "$c".c
fi
