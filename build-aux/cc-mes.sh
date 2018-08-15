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

MESCC=${MESCC-$(command -v mescc)}
[ -z "$MESCC" ] && MESCC=scripts/mescc
MES=${MES-$(command -v mes)}
[ -z "$MES" ] && MES=src/mes

if [ "$V" = 2 ]; then
    MES_CFLAGS="$MES_CFLAGS -v"
fi

c=$1

set -e

if [ -z "$ARCHDIR" ]; then
    o="$c"
    d=${c%%/*}
    p="mes-"
else
    b=${c##*/}
    d=${c%%/*}/x86-mes
    o="$d/$b"
fi
mkdir -p $d

if [ -n "$PREPROCESS" ]; then
    trace "CPP.mes    $c.c" ./pre-inst-env bash $MESCC $MES_CPPFLAGS $MES_CFLAGS -E -o "$o.E" "${srcdest}$c".c
    trace "CC.mes     $c.E" ./pre-inst-env bash $MESCC $MES_CFLAGS -S "$o".E
    trace "AS.mes     $c.S" ./pre-inst-env bash $MESCC $MES_CFLAGS -c -o "$o".${p}o "$o".S
    if [ -z "$NOLINK" ]; then
        trace "LD.mes     $c.o" ./pre-inst-env bash $MESCC $MES_CFLAGS -o "$o".${p}out "$o".${p}o $MES_LIBS
    fi
elif [ -n "$COMPILE" ]; then
    trace "CC.mes     $c.c" trace "MESCC $c.c" ./pre-inst-env bash $MESCC $MES_CPPFLAGS $MES_CFLAGS -S -o "$o.S" "${srcdest}$c".c
    trace "AS.mes     $c.S" ./pre-inst-env bash $MESCC $MES_CFLAGS -c -o "$o".${p}o "$o".S
    if [ -z "$NOLINK" ]; then
        trace "LD.mes     $c.o" ./pre-inst-env bash $MESCC $MES_CFLAGS -o "$o".${p}out "$o".${p}o $MES_LIBS
    fi
elif [ -z "$NOLINK" ]; then
    trace "CC.mes     $c.c" ./pre-inst-env bash $MESCC $MES_CPPFLAGS $MES_CFLAGS -o "$o".${p}out "${srcdest}$c".c $MES_LIBS
else
   trace "CC.mes     $c.c" ./pre-inst-env bash $MESCC $MES_CPPFLAGS $MES_CFLAGS -c -o "$o".${p}o "${srcdest}$c".c
fi
