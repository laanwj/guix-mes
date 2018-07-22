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

. build-aux/trace.sh
. build-aux/config.sh

c=$1

if [ -z "$ARCHDIR" ]; then
    o="${top_builddest}$c"
    d=${top_builddest}${c%%/*}
    p="gcc-"
else
    b=${c##*/}
    d=${top_builddest}${c%/*}/gcc
    o="$d/$b"
fi
mkdir -p $d

trace "CC $c.c" $CC\
    -c\
    $CC_CPPFLAGS\
    $CPPFLAGS\
    $CC_CFLAGS\
    $CFLAGS\
    -D WITH_GLIBC=1\
    -D POSIX=1\
    -o "$o".${p}o\
    "$c".c

if [ -z "$NOLINK" ]; then
    trace "CCLD "$o".${p}out" $CC\
        $CC_CPPFLAGS\
        $CPPFLAGS\
        $CC_CFLAGS\
        $CFLAGS\
        -o "$o".${p}out\
        "$o".${p}o\
        ${top_builddest}lib/gcc/libmes.o
fi
