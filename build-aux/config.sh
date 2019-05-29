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

srcdir=${srcdir-.}
top_builddir=${top_builddir-.}

if [ "$V" = 2 ]; then
    echo $0
    echo srcdest=${srcdest}
    echo top_builddir=${top_builddir}
fi

if [ -n "$mes_p" -a -n "$gcc_p" ]; then
    crt1=lib/linux/$mes_arch/crt1.o
fi

MES=${MES-${program_prefix}mes}
libc=${libc-"-l c"}
export libc

if [ ! "$CC" ]; then
    CC="./pre-inst-env mescc"
fi

unset CFLAGS
unset CPPFLAGS
unset HEX2FLAGS
unset LDFLAGS
unset M1FLAGS

export AR
export CC
export CFLAGS
export CPPFLAGS
export GUILD
export GUILE
export GUILE_LOAD_COMPILED_PATH
export GUILE_LOAD_PATH
export HEX2
export HEX2FLAGS
export M1
export M1FLAGS
export MES
export MES_CFLAGS
export MES_FOR_BUILD
export MES_SEED
export MESCC

export MES_DEBUG
export MES_ARENA
export TINYCC_PREFIX
export V

export config_status
export abs_top_builddir
export abs_top_srcdir
export arch
export datadir
export moduledir
export prefix
export program_prefix
export srcdest
export srcdir
export top_builddir

export bits
export build
export host
export compiler
export gcc_p
export mes_p
export mesc_p
export tcc_p
export mes_arch
export with_glibc_p

CPPFLAGS=${CPPFLAGS-"
-D 'VERSION=\"$VERSION\"'
-D 'MODULEDIR=\"$moduledir\"'
-D 'PREFIX=\"$prefix\"'
-I ${srcdest}.
-I ${srcdest}lib
-I ${srcdest}include
"}

[ "$with_glibc_p" ] && CPPFLAGS="$CPPFLAGS -D SYSTEM_LIBC=1"

LDFLAGS=${LDFLAGS-"
-v
-g
-L lib/linux/$mes_arch
-L lib/linux
-L lib/$mes_arch
-L lib
"}

if [ -f "$MES_SEED/x86-mes/mes.S" ]; then
    LDFLAGS="$LDFLAGS
-L $MES_SEED
"
fi

if [ -n "$gcc_p" ]; then
CFLAGS=${CFLAGS-"
-v
--std=gnu99
-O0
-g
"}
fi

if [ "$mes_p" -a "$gcc_p" ]; then
CFLAGS="$CFLAGS
-fno-builtin
-fno-stack-protector
-nostdinc
-nostdlib
-Wno-discarded-qualifiers
-Wno-int-to-pointer-cast
-Wno-pointer-to-int-cast
-Wno-pointer-sign
-Wno-int-conversion
-Wno-incompatible-pointer-types
"
fi

if [ "$arch" = "x86" ]; then
    HEX2FLAGS=${HEX2FLAGS-"
--LittleEndian
--architecture x86
--BaseAddress 0x1000000
"}
    M1FLAGS=${M1FLAGS-"
--LittleEndian
--architecture x86
"}
    bits=32
elif [ "$arch" = "x86_64" ]; then
    HEX2FLAGS=${HEX2FLAGS-"
--LittleEndian
--architecture amd64
--BaseAddress 0x1000000
"}
    M1FLAGS=${M1FLAGS-"
--LittleEndian
--architecture amd64
"}
    bits=64
fi
