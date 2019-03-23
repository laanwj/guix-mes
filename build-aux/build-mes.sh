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

if [ ! "$config_status" ]; then
    . ./config.status
fi

. ${srcdest}build-aux/config.sh
. ${srcdest}build-aux/trace.sh
. ${srcdest}build-aux/cc.sh

[ "$mes_p" ] && (program_prefix= compile lib/linux/$mes_arch/crt1)
[ "$mes_p" -a ! "$gcc_p" ] && cp -f lib/linux/$mes_arch/crt1.S lib/$mes_arch/crt1.S
[ "$mes_p" -a ! "$gcc_p" ] && cp -f lib/linux/$mes_arch/crt1.o lib/$mes_arch/crt1.o

 [ ! "$mesc_p" -a ! "$with_glibc_p" ] && (program_prefix= compile lib/linux/$mes_arch/crt0)
[ "$mes_p" -a "$gcc_p" ] && (program_prefix= compile lib/linux/$mes_arch/crti)
[ "$mes_p" -a "$gcc_p" ] && (program_prefix= compile lib/linux/$mes_arch/crtn)

[ ! "$mes_p" -a ! "$mesc_p" ] && compile lib/libmes
[ ! "$mes_p" -a ! "$mesc_p" ] && archive lib/libmes

[ "$mes_p" ] && compile lib/libc-mini
[ "$mes_p" ] && archive lib/libc-mini

[ "$mes_p" ] && compile lib/libc
[ "$mes_p" ] && archive lib/libc

[ "$mes_p"  ] && compile lib/libc+tcc
[ "$mes_p"  ] && archive lib/libc+tcc

[ "$mes_p" ] && compile lib/libc+gnu
[ "$mes_p" ] && archive lib/libc+gnu

[ "$mes_p" -a ! "$mesc_p" ] && compile lib/libtcc1
[ "$mes_p" -a ! "$mesc_p" ] && archive lib/libtcc1

[ "$mes_p" -a ! "$mesc_p" ] && compile lib/libg
[ "$mes_p" -a ! "$mesc_p" ] && archive lib/libg

[ "$mes_p" -a ! "$mesc_p" ] && compile lib/libgetopt
[ "$mes_p" -a ! "$mesc_p" ] && archive lib/libgetopt

compile scaffold/main
(libc= link scaffold/main)

compile scaffold/hello
(libc="-l c-mini" link scaffold/hello)

compile scaffold/argv
(libc="-l c-mini" link scaffold/argv)

[ "$mes_p" ] && compile lib/tests/stdlib/50-malloc
[ "$mes_p" ] && link lib/tests/stdlib/50-malloc

[ "$mes_p" ] && compile lib/tests/posix/50-getenv
[ "$mes_p" ] && link lib/tests/posix/50-getenv


[ "$mes_p" ] && compile scaffold/micro-mes
[ "$mes_p" ] && link scaffold/micro-mes

[ "$mes_p" ] && compile scaffold/tiny-mes
[ "$mes_p" ] && link scaffold/tiny-mes

compile src/mes
link src/mes
