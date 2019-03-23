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

compile () {
    flags=
    [ "$mesc_p" ] && flags="$LDFLAGS $MES_CFLAGS"
    trace "CC         $1.c" $CC -c $CPPFLAGS $CFLAGS $flags -o "$1".${program_prefix}o "${srcdest}$1".c
}

archive () {
    l=$1
    shift
    objects=$(for i in $@; do echo $i.${program_prefix}o; done)
    [ -z "$objects" ] && objects=$l.${program_prefix}o
    out=$(dirname "$l")/$mes_arch/$(basename "$l").a
    d=$(dirname $out)
    mkdir -p $d
    if [ "$mesc_p" ]; then
        trace "AR         $l.a" mv $l.${program_prefix}o $(dirname $l)/$mes_arch/$(basename $l).o\
            && mv $l.${program_prefix}S $(dirname $l)/$mes_arch/$(basename $l).S
    else
        trace "AR         $l.a" $AR cr $out $objects\
            && mv $objects $d
    fi
}

link () {
    lib=$libc
    [ "$with_glibc_p" ] && lib='-l mes'
    out=$(dirname "$1")/${program_prefix}$(basename "$1")
    trace "CCLD       $1" $CC $CFLAGS $LDFLAGS -o" $out" $crt1 "$1".${program_prefix}o $2 $lib
}
