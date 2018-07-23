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

#set -e

. build-aux/trace.sh

GUILE_EFFECTIVE_VERSION=${GUILE_EFFECTIVE_VERSION-$(guile -c '(display (effective-version))')}

prefix=${prefix-/usr/local}
datadir=${datadir-$prefix/share}
docdir=${docdir-$datadir/doc/mes-$VERSION}
infodir=${infodir-$datadir/info}
mandir=${mandir-$datadir/man}
moduledir=${moduledir-$datadir/mes/module}
guile_site_dir=${guile_site_dir-$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION}
guile_site_ccache_dir=${guile_site_ccache_dir-$prefix/lib/guile/$GUILE_EFFECTIVE_VERSION/site-ccache}

mkdir -p $DESTDIR$prefix/bin
cp ${top_builddest}src/mes $DESTDIR$prefix/bin/mes

mkdir -p $DESTDIR$prefix/lib
mkdir -p $DESTDIR$MES_PREFIX/lib
cp ${top_builddest}scripts/mescc $DESTDIR$prefix/bin/mescc

rm $DESTDIR$prefix/bin/mes
rm $DESTDIR$prefix/bin/mescc
rmdir $DESTDIR$prefix/bin || :

for i in\
    AUTHORS\
    BOOTSTRAP\
    COPYING\
    ChangeLog\
    HACKING\
    INSTALL\
    NEWS\
    README\
    ;do
    rm $DESTDIR$docdir/$i || :;
done

rmdir $DESTDIR$docdir || :

rm -r $DESTDIR$MES_PREFIX
rm -r $DESTDIR$guile_site_ccache_dir/mes
rm -r $DESTDIR$guile_site_ccache_dir/mescc

rm -r $DESTDIR$guile_site_dir/mes
rm -r $DESTDIR$guile_site_dir/mescc

rm $DESTDIR$prefix/share/info/mes.info*
rm $DESTDIR$mandir/man1/mes.1
rm $DESTDIR$mandir/man1/mescc.1
