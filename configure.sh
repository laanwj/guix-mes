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

VERSION=0.17

# parse --prefix=prefix
cmdline=$(echo "$@")
p=${cmdline##*--prefix=}
p=${p% *}
p=${p% -*}
prefix=${p-${prefix}}
if [ -z "$prefix" ]; then
   prefix=/usr/local
fi

. build-aux/trace.sh

BASH=${BASH-$(command -v bash)}
GUILE=${GUILE-$(command -v guile)} || true

srcdir=${srcdir-.}
top_builddest=${top_builddest-}
top_builddir=${top_builddest-.}
abs_top_srcdir=${abs_top_srcdir-$PWD}
abs_top_builddir=${abs_top_srcdir-$abs_top_builddir}

if [ -z "$GUILE" -o "$GUILE" = true ]; then
    GUILE_EFFECTIVE_VERSION=${GUILE_EFFECTIVE_VERSION-2.2}
else
    GUILE_EFFECTIVE_VERSION=${GUILE_EFFECTIVE_VERSION-$(guile -c '(display (effective-version))')}
fi
datadir=${datadir-$prefix/share}
docdir=${docdir-$datadir/doc/mes-$VERSION}
infodir=${infodir-$datadir/info}
mandir=${mandir-$datadir/man}
moduledir=${moduledir-$datadir/mes/module}
guile_site_dir=${guile_site_dir-$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION}
guile_site_ccache_dir=${guile_site_ccache_dir-$prefix/lib/guile/$GUILE_EFFECTIVE_VERSION/site-ccache}

sed \
    -e s,"@srcdir@,$srcdir,"\
    -e s,"@abs_top_srcdir@,$abs_top_srcdir,"\
    -e s,"@abs_top_builddir@,$abs_top_builddir,"\
    -e s,"@top_builddir@,$top_builddir,"\
    -e s,"@top_builddest@,$top_builddest,"\
    -e s,"@BASH@,$BASH,"\
    -e s,"@GUILE@,$GUILE,"\
    -e s,"@guile_site_dir@,$guile_site_dir,"\
    -e s,"@guile_site_ccache_dir@,$guile_site_ccache_dir,"\
    -e s,"@VERSION@,$VERSION,"\
    -e s,"mes/module/,$moduledir,"\
    build-aux/pre-inst-env.in\
    > pre-inst-env

chmod +x pre-inst-env

sed \
    -e s,"@srcdir@,$srcdir,"\
    -e s,"@abs_top_srcdir@,$abs_top_srcdir,"\
    -e s,"@abs_top_builddir@,$abs_top_builddir,"\
    -e s,"@top_builddir@,$top_builddir,"\
    -e s,"@top_builddest@,$top_builddest,"\
    -e s,"@BASH@,$BASH,"\
    -e s,"@GUILE@,$GUILE,"\
    -e s,"@guile_site_dir@,$guile_site_dir,"\
    -e s,"@guile_site_ccache_dir@,$guile_site_ccache_dir,"\
    -e s,"@VERSION@,$VERSION,"\
    -e s,"mes/module/,$moduledir,"\
    mes/module/mes/boot-0.scm.in\
    > mes/module/mes/boot-0.scm

sed \
    -e s,"@srcdir@,$srcdir,"\
    -e s,"@abs_top_srcdir@,$abs_top_srcdir,"\
    -e s,"@abs_top_builddir@,$abs_top_builddir,"\
    -e s,"@top_builddir@,$top_builddir,"\
    -e s,"@top_builddest@,$top_builddest,"\
    -e s,"@BASH@,$BASH,"\
    -e s,"@GUILE@,$GUILE,"\
    -e s,"@guile_site_dir@,$guile_site_dir,"\
    -e s,"@guile_site_ccache_dir@,$guile_site_ccache_dir,"\
    -e s,"@VERSION@,$VERSION,"\
    -e s,"mes/module/,$moduledir,"\
    scripts/mescc.in\
    > scripts/mescc

chmod +x scripts/mescc

if [ "$srcdir" != . ]; then
    mkdir -p mes/module/mes
    { cd mes/module/mes; ln -sf $abs_top_srcdir/mes/module/mes/*.mes .; }
fi

cat <<EOF
Run:
  prefix=$prefix ./build.sh      to build mes
  prefix=$prefix ./check.sh      to check mes
  prefix=$prefix ./install.sh    to install mes
EOF
