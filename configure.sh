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

VERSION=0.18

# parse --prefix=prefix
cmdline=$(echo "$@")
p=${cmdline##*--prefix=}
p=${p% *}
p=${p% -*}
if [ -z "$p" ]; then
    p=${prefix-/usr/local}
fi
prefix=$p

srcdir=${srcdir-$(dirname $0)}
. ${srcdest}build-aux/trace.sh

BASH=${BASH-$(command -v bash)}
GUILE=${GUILE-$(command -v guile)} || true

if [ "$srcdir" = . ]; then
    top_builddir=.
else
    srcdest=${srcdest}
    top_builddir=$PWD
fi
abs_top_srcdir=${abs_top_srcdir-$(cd ${srcdir} && pwd)}
abs_top_builddir=$PWD

if [ -z "$GUILE" -o "$GUILE" = true ]; then
    GUILE_EFFECTIVE_VERSION=${GUILE_EFFECTIVE_VERSION-2.2}
else
    GUILE_EFFECTIVE_VERSION=${GUILE_EFFECTIVE_VERSION-$(guile -c '(display (effective-version))')}
fi
datadir=$(eval echo ${datadir-$prefix/share})
docdir=$(eval echo ${docdir-$datadir/doc/mes-$VERSION})
infodir=$(eval echo ${infodir-$datadir/info})
mandir=$(eval echo ${mandir-$datadir/man})
moduledir=$(eval echo ${moduledir-$datadir/mes/module})
moduledir_="$moduledir/"
guile_site_dir=$(eval echo ${guile_site_dir-$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION})
guile_site_ccache_dir=$(eval echo ${guile_site_ccache_dir-$prefix/lib/guile/$GUILE_EFFECTIVE_VERSION/site-ccache})
arch=$(get_machine || uname -m)

subst () {
    sed \
    -e s,"@srcdest@,$srcdest,"\
    -e s,"@srcdir@,$srcdir,"\
    -e s,"@abs_top_srcdir@,$abs_top_srcdir,"\
    -e s,"@abs_top_builddir@,$abs_top_builddir,"\
    -e s,"@top_builddir@,$top_builddir,"\
    -e s,"@BASH@,$BASH,"\
    -e s,"@GUILE@,$GUILE,"\
    -e s,"@prefix@,$prefix,"\
    -e s,"@guile_site_dir@,$guile_site_dir,"\
    -e s,"@guile_site_ccache_dir@,$guile_site_ccache_dir,"\
    -e s,"@VERSION@,$VERSION,"\
    -e s,"@arch@,$arch,"\
    -e s,"mes/module/,$moduledir/,"\
    $1 > $2
}

subst ${srcdest}build-aux/pre-inst-env.in pre-inst-env
chmod +x pre-inst-env
subst ${srcdest}mes/module/mes/boot-0.scm.in mes/module/mes/boot-0.scm
subst ${srcdest}scripts/mescc.in scripts/mescc

subst ${srcdest}build-aux/GNUmakefile.in GNUmakefile
subst ${srcdest}build-aux/build.sh.in build.sh
subst ${srcdest}build-aux/check.sh.in check.sh
subst ${srcdest}build-aux/install.sh.in install.sh
subst ${srcdest}build-aux/uninstall.sh.in uninstall.sh

chmod +x scripts/mescc

cat <<EOF
Run:
  ./build.sh      to build mes
  ./check.sh      to check mes
  ./install.sh    to install mes
EOF
