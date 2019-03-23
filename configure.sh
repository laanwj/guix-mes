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

VERSION=0.19
srcdir=${srcdir-$(dirname $0)}
. ${srcdest}build-aux/trace.sh

# parse --mes
cmdline=$(echo " $@")
p=${cmdline/ --mes/}
if [ "$p" != "$cmdline" ]; then
    mes_p=${mes_p-1}
fi

# parse --prefix=PREFIX
p=${cmdline/ --prefix=/ -prefix=}
if [ "$p" != "$cmdline" ]; then
    p=${p##* -prefix=}
    p=${p% *}
    p=${p% -*}
    prefix=${p-/usr/local}

else
    prefix=${prefix-/usr/local}
fi

# parse --program-prefix=
p=${cmdline/ --program-prefix=/ -program-prefix=}
if [ "$p" != "$cmdline" ]; then
    p=${p##* -program-prefix=}
    p=${p% *}
    p=${p% -*}
    program_prefix=$p
fi

AR=${AR-$(command -v ar)} || true
BASH=${BASH-$(command -v bash)}
BLOOD_ELF=${BLOOD_ELF-$(command -v blood-elf)}
CC=${CC-$(command -v gcc)} || true
GUILD=${GUILD-$(command -v guild)} || true
GUILE_TOOLS=${GUILE_TOOLS-$(command -v guile-tools)} || true
if [ ! "$GUILD" ]; then
    if [ "$GUILE_TOOLS" ]; then
        GUILD=$GUILE_TOOLS
    else
        GUILD=true
    fi
fi
GUILE=${GUILE-$(command -v guile)} || true
HEX2=${HEX2-$(command -v hex2)}
M1=${M1-$(command -v M1)}
MES_FOR_BUILD=${MES_FOR_BUILD-$(command -v mes || command -v guile || echo mes)}
GIT=${GIT-$(command -v git)} || true
PERL=${PERL-$(command -v perl)} || true
MES_SEED=${MES_SEED-../mes-seed}

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
bindir=$(eval echo ${bindir-$prefix/bin})
datadir=$(eval echo ${datadir-$prefix/share})
docdir=$(eval echo ${docdir-$datadir/doc/mes-$VERSION})
infodir=$(eval echo ${infodir-$datadir/info})
libdir=$(eval echo ${libdir-$prefix/lib})
mandir=$(eval echo ${mandir-$datadir/man})
moduledir=$(eval echo ${moduledir-$datadir/mes/module})
moduledir_="$moduledir/"
guile_site_dir=$(eval echo ${guile_site_dir-$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION})
guile_site_ccache_dir=$(eval echo ${guile_site_ccache_dir-$prefix/lib/guile/$GUILE_EFFECTIVE_VERSION/site-ccache})

subst () {
    sed \
    -e s,"@PACKAGE@,$PACKAGE,"\
    -e s,"@VERSION@,$VERSION,"\
    -e s,"@arch@,$arch,"\
    -e s,"@build@,$build,"\
    -e s,"@host@,$host,"\
    -e s,"@compiler@,$compiler,"\
    -e s,"@gcc_p@,$gcc_p,"\
    -e s,"@mes_p@,$mes_p,"\
    -e s,"@mesc_p@,$mesc_p,"\
    -e s,"@tcc_p@,$tcc_p,"\
    -e s,"@mes_arch@,$mes_arch,"\
    -e s,"@with_glibc_p@,$with_glibc_p,"\
    -e s,"@abs_top_srcdir@,$abs_top_srcdir,"\
    -e s,"@abs_top_builddir@,$abs_top_builddir,"\
    -e s,"@top_builddir@,$top_builddir,"\
    -e s,"@srcdest@,$srcdest,"\
    -e s,"@srcdir@,$srcdir,"\
    -e s,"@prefix@,$prefix,"\
    -e s,"@program_prefix@,$program_prefix,"\
    -e s,"@bindir@,$bindir,"\
    -e s,"@datadir@,$datadir,"\
    -e s,"@docdir@,$docdir,"\
    -e s,"@guile_site_dir@,$guile_site_dir,"\
    -e s,"@guile_site_ccache_dir@,$guile_site_ccache_dir,"\
    -e s,"@infodir@,$infodir,"\
    -e s,"@libdir@,$libdir,"\
    -e s,"@mandir@,$mandir,"\
    -e s,"@moduledir@,$moduledir,"\
    -e s,"@sysconfdir@,$sysconfdir,"\
    -e s,"@GUILE_EFFECTIVE_VERSION@,$GUILE_EFFECTIVE_VERSION,"\
    -e s,"@V@,$V,"\
    -e s,"@AR@,$AR,"\
    -e s,"@BASH@,$BASH,"\
    -e s,"@BLOOD_ELF@,$BLOOD_ELF,"\
    -e s,"@CC@,$CC,"\
    -e s,"@GIT@,$GIT,"\
    -e s,"@GUILD@,$GUILD,"\
    -e s,"@GUILE@,$GUILE,"\
    -e s,"@PERL@,$PERL,"\
    -e s,"@CFLAGS@,$CFLAGS,"\
    -e s,"@HEX2@,$HEX2,"\
    -e s,"@HEX2FLAGS@,$HEX2FLAGS,"\
    -e s,"@M1@,$M1,"\
    -e s,"@M1FLAGS@,$M1FLAGS,"\
    -e s,"@MES_FOR_BUILD@,$MES_FOR_BUILD,"\
    -e s,"@MES_SEED@,$MES_SEED,"\
    -e s,"@MES_SEED@,$MES_SEED,"\
    -e s,"@SHELL@,$SHELL,"\
    -e s,"mes/module/,$moduledir/,"\
    $1 > $2
}

subst ${srcdest}build-aux/pre-inst-env.in pre-inst-env
chmod +x pre-inst-env
subst ${srcdest}scripts/mescc.in scripts/mescc
chmod +x scripts/mescc
subst ${srcdest}scripts/mescc.scm.in scripts/mescc.scm
chmod +x scripts/mescc.scm

host=${host-$($CC -dumpmachine 2>/dev/null || echo x86)}
if [ -z "$host" ]; then
    arch=${arch-$(get_machine || uname -m)}
else
    arch=${host%%-*}
fi
if [ "$arch" = i386\
             -o "$arch" = i486\
             -o "$arch" = i586\
             -o "$arch" = i686\
   ]; then
    arch=x86
fi

#
if $CC --version | grep gcc; then #2>/dev/null; then
    gcc_p=1
    compiler=gcc
elif $CC --version | grep tcc; then #2>/dev/null; then
    tcc_p=1
    compiler=tcc
else
    mes_p=1
    mesc_p=1
    compiler=mescc
fi

mes_arch=$arch
if [ "$mes_p" -o "$mesc_p" ]; then
    mes_arch=$arch-mes
fi

if [ ! "$mesc_p" ]; then
    mes_arch=$mes_arch-$compiler
fi
if [ ! "$mesc_p" -a ! "$mes_p" ]; then
    with_glibc_p=1
fi

subst ${srcdest}mes/module/mes/boot-0.scm.in mes/module/mes/boot-0.scm
subst ${srcdest}build-aux/GNUmakefile.in GNUmakefile
subst ${srcdest}build-aux/config.status.in config.status
subst ${srcdest}build-aux/build.sh.in build.sh
chmod +x build.sh
subst ${srcdest}build-aux/bootstrap.sh.in bootstrap.sh
chmod +x bootstrap.sh
subst ${srcdest}build-aux/check.sh.in check.sh
chmod +x check.sh
subst ${srcdest}build-aux/install.sh.in install.sh
chmod +x install.sh
subst ${srcdest}build-aux/uninstall.sh.in uninstall.sh
chmod +x uninstall.sh

cat <<EOF
GNU Mes is configured for $mes_arch

Run:
  ./build.sh      to build mes
  ./check.sh      to check mes
  ./install.sh    to install mes
EOF
