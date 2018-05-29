#! /bin/sh

set -e

export PREFIX
SHELL=${SHELL-$(command -v sh)}
PREFIX=${PREFIX-/usr/local}
MES_PREFIX=${MES_PREFIX-$PREFIX/share/mes}
MES_SEED=${MES_SEED-../mes-seed}
TINYCC_SEED=${TINYCC_SEED-../tinycc-seed}

mkdir -p $PREFIX/bin
cp src/mes $PREFIX/bin/mes

mkdir -p $PREFIX/lib
mkdir -p $MES_PREFIX/lib
cp scripts/mescc $PREFIX/bin/mescc

mkdir -p $MES_PREFIX
tar -cf- doc guile include lib module scaffold | tar -xf- -C $MES_PREFIX

GUILE_EFFECTIVE_VERSION=${GUILE_EFFECTIVE_VERSION-2.2}
DATADIR=${MODULEDIR-$PREFIX/share/mes}
DOCDIR=${MODULEDIR-$PREFIX/share/doc/mes}
MODULEDIR=${MODULEDIR-$DATADIR/module}
GUILEDIR=${MODULEDIR-$PREFIX/share/guile/site/$GUILE_EFFECTIVE_VERSION}
GODIR=${GODIR-$PREFIX/lib/guile/$GUILE_EFFECTIVE_VERSION/site-ccache}
DOCDIR=${MODULEDIR-$PREFIX/share/doc/mes}

chmod +w $PREFIX/bin/mescc
sed \
    -e "s,^#! /bin/sh,#! $SHELL," \
    -e "s,module/,$MODULEDIR/," \
    -e "s,@DATADIR@,$DATADIR,g" \
    -e "s,@DOCDIR@,$DOCDIR,g" \
    -e "s,@GODIR@,$GODIR,g" \
    -e "s,@GUILEDIR@,$GUILEDIR,g" \
    -e "s,@MODULEDIR@,$MODULEDIR,g" \
    -e "s,@PREFIX@,$PREFIX,g" \
    -e "s,@VERSION@,$VERSION,g" \
    scripts/mescc > $PREFIX/bin/mescc
chmod +w $MODULEDIR/mes/boot-0.scm
sed \
    -e "s,^#! /bin/sh,#! $SHELL," \
    -e "s,module/,$MODULEDIR/," \
    -e "s,@DATADIR@,$DATADIR,g" \
    -e "s,@DOCDIR@,$DOCDIR,g" \
    -e "s,@GODIR@,$GODIR,g" \
    -e "s,@GUILEDIR@,$GUILEDIR,g" \
    -e "s,@MODULEDIR@,$MODULEDIR,g" \
    -e "s,@PREFIX@,$PREFIX,g" \
    -e "s,@VERSION@,$VERSION,g" \
    module/mes/boot-0.scm > $MODULEDIR/mes/boot-0.scm

cp scripts/diff.scm $PREFIX/bin/diff.scm
sed \
    -e "s,^#! /bin/sh,#! $SHELL," \
    scripts/diff.scm > $PREFIX/bin/diff.scm
