#! /bin/sh

set -e
set -o pipefail

if [ -n "$BUILD_DEBUG" ]; then
    set -x
fi

SHELL=${SHELL-$(command -v sh)}
PREFIX=${PREFIX-/usr/local}
MES_PREFIX=${MES_PREFIX-$PREFIX/share/mes}
MES_SEED=${MES_SEED-../mes-seed}
TINYCC_SEED=${TINYCC_SEED-../tinycc-seed}

mkdir -p $DESTDIR$PREFIX/bin
cp src/mes $DESTDIR$PREFIX/bin/mes

mkdir -p $DESTDIR$PREFIX/lib
mkdir -p $DESTDIR$MES_PREFIX/lib
cp scripts/mescc $DESTDIR$PREFIX/bin/mescc

mkdir -p $DESTDIR$MES_PREFIX
tar -cf- doc guile include lib module scaffold | tar -xf- -C $DESTDIR$MES_PREFIX

GUILE_EFFECTIVE_VERSION=${GUILE_EFFECTIVE_VERSION-2.2}
DATADIR=${MODULEDIR-$PREFIX/share/mes}
DOCDIR=${MODULEDIR-$PREFIX/share/doc/mes}
MODULEDIR=${MODULEDIR-$DATADIR/module}
GUILEDIR=${MODULEDIR-$PREFIX/share/guile/site/$GUILE_EFFECTIVE_VERSION}
GODIR=${GODIR-$PREFIX/lib/guile/$GUILE_EFFECTIVE_VERSION/site-ccache}
DOCDIR=${MODULEDIR-$PREFIX/share/doc/mes}

chmod +w $DESTDIR$PREFIX/bin/mescc
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
    scripts/mescc > $DESTDIR$PREFIX/bin/mescc
chmod +w $DESTDIR$MODULEDIR/mes/boot-0.scm
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
    module/mes/boot-0.scm > $DESTDIR$MODULEDIR/mes/boot-0.scm

sed \
    -e "s,^#! /bin/sh,#! $SHELL," \
    scripts/diff.scm > $DESTDIR$PREFIX/bin/diff.scm
chmod -w+x $DESTDIR$PREFIX/bin/diff.scm

if [ -f doc/mes.info ]; then
    mkdir -p $DESTDIR$PREFIX/share/info
    install-info --info-dir=$DESTDIR$PREFIX/share/info doc/mes.info
    tar -cf- doc/mes.info* | tar -xf- --strip-components=1 -C $DESTDIR$PREFIX/share/info
fi

if [ -f doc/mes.1 ]; then
    mkdir -p $DESTDIR$PREFIX/man/man1
    cp doc/mes.1 $DESTDIR$PREFIX/man/man1/
fi

if [ -f doc/mescc.1 ]; then
    mkdir -p $DESTDIR$PREFIX/man/man1
    cp doc/mescc.1 $DESTDIR$PREFIX/man/man1/
fi
