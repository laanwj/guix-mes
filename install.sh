#! /bin/sh

set -e
if [ -n "$BUILD_DEBUG" ]; then
    set -x
fi

# use bash or lose if pipes fail
[ -n "$BASH" ] && set -o pipefail

SHELL=${SHELL-$(command -v sh)}
prefix=${prefix-/usr/local}

MES_PREFIX=${MES_PREFIX-$prefix/share/mes}
MES_SEED=${MES_SEED-../MES-SEED}
TINYCC_SEED=${TINYCC_SEED-../TINYCC-SEED}

mkdir -p $DESTDIR$prefix/bin
cp src/mes $DESTDIR$prefix/bin/mes

mkdir -p $DESTDIR$prefix/lib
mkdir -p $DESTDIR$MES_PREFIX/lib
cp scripts/mescc $DESTDIR$prefix/bin/mescc

mkdir -p $DESTDIR$MES_PREFIX
tar -cf- doc guile include lib module scaffold | tar -xf- -C $DESTDIR$MES_PREFIX

GUILE_EFFECTIVE_VERSION=${GUILE_EFFECTIVE_VERSION-2.2}
datadir=${moduledir-$prefix/share/mes}
docdir=${moduledir-$prefix/share/doc/mes}
mandir=${mandir-$prefix/share/man}
moduledir=${moduledir-$datadir/module}
guile_site_dir=${moduledir-$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION}
guile_site_ccache_dir=${guile_site_ccache_dir-$prefix/lib/guile/$GUILE_EFFECTIVE_VERSION/site-ccache}
docdir=${moduledir-$prefix/share/doc/mes}

chmod +w $DESTDIR$prefix/bin/mescc
sed \
    -e "s,^#! /bin/sh,#! $SHELL," \
    -e "s,module/,$moduledir/," \
    -e "s,@datadir@,$datadir,g" \
    -e "s,@docdir@,$docdir,g" \
    -e "s,@guile_site_ccache_dir@,$guile_site_ccache_dir,g" \
    -e "s,@guile_site_dir@,$guile_site_dir,g" \
    -e "s,@moduledir@,$moduledir,g" \
    -e "s,@prefix@,$prefix,g" \
    -e "s,@VERSION@,$VERSION,g" \
    scripts/mescc > $DESTDIR$prefix/bin/mescc
chmod +w $DESTDIR$moduledir/mes/boot-0.scm
sed \
    -e "s,^#! /bin/sh,#! $SHELL," \
    -e "s,module/,$moduledir/," \
    -e "s,@datadir@,$datadir,g" \
    -e "s,@docdir@,$docdir,g" \
    -e "s,@guile_site_ccache_dir@,$guile_site_ccache_dir,g" \
    -e "s,@guile_site_dir@,$guile_site_dir,g" \
    -e "s,@moduledir@,$moduledir,g" \
    -e "s,@prefix@,$prefix,g" \
    -e "s,@VERSION@,$VERSION,g" \
    module/mes/boot-0.scm > $DESTDIR$moduledir/mes/boot-0.scm

sed \
    -e "s,^#! /bin/sh,#! $SHELL," \
    scripts/diff.scm > $DESTDIR$prefix/bin/diff.scm
chmod -w+x $DESTDIR$prefix/bin/diff.scm

if [ -f doc/mes.info ]; then
    mkdir -p $DESTDIR$prefix/share/info
    tar -cf- doc/mes.info* doc/images | tar -xf- --strip-components=1 -C $DESTDIR$prefix/share/info
    install-info --info-dir=$DESTDIR$prefix/share/info doc/mes.info
fi

if [ -f doc/mes.1 ]; then
    mkdir -p $DESTDIR$mandir/man1
    cp doc/mes.1 $DESTDIR$mandir/man1/
fi

if [ -f doc/mescc.1 ]; then
    mkdir -p $DESTDIR$mandir/man1
    cp doc/mescc.1 $DESTDIR$mandir/man1/
fi
