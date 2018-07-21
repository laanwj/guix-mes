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

GUILE_EFFECTIVE_VERSION=${GUILE_EFFECTIVE_VERSION-$(guile -c '(display (effective-version))')}
bindir=${bindir-$prefix/bin}
datadir=${datadir-$prefix/share/mes}
docdir=${docdir-$prefix/share/doc/mes}
infodir=${infodir-$prefix/share/info}
mandir=${mandir-$prefix/share/man}
moduledir=${moduledir-$datadir/module}
guile_site_dir=${guile_site_dir-$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION}
guile_site_ccache_dir=${guile_site_ccache_dir-$prefix/lib/guile/$GUILE_EFFECTIVE_VERSION/site-ccache}

mkdir -p $DESTDIR$bindir
cp src/mes $DESTDIR$bindir/mes
cp scripts/mescc $DESTDIR$bindir/mescc

sed \
    -e "s,^#! /bin/sh,#! $SHELL," \
    scripts/diff.scm > $DESTDIR$bindir/diff.scm
chmod -w+x $DESTDIR$bindir/diff.scm


mkdir -p $docdir

if [ -n "$PERL" -a -n "$GIT" ]\
       && $PERL -v > /dev/null\
       && $GIT --status > /dev/null; then
    $PERL build-aux/gitlog-to-changelog --srcdir=. > ChangeLog
fi

cp\
    AUTHORS\
    BOOTSTRAP\
    COPYING\
    ChangeLog\
    HACKING\
    INSTALL\
    NEWS\
    README\
    $DESTDIR$docdir

if [ -f ${top_builddest}ChangeLog ]; then
    cp ${top_builddest}ChangeLog $DESTDIR$docdir
else
    cp ChangeLog $DESTDIR$docdir
fi

tar -cf- include lib | tar -xf- -C $DESTDIR$MES_PREFIX
tar -cf- scaffold --exclude='*.gcc*' --exclude='*.mes*' | tar -xf- -C $DESTDIR$MES_PREFIX
tar -cf- --exclude='*.go' module | tar -xf- -C $DESTDIR$MES_PREFIX
tar -cf- -C mes module | tar -xf- -C $DESTDIR$MES_PREFIX

mkdir -p $DESTDIR$guile_site_dir
mkdir -p $DESTDIR$guile_site_ccache_dir
tar -cf- -C module --exclude='*.go' . | tar -xf- -C $DESTDIR$guile_site_dir
tar -cf- -C module --exclude='*.scm' . | tar -xf- -C $DESTDIR$guile_site_ccache_dir

if [ -f doc/mes.info ]; then
    mkdir -p $DESTDIR$prefix/share/info
    tar -cf- doc/mes.info* doc/images | tar -xf- --strip-components=1 -C $DESTDIR$infodir
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
