#! /bin/sh

set -e

if [ -n "$BUILD_DEBUG" ]; then
    set -x
fi

mkdir -p src
cd src

GUILE_EFFECTIVE_VERSION=2.2
GUILE_SITE_DIR=/usr/local/share/guile/site/$GUILE_EFFECTIVE_VERSION
GUILE_SITE_CCACHE_DIR=/usr/local/lib/guile/$GUILE_EFFECTIVE_VERSION/site-ccache
GUILE_LOAD_PATH=$GUILE_SITE_DIR
GUILE_LOAD_COMPILED_PATH=$GUILE_SITE_CCACHE_DIR
export GUILE_LOAD_PATH
export GUILE_LOAD_COMPILED_PATH

sudo apt-get install --no-install-recommends build-essential ca-certificates gcc-i686-linux-gnu guile-$GUILE_EFFECTIVE_VERSION-dev help2man texinfo
echo checking for M1
if ! command -v M1; then
    if sudo apt-get install mescc-tools; then
        echo yay
    else
        wget -O mescc-tools-Release_0.5.1.tar.gz https://github.com/oriansj/mescc-tools/archive/Release_0.5.1.tar.gz
        tar xf mescc-tools-Release_0.5.1.tar.gz
        cd mescc-tools-Release_0.5.1
        make
        make check
        sudo make install
        cd ..
    fi
fi

echo checking for Nyacc
if ! guile -c '(use-modules (nyacc lalr)) (display *nyacc-version*) (newline)'; then
    if sudo apt-get install --no-install-recommends nyacc; then
        echo yay
    else
        wget -O nyacc-v0.80.43.tar.gz https://gitlab.com/janneke/nyacc/-/archive/v0.80.43/nyacc-v0.80.43.tar.gz
        tar xf nyacc-v0.80.43.tar.gz
        cd nyacc-v0.80.43
        ./configure --prefix=/usr/local
        make
        make check
        sudo make install
        cd ..
    fi
fi

echo checking for mes
if ! command -v mes; then
    sudo apt-get install --no-install-recommends git
    git clone http://gitlab.com/janneke/mes
    cd mes
    git checkout wip-gnu
    ./configure
    make
    make check
    make install
fi
