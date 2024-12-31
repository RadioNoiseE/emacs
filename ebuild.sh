#!/bin/sh

set -e

cat <<'EOF'
-----------------------------------------------------------------------------------
This script automates the build process of Emacs (more specifically the igc branch)
on Darwin and other UNIX platforms.  The dependencies are `git', `curl', and all
the dependencies a normal Emacs build requires.  You don't need to install the
igc specific dependencies and won't need GNU makeinfo for document generation for
a non-release Emacs as this script will take care of these.

You are recommended to create the working space in `/tmp' and drop in this script.
The options passed to `./configure' Emacs is specified in the `eflg' array below.

                                                   Copyright 2024, 2025 RadioNoiseE
-----------------------------------------------------------------------------------
EOF

# ------------
# Global Flags
# ------------

CFLAGS="-O3 -march=native -pipe"
LDFLAGS="-Wl,-O3"

# --------------------
# Emacs Source Prepare
# --------------------

git clone -b 'scratch/igc' --depth 1 https://git.savannah.gnu.org/git/emacs.git esrc

# ------------------------
# Install Makeinfo Locally
# ------------------------

curl -O https://ftp.gnu.org/gnu/texinfo/texinfo-7.2.tar.xz
tar -xf texinfo-7.2.tar.xz

mkdir epre

cd texinfo-7.2
./configure --prefix=`pwd`/../epre && make && make install # it's a release
cd ..

export PATH=$PATH:`pwd`/epre/bin

# ----------------------
# The Memory Pool System
# ----------------------

git clone --depth 1 https://github.com/Ravenbrook/mps.git emps

cd emps/code
cc $CFLAGS -c mps.c $LDFLAGS
ar rvs libmps.a mps.o
cd ../..

mkdir eart
cp emps/code/mps*.h emps/code/libmps.a eart

# ---------------------------
# Configure and Compile Emacs
# ---------------------------

eflg=(
  --disable-gc-mark-trace
  --without-all
  --with-xml2
  --with-native-image-api
  --with-ns
  --with-gnutls
  --with-toolkit-scroll-bars
  --with-small-ja-dic
  --with-tree-sitter
  --with-xwidgets
)

cd esrc
./autogen.sh && ./configure "CFLAGS=$CFLAGS" CPPFLAGS=-I`pwd`/../eart LDFLAGS=-L`pwd`/../eart --with-mps "${eflg[@]}" && \
  make -j && make install
