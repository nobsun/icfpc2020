#!/bin/sh

set -e
set -x

hpack
cabal v1-clean
cabal v1-configure
cabal v1-build -j exe:solution

mkdir -p solution
cp -a dist/build/solution/solution ./solution/
strip ./solution/solution
