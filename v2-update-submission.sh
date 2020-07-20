#!/bin/sh

set -e
set -x

hpack
cabal v2-clean || true
cabal v2-configure --flags="static"
cabal v2-build -j exe:solution
cabal v2-test
cabal v2-build --flag gui

mkdir -p solution
cp -a dist/build/solution/solution ./solution/
strip ./solution/solution
