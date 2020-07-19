#!/bin/sh

set -e
set -x

hpack
cabal v1-clean || true
cabal v1-configure --flags="static"
cabal v1-build -j exe:solution

mkdir -p solution
cp -a dist/build/solution/solution ./solution/
strip ./solution/solution
