#!/bin/sh

set -e
set -x

if [ -r .use-system-ghc ]; then
    system_ghc=--system-ghc
fi

stack $system_ghc install icfpc2020:solution

mkdir -p solution
cp -a ~/.local/bin/solution ./solution/
strip ./solution/solution
