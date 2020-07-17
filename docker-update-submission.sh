#!/bin/sh

set -e
set -x

mkdir -p solution
docker run -it --name icfpc2020-build --rm --network none -v `pwd`:/icfpc2020 icfpcontest2020/haskell bash -c "cd /icfpc2020 && stack build && cp -a \`stack path --local-install-root\`/bin/solution ./solution/"
