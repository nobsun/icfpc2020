#!/bin/sh

set -e
set -x

# build docker image
docker build -t sampou-icfpc2020-dev -f env/dev/Dockerfile env/dev/

# build solution
mkdir -p solution
docker run -it --name icfpc2020-build --rm --network none -v `pwd`:/icfpc2020 -w /icfpc2020 sampou-icfpc2020-dev bash -c "stack build && cp -a \`stack path --local-install-root\`/bin/solution ./solution/"

# sanity check
docker run -it --name icfpc2020-build --rm --network none -v `pwd`/solution:/solution -w /solution icfpcontest2020/haskell  ldd ./solution
