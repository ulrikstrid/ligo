#!/bin/sh
set -e
opam install -y --build-test --deps-only ./src/
# TODO: also try instead from time to time:
#- (cd ./src/; dune build -p ligo)
dune build -p ligo
dune build @ligo-test
