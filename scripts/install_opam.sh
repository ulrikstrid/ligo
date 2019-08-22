#!/bin/sh
set -e

wget https://github.com/ocaml/opam/releases/download/2.0.1/opam-2.0.1-x86_64-linux -O ${GIT_ROOT}/_external_bin/opam
chmod +x ${GIT_ROOT}/_external_bin/opam