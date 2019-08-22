#!/bin/sh
set -e

opam init --bare
opam switch create . ocaml-base-compiler.4.06.1
eval $(opam config env)
opam install -y alcotest-lwt crowbar