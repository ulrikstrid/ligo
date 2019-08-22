#!/bin/sh
set -e

eval $(opam env)
opam install -y ocp-indent tuareg merlin
opam user-setup install