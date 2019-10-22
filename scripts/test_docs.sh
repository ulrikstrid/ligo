#!/bin/sh
cd gitlab-pages/docs 
dune clean 
dune build --verbose 
opam info mdx
which ocaml-mdx
which ocaml-mdx-test
ocaml-mdx --help=plain
ocaml-mdx-test --help=plain
dune runtest --verbose
