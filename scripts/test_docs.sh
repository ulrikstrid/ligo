#!/bin/sh
cd gitlab-pages/docs 
dune clean 
dune build --verbose 
opam info mdx
dune runtest --verbose
