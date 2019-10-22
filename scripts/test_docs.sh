#!/bin/sh
cd gitlab-pages/docs && dune clean && dune build --verbose && dune runtest --verbose