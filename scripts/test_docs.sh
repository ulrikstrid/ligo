#!/bin/sh
cd gitlab-pages/docs && dune clean && dune build && dune runtest