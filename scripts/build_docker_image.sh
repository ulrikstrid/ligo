#!/bin/sh
set -e
docker build -t "${LIGO_REGISTRY_IMAGE_BUILD:-ligolang/ligo}:next" -f ./scripts/distribution/debian/distribute.Dockerfile .
