#!/bin/bash

# TODO: must apply to outer dir?
make clean
make

# how to do a version string?
KOKA_ZERO_COMMIT=$(git rev-parse HEAD)
echo koka-zero commit $KOKA_ZERO_COMMIT

clang --version
# TODO: requires $GC (.env ?)
GC_VERSION=$(egrep 'version [[:digit:]]+\.[[:digit:]]+\.[[:digit:]]+' -oh $GC/README.md)
echo Boehm-GC $GC_VERSION
