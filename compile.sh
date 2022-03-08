#!/bin/bash

set -e

# if this script is not run from the root directory of this project
# then the path to that must be passed in
PROJECT_ROOT=${PROJECT_ROOT:-.}

KOKA_COMPILER=${KOKA_COMPILER:-$PROJECT_ROOT/_build/default/bin/main.exe}

LL_C_COMPILER=clang
OPT_LEVEL=${OPT_LEVEL:-0}
# intentionally rely on clang to know the system's target triple
LL_C_FLAGS="-Wall -Wno-override-module -DENABLE_GC -O$OPT_LEVEL"

RUNTIME=$PROJECT_ROOT/lib/runtime/runtime.c
GC=/home/dan/boehm/gc

SOURCE="$1"
BASENAME="${SOURCE%.kk}"

IR="$BASENAME.ll"

BINARY="$BASENAME"

# try to avoid overwriting the source file
if [[ "$BINARY" = "$SOURCE" ]]; then
    BINARY="$BINARY.exe"
fi

$KOKA_COMPILER compile $SOURCE -o $IR
$LL_C_COMPILER $LL_C_FLAGS $IR $RUNTIME -I$GC/include -L$GC/lib -lgc -o $BINARY

