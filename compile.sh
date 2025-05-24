#!/bin/bash

set -e

# if this script is not run from the root directory of this project
# then the path to that must be passed in
PROJECT_ROOT=${PROJECT_ROOT:-.}

KOKA_COMPILER=${KOKA_COMPILER:-$PROJECT_ROOT/_build/default/bin/main.exe}

LL_C_COMPILER=clang
OPT_LEVEL=${OPT_LEVEL:-0}
if [ "$OPT_LEVEL" -gt 0 ]; then
    KOKA_OPT_FLAG=-optimise
else
    KOKA_OPT_FLAG=""
fi

# intentionally rely on clang to know the system's target triple
LL_C_FLAGS="-Wall -Wno-override-module -O$OPT_LEVEL"

RUNTIME=$PROJECT_ROOT/lib/runtime/runtime.c

# set DISABLE_GC to run without any deallocator (e.g. for debugging)
if [ -z "$DISABLE_GC" ]; then
    # GC enabled (default)
    GC_PATH=${GC_PATH:-/opt/homebrew/Cellar/bdw-gc/8.2.8}
    LL_C_FLAGS="$LL_C_FLAGS -DENABLE_GC -I$GC_PATH/include -L$GC_PATH/lib -lgc"
else
    echo "compiling without gargage collector"
fi

SOURCE="$1"
BASENAME="${SOURCE%.kk}"

IR="$BASENAME.ll"

BINARY="$BASENAME"

# try to avoid overwriting the source file
if [[ "$BINARY" = "$SOURCE" ]]; then
    BINARY="$BINARY.exe"
fi

$KOKA_COMPILER compile $SOURCE -o $IR $KOKA_OPT_FLAG
$LL_C_COMPILER $LL_C_FLAGS $IR $RUNTIME -o $BINARY

