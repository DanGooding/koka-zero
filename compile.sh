#!/bin/bash

set -e

KC=./_build/default/bin/main.exe
CC=clang
CFLAGS=-Wall

RUNTIME=lib/runtime/runtime.c
GC_INCLUDE=/home/dan/boehm/gc/include
GC_LIB=/home/dan/boehm/gc/lib

SOURCE="$1"
BASENAME="${SOURCE%.kk}"

IR="$BASENAME.ll"

BINARY="$BASENAME"

# try to avoid overwriting the source file
if [[ "$BINARY" = "$SOURCE" ]]; then
    BINARY="$BINARY.exe"
fi

$KC compile $SOURCE -o $IR
$CC $CFLAGS $IR $RUNTIME -I$GC_INCLUDE -L$GC_LIB -lgc -o $BINARY

