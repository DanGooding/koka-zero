#!/bin/bash

set -e

KOKA_COMPILER=koka-zero OPT_LEVEL=2 $PROJECT_ROOT/compile.sh "$@"

