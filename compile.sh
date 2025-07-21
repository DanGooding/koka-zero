#!/bin/bash

set -e

# if this script is not run from the root directory of this project
# then the path to that must be passed in
PROJECT_ROOT="${PROJECT_ROOT:-.}"

KOKA_COMPILER="${KOKA_COMPILER:-$PROJECT_ROOT/_build/default/bin/main.exe}"
KOKA_CONFIG="$PROJECT_ROOT/koka-zero-config.sexp"

"$KOKA_COMPILER" compile -config "$KOKA_CONFIG" "$@"
