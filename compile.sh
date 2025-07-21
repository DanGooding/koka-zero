#!/bin/bash

set -e

# if this script is not run from the root directory of this project
# then the path to that must be passed in
PROJECT_ROOT="${PROJECT_ROOT:-.}"

opam exec -- dune exec -- bin/main.exe compile -config "$PROJECT_ROOT/koka-zero-config.sexp" "$@"
