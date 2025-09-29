#!/bin/bash

set -e

# This script is a little weird, since it runs in the working directory
# of each test case. The test cases each set PROJECT_ROOT, and we could make
# them set TEST_ROOT too, which might neaten this up.

CONFIG=koka-zero-config.sexp

sed "s|PROJECT_ROOT|$PROJECT_ROOT|g" \
  "$PROJECT_ROOT/test/koka-zero-config-template.sexp" \
  > $CONFIG

command="$1"
shift

case "$command" in
  "compile")
    koka-zero compile -config "$CONFIG" -optimise "$@"
    ;;

  "compile-no-opt")
    koka-zero compile -config "$CONFIG" "$@"
    ;;
  
  "interpret")
    koka-zero interpret -config "$CONFIG" "$@"
    ;;

  "check")
    koka-zero check -config "$CONFIG" "$@"
    ;;
esac
