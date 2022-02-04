Have an inner handler re-raise to an outer one
  $ koka-zero interpret delegating-handlers.kk
  3

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh delegating-handlers.kk
  $ ./delegating-handlers
  3

