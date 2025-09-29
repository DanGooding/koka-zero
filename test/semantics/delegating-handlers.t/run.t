Have an inner handler re-raise to an outer one
  $ export PROJECT_ROOT=../../..
  $ ../../koka-zero.sh interpret delegating-handlers.kk
  3

  $ ../../koka-zero.sh compile delegating-handlers.kk
  $ ./delegating-handlers
  3

