Yielding through other handlers
  $ koka-zero interpret nested-handlers.kk
  13

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh nested-handlers.kk
  $ ./nested-handlers
  13

