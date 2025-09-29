Yielding through other handlers
  $ export PROJECT_ROOT=../../..
  $ ../../koka-zero.sh interpret nested-handlers.kk
  13

  $ ../../koka-zero.sh compile nested-handlers.kk
  $ ./nested-handlers
  13

