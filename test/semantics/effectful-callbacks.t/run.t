Function which takes callbacks, which may have effects.
This tests the optimisation that uses a cheaper calling convention for pure functions,
since these are not guaranteed to be pure.

  $ export PROJECT_ROOT=../../..

  $ ../../koka-zero.sh interpret effectful-callbacks.kk -no-prelude
  5

  $ ../../koka-zero.sh compile effectful-callbacks.kk -no-prelude
  $ ./effectful-callbacks
  5

