Cooperative multithreading - two tasks alternate
  $ export PROJECT_ROOT=../../..

  $ ../../koka-zero.sh interpret coroutines.kk
  1
  10
  2
  20
  3
  30
  40

  $ ../../koka-zero.sh compile coroutines.kk
  $ ./coroutines
  1
  10
  2
  20
  3
  30
  40

