Construct and destruct options
  $ export PROJECT_ROOT=../../..
  $ ../../koka-zero.sh interpret option.kk
  1 1
  2 4
  3 9
  4 16
  5 25
  6 36
  7 49
  8 64
  9 81

  $ ../../koka-zero.sh compile option.kk
  $ ./option
  1 1
  2 4
  3 9
  4 16
  5 25
  6 36
  7 49
  8 64
  9 81
