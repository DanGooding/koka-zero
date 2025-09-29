Read integers from stdin
  $ export PROJECT_ROOT=../../..
  $ ../../koka-zero.sh interpret multiple-resumptions.kk
  0
  1
  10
  11
  100
  101
  110
  111
  1

  $ ../../koka-zero.sh compile multiple-resumptions.kk
  $ ./multiple-resumptions
  0
  1
  10
  11
  100
  101
  110
  111
  1
