Read integers from stdin
  $ koka-zero interpret multiple-resumptions.kk
  0
  1
  10
  11
  100
  101
  110
  111
  1

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh multiple-resumptions.kk
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
