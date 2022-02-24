`fun` operations should be performed as if at the handler,
depsite tail resumption optimisation
  $ koka-zero interpret tail-resumption.kk
  10

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh tail-resumption.kk
  $ ./tail-resumption
  10
