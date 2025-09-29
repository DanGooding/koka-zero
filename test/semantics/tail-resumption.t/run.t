`fun` operations should be performed as if at the handler,
depsite tail resumption optimisation
  $ export PROJECT_ROOT=../../..
  $ ../../koka-zero.sh interpret tail-resumption.kk
  10

  $ ../../koka-zero.sh compile tail-resumption.kk
  $ ./tail-resumption
  10
