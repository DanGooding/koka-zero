Should be able to thread state closures through code using effects
  $ export PROJECT_ROOT=../../..
  $ ../../koka-zero.sh interpret state.kk
  55

  $ ../../koka-zero.sh compile state.kk
  $ ./state
  55

