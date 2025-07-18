Should be able to thread state closures through code using effects
  $ koka-zero interpret state.kk
  55

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh state.kk
  $ ./state
  55

