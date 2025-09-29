Access variables from multiple levels of enclosing scope
  $ export PROJECT_ROOT=../../..
  $ ../../koka-zero.sh interpret free-variables.kk
  55

  $ ../../koka-zero.sh compile free-variables.kk
  $ ./free-variables
  55
