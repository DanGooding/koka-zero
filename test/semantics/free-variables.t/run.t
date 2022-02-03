Access variables from multiple levels of enclosing scope
  $ koka-zero interpret free-variables.kk
  55

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh free-variables.kk
  $ ./free-variables
  55
