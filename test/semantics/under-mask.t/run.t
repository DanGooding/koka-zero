`fun` opertions may be run at the perform-site, but they should
appear to run at the handler-site
  $ export PROJECT_ROOT=../../..
  $ ../../koka-zero.sh interpret under.kk
  1

  $ ../../koka-zero.sh compile under.kk
  $ ./under
  1
