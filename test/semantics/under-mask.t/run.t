`fun` opertions may be run at the perform-site, but they should
appear to run at the handler-site
  $ koka-zero interpret under.kk
  1

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh under.kk
  $ ./under
  1
