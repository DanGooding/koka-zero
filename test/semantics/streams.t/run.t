Should be able to mix state and generator effects
  $ koka-zero interpret streams.kk
  2
  3
  5
  7
  11
  13
  17
  19
  23
  29

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh streams.kk
  $ ./streams
  2
  3
  5
  7
  11
  13
  17
  19
  23
  29

