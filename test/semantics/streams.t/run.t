Should be able to mix state and generator effects
  $ export PROJECT_ROOT=../../..
  $ ../../koka-zero.sh interpret streams.kk
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

  $ ../../koka-zero.sh compile streams.kk
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

