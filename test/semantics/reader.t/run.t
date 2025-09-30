A reader effect, similar to an environment variable
  $ export PROJECT_ROOT=../../..

  $ ../../koka-zero.sh interpret reader.kk
  42

  $ ../../koka-zero.sh compile reader.kk
  $ ./reader
  42

