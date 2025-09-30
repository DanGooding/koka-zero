Finding pythagorean triples
  $ export PROJECT_ROOT=../../..

  $ cat >in.txt <<EOF
  > 30
  > EOF

  $ ../../koka-zero.sh interpret pythagorean.kk <in.txt
  input> 3 4 5
  6 8 10
  5 12 13
  9 12 15
  8 15 17
  12 16 20
  15 20 25
  7 24 25
  10 24 26
  20 21 29

  $ ../../koka-zero.sh compile pythagorean.kk
  $ ./pythagorean <in.txt
  input> 3 4 5
  6 8 10
  5 12 13
  9 12 15
  8 15 17
  12 16 20
  15 20 25
  7 24 25
  10 24 26
  20 21 29

