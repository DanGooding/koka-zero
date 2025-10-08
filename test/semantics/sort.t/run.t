Sort a list of integers

Format: length, followed by the elements
  $ cat >in.txt <<EOF
  > 30
  > 28
  > 24
  > 22
  > 8
  > 12
  > 27
  > 21
  > 19
  > 2
  > 13
  > 23
  > 6
  > 0
  > 27
  > 2
  > 16
  > 9
  > 0
  > 26
  > 24
  > 6
  > 5
  > 5
  > 11
  > 23
  > 7
  > 15
  > 23
  > 12
  > 7
  > EOF

  $ export PROJECT_ROOT=../../..
  $ ../../koka-zero.sh interpret sort.kk <in.txt
  input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> 
  0 0 2 2 5 5 6 6 7 7 8 9 11 12 12 13 15 16 19 21 22 23 23 23 24 24 26 27 27 28 

  $ ../../koka-zero.sh compile sort.kk
  $ ./sort <in.txt
  input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> input> 
  0 0 2 2 5 5 6 6 7 7 8 9 11 12 12 13 15 16 19 21 22 23 23 23 24 24 26 27 27 28 
