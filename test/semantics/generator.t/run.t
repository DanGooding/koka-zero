A generator yielding the fibonacci sequence
  $ export PROJECT_ROOT=../../..

  $ cat >in.txt <<EOF
  > 100
  > EOF

  $ ../../koka-zero.sh interpret generator.kk <in.txt
  input> 0
  1
  1
  2
  3
  5
  8
  13
  21
  34
  55
  89

  $ ../../koka-zero.sh compile generator.kk
  $ ./generator <in.txt
  input> 0
  1
  1
  2
  3
  5
  8
  13
  21
  34
  55
  89

