The extremely fast-growing Ackermann function
  $ export PROJECT_ROOT=../../..

  $ cat >in.txt <<EOF
  > 3
  > 3
  > EOF

  $ ../../koka-zero.sh interpret ackermann.kk <in.txt
  input> input> 61

  $ ../../koka-zero.sh compile ackermann.kk
  $ ./ackermann <in.txt
  input> input> 61

