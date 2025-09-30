Implementing choice as an effect
  $ export PROJECT_ROOT=../../..

  $ cat >in.txt <<EOF
  > 1
  > 0
  > 1
  > EOF

  $ ../../koka-zero.sh interpret choose.kk <in.txt
  input> input> input> 1

  $ ../../koka-zero.sh compile choose.kk
  $ ./choose <in.txt
  input> input> input> 1

