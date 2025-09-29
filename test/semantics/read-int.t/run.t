Read integers from stdin
  $ cat >in.txt <<EOF
  > 123
  > 456
  > EOF

  $ export PROJECT_ROOT=../../..
  $ ../../koka-zero.sh interpret sum-input.kk <in.txt
  input> input> 579

  $ ../../koka-zero.sh compile sum-input.kk
  $ ./sum-input <in.txt
  input> input> 579
