Read integers from stdin
  $ cat >in.txt <<EOF
  > 123
  > 456
  > EOF

  $ koka-zero interpret sum-input.kk <in.txt
  input> input> 579

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh sum-input.kk
  $ ./sum-input <in.txt
  input> input> 579
