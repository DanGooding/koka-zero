Run a tail-recursive loop a large number of iterations
  $ export PROJECT_ROOT=../../..
  $ cat >in.txt <<EOF
  > 10000000
  > EOF

  $ ../../koka-zero.sh compile tail-recursive.kk
  $ ./tail-recursive <in.txt
  input> 
