Run a tail-recursive loop a large number of iterations
  $ cat >in.txt <<EOF
  > 10000000
  > EOF

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh tail-recursive.kk
  $ ./tail-recursive <in.txt
  input> 
