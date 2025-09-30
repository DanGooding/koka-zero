Computing triangular numbers
  $ export PROJECT_ROOT=../../..

  $ cat >in.txt <<EOF
  > 100
  > EOF

  $ ../../koka-zero.sh interpret triangular.kk <in.txt
  input> 5050

  $ ../../koka-zero.sh compile triangular.kk
  $ ./triangular <in.txt
  input> 5050

