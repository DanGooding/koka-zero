Boolean operators should only evaluate the right operand when required
  $ koka-zero interpret short-circuit.kk
  1
  3

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh short-circuit.kk
  $ ./short-circuit
  1
  3
