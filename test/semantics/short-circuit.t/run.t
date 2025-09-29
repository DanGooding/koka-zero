Boolean operators should only evaluate the right operand when required
  $ export PROJECT_ROOT=../../..
  $ ../../koka-zero.sh interpret short-circuit.kk
  1
  3

  $ ../../koka-zero.sh compile short-circuit.kk
  $ ./short-circuit
  1
  3
