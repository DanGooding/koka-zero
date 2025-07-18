Should be able to mix state and generator effects
  $ koka-zero interpret streams.kk
  2
  3
  5
  7
  11
  13
  17
  19
  23
  29

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh streams.kk
  verifier error: Function return type does not match operand type of return inst!
    ret i8 %not
   ptrFunction return type does not match operand type of return inst!
    ret i8 %not
   ptrFunction return type does not match operand type of return inst!
    ret i8 %not
   ptrFunction return type does not match operand type of return inst!
    ret i8 %not
   ptr
  [1]
  $ ./streams
  ./streams: No such file or directory
  [127]

