Various pattern matching
  $ koka-zero interpret pattern-matching.kk
  1 2 3 
  1 0 1 

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh pattern-matching.kk
  codegen error: verifier error: PHINode should have one entry for each predecessor of its parent basic block!
    %incoming = phi ptr [ null, %switch_false ], [ inttoptr (i64 1 to ptr), %switch_true ]
  Terminator found in the middle of a basic block!
  label %switch_false
  Terminator found in the middle of a basic block!
  label %switch_true
  
  [2]
  $ ./pattern-matching
  ./pattern-matching: No such file or directory
  [127]

