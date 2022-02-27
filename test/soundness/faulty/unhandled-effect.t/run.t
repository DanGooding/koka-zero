Every operation requires an enclosing handler
  $ koka-zero check unhandled-effect.kk
  type error: cannot unify
  (Row (Open (Non_empty ((read 1))) (Metavariable e17)))
  with
  (Row (Closed ()))
  
  [1]
