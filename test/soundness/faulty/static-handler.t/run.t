Attempting to perform an effect not under a (dynamic) handler,
but within a function _defined_ under a handler (statically)
  $ koka-zero check static-handler.kk
  type error: cannot unify
  (Row (Open (Non_empty ((exn 1))) (Metavariable e36)))
  with
  (Row (Closed ()))
  
  [1]
