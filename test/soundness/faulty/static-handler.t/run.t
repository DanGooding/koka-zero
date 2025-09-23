Attempting to perform an effect not under a (dynamic) handler,
but within a function _defined_ under a handler (statically)
  $ koka-zero check static-handler.kk
  type error: (("error when expanding constraint"
    ((type_lo (Arrow () (Metavariable em25) (Tuple ())))
     (type_hi (Arrow () (Labels ()) (Tuple ()))))
    (location Entry_point))
   ("constraint doesn't hold" (labels (exn)) (expected_at_most (console))))
  [1]
