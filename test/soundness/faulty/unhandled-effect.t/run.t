Every operation requires an enclosing handler
  $ koka-zero check unhandled-effect.kk
  type error: (("error when expanding constraint"
    (type_lo (Arrow () (Metavariable em10) (Tuple ())))
    (type_hi (Arrow () (Labels ()) (Tuple ()))))
   ("constraint doesn't hold" (labels (read)) (expected_at_most (console))))
  [1]
