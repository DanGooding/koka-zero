Every operation requires an enclosing handler
  $ koka-zero check unhandled-effect.kk
  type error: (("error when expanding constraint"
    (type_lo (Arrow () (Unknown (Metavariable em6)) (Primitive Unit)))
    (type_hi (Arrow () (Labels ()) (Primitive Unit))))
   ("less-than constraint doesn't hold" (labels (read))
    (expected_at_most (console))))
  [1]
