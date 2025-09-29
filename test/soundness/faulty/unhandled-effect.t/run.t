Every operation requires an enclosing handler
  $ export PROJECT_ROOT=../../../..
  $ ../../../koka-zero.sh check unhandled-effect.kk
  type error: (("error when expanding constraint"
    ((type_lo (Arrow () (Metavariable em174) (Tuple ())))
     (type_hi (Arrow () (Labels ()) (Tuple ()))))
    (location Entry_point))
   ("constraint doesn't hold" (labels (read)) (expected_at_most (console))))
  [1]
