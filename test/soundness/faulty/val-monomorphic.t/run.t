Val bindings are not polymorphic
  $ koka-zero check val-monomorphic.kk
  type error: (("error when expanding constraint"
    (type_lo (Arrow ((Metavariable tm1)) (Labels ()) (Metavariable tm1)))
    (type_hi
     (Arrow ((Primitive Bool)) (Unknown (Metavariable em2)) (Metavariable tm3))))
   ("inconsistent types" (p Bool) (p' Int)))
  [1]
