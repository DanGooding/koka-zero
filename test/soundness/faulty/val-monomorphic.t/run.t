Val bindings are not polymorphic
  $ export PROJECT_ROOT=../../../..
  $ ../../../koka-zero.sh check val-monomorphic.kk
  type error: (("error when expanding constraint"
    ((type_lo (Arrow ((Metavariable tm118)) (Labels ()) (Metavariable tm118)))
     (type_hi
      (Arrow ((Primitive Bool)) (Metavariable em58) (Metavariable tm120))))
    (location
     (Application (Value (Variable (User id))) ((Value (Literal (Bool true)))))))
   ("inconsistent types" (p Bool) (p' Int)))
  [1]
