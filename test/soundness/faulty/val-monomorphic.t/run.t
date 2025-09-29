Val bindings are not polymorphic
  $ export PROJECT_ROOT=../../../..
  $ ../../../koka-zero.sh check val-monomorphic.kk
  type error: (("error when expanding constraint"
    ((type_lo (Arrow ((Metavariable tm320)) (Labels ()) (Metavariable tm320)))
     (type_hi
      (Arrow ((Primitive Bool)) (Metavariable em175) (Metavariable tm322))))
    (location
     (Application (Value (Variable (User id))) ((Value (Literal (Bool true)))))))
   ("inconsistent types" (p Bool) (p' Int)))
  [1]
