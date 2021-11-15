(* TODO: is the aim to annotate the tree, or just to check it and then forget
   about types *)

val infer_type : Expr.t -> Type.Scheme.t

(* TODO: substitution monad? *)
