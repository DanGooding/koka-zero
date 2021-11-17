(* TODO: is the aim to annotate the tree, or just to check it and then forget
   about types *)

val infer_type : Minimal_syntax.Expr.t -> Type.Poly.t

(* TODO: substitution monad? *)
