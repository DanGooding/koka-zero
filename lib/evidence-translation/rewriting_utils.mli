open Evidence_passing_syntax

(** apply the given rewrite at every expression node in this expression
    (including the root). This performs one iteration of non-overlapping
    rewrites, so should be repeated until no change is observed *)
val apply_everywhere
  :  rewrite:(Expr.t -> Expr.t Modified.t)
  -> Expr.t
  -> Expr.t Modified.t

(** apply the given rewrite at every expression node in a program. This performs
    one iteration of non-overlapping rewrites, so should be repeated until no
    change is observed *)
val apply_everywhere_to_program
  :  rewrite:(Expr.t -> Expr.t Modified.t)
  -> Program.t
  -> Program.t Modified.t
