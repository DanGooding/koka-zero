open Koka_zero_inference

val translate_expr : Minimal_syntax.Expr.t -> Evidence_passing_syntax.Expr.t
val translate : Minimal_syntax.Program.t -> Evidence_passing_syntax.Program.t
