open Koka_zero_inference

val translate_expr : Minimal_syntax.Expr.t -> Monadic_syntax.Expr.t
val translate : Minimal_syntax.Program.t -> Monadic_syntax.Program.t
