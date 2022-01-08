open Koka_zero_inference

val translate_expr
  :  Explicit_syntax.Expr.t
  -> Evidence_passing_syntax.Expr.t Generation.t

val translate
  :  Explicit_syntax.Program.t
  -> Evidence_passing_syntax.Program.t Generation.t
