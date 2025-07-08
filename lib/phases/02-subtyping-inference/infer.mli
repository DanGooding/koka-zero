open! Core
open! Import

val infer_expr_toplevel
  :  Minimal_syntax.Expr.t
  -> declarations:Minimal_syntax.Decl.t list
  -> (Polar_type.t * Polar_type.Effect.t * Explicit_syntax.Expr.t) Or_error.t

val infer_program
  :  Minimal_syntax.Program.t
  -> Explicit_syntax.Program.t Or_error.t

val infer_program_without_main
  :  Minimal_syntax.Program.t
  -> Explicit_syntax.Program.t Or_error.t
