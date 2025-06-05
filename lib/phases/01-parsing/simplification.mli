open! Core
open Import

(** convert a progam into [Koka_zero_inference.Minimal_syntax.t] representation,
    performing some desugaring and returning a [Static_error.t] for currently
    unsupported koka syntax *)
val simplify_program
  :  Syntax.program
  -> Minimal_syntax.Program.t Or_static_error.t

val simplify_expr : Syntax.expr -> Minimal_syntax.Expr.t Or_static_error.t
