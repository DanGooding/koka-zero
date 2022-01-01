open Core
open Koka_zero_util

(** convert a progam into [Koka_zero_inference.Minimal_syntax.t] representation,
    performing some desugaring and returning a [Static_error.t] for currently
    unsupported koka syntax *)
val program_to_minimal_syntax
  :  Syntax.program
  -> (Koka_zero_inference.Minimal_syntax.Program.t, Static_error.t) Result.t

val expr_to_minimal_syntax
  :  Syntax.expr
  -> (Koka_zero_inference.Minimal_syntax.Expr.t, Static_error.t) Result.t
