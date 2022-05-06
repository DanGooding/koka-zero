open Koka_zero_inference

val translate_lambda
  :  Explicit_syntax.Expr.lambda
  -> Evidence_passing_syntax.Expr.lambda Generation.t

val translate
  :  Explicit_syntax.Program.t
  -> Evidence_passing_syntax.Program.t Generation.t

(** translate a program, but do not prepend the prelude function definitions.
    Exists for testing purposes. *)
val translate_no_prelude
  :  Explicit_syntax.Program.t
  -> Evidence_passing_syntax.Program.t Generation.t
