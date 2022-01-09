open Koka_zero_util
module Type = Type
module Effect = Effect
module Variable = Variable
module Minimal_syntax = Minimal_syntax
module Explicit_syntax = Explicit_syntax

(** typecheck a program, converting to a form with necessary types/effects made
    explicit. Returns a [Static_error] upon type errors *)
val infer_program
  :  Minimal_syntax.Program.t
  -> Explicit_syntax.Program.t Or_static_error.t

module Private : sig
  (** infer the type and effect of an expression, in the context containing the
      given declarations. exists for testing purposes *)
  val infer_expr_toplevel
    :  Minimal_syntax.Expr.t
    -> declarations:Minimal_syntax.Decl.t list
    -> (Type.Mono.t * Effect.t * Explicit_syntax.Expr.t) Or_static_error.t
end
