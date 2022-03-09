open Koka_zero_inference
open Koka_zero_util
module Evidence_passing_syntax = Evidence_passing_syntax

(** apply the evidence-passing translation to a program *)
val translate
  :  Explicit_syntax.Program.t
  -> Evidence_passing_syntax.Program.t Or_static_error.t

(** apply optimising transformations *)
val rewrite_program
  (* TODO: perhaps always use this? *)
  :  Evidence_passing_syntax.Program.t
  -> Evidence_passing_syntax.Program.t

module Free_variables = Free_variables

module Private : sig
  (** apply the evidence passing translation on an expression. Note generated
      names will not be unique across invocations *)
  val translate_expr
    :  Explicit_syntax.Expr.t
    -> Evidence_passing_syntax.Expr.t Or_static_error.t

  (** apply the evidence-passing translation to a program, but don't prepend the
      prelude functions. *)
  val translate_no_prelude
    :  Explicit_syntax.Program.t
    -> Evidence_passing_syntax.Program.t Or_static_error.t
end
