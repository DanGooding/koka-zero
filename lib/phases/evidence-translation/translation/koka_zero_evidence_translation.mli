open! Core
open! Import

(** apply the evidence-passing translation to a program *)
val translate
  :  Explicit_syntax.Program.t
  -> Evidence_passing_syntax.Program.t Or_static_error.t

module Private : sig
  (** apply the evidence passing translation to a lambda, adding an evidence
      vector as its final parameter. Note generated names will not be unique
      across invocations *)
  val translate_lambda
    :  Explicit_syntax.Expr.lambda
    -> Evidence_passing_syntax.Expr.lambda Or_static_error.t

  (** apply the evidence-passing translation to a program, but don't prepend the
      prelude functions. *)
  val translate_no_prelude
    :  Explicit_syntax.Program.t
    -> Evidence_passing_syntax.Program.t Or_static_error.t

  val translate_expr
    :  Explicit_syntax.Expr.t
    -> evv:Evidence_passing_syntax.Expr.t
    -> Evidence_passing_syntax.Expr.t Or_static_error.t
end
