open! Core
open! Import
module Evidence_passing_syntax = Evidence_passing_syntax

(** apply the evidence-passing translation to a program *)
val translate
  :  Explicit_syntax.Program.t
  -> Evidence_passing_syntax.Program.t Or_static_error.t

(** apply optimising transformations *)
val rewrite_program
  :  Evidence_passing_syntax.Program.t
  -> Evidence_passing_syntax.Program.t Or_static_error.t

module Free_variables = Free_variables

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

  module Rewriting : sig
    val apply_bind_inlining
      :  Evidence_passing_syntax.Expr.t
      -> toplevel:Variable.Set.t
      -> (Evidence_passing_syntax.Expr.t
         * Evidence_passing_syntax.Program.Fun_decl.t list)
           Or_static_error.t
  end
end
