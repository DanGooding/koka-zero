open Koka_zero_inference

(** apply the evidence-passing translation to a program *)
val translate : Explicit_syntax.Program.t -> Evidence_passing_syntax.Program.t

module Private : sig
  (** apply the evidence passing translation on an expression. Note generated
      names will not be unique across invocations *)
  val translate_expr : Explicit_syntax.Expr.t -> Evidence_passing_syntax.Expr.t
end
