open! Core
open! Import

(** apply optimising transformations *)
val rewrite_program
  :  Evidence_passing_syntax.Program.t
  -> Evidence_passing_syntax.Program.t Or_static_error.t

module Private : sig
  val apply_bind_inlining
    :  Evidence_passing_syntax.Expr.t
    -> toplevel:Variable.Set.t
    -> (Evidence_passing_syntax.Expr.t
       * Evidence_passing_syntax.Program.Fun_decl.t list)
         Or_static_error.t
end
