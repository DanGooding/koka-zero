open Koka_zero_util

(** typecheck a program, returning unit if well typed *)
val check_program : Minimal_syntax.Program.t -> unit Or_static_error.t

(** infer the type and effect of an expression, in the context containing the
    given declarations. exists for testing purposes *)
val infer_expr_toplevel
  :  Minimal_syntax.Expr.t
  -> declarations:Minimal_syntax.Decl.t list
  -> (Type.Mono.t * Effect.t) Or_static_error.t

(** infer the type of a declaration and add it to the context. Exposed for
    testing purposes. *)
val infer_decl
  :  Minimal_syntax.Decl.t
  -> env:Context.t
  -> effect_env:Effect_signature.Context.t
  -> (Context.t * Effect_signature.Context.t) Inference.t
