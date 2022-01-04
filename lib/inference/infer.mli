open Koka_zero_util

(** typecheck a program, returning unit if well typed *)
val check_program : Minimal_syntax.Program.t -> unit Or_static_error.t

(** infer the type and effect of an expression, in the context containing the
    given declarations. exists for testing purposes *)
val infer_expr_toplevel
  :  Minimal_syntax.Expr.t
  -> declarations:Minimal_syntax.Decl.t list
  -> (Type.Mono.t * Effect.t) Or_static_error.t
