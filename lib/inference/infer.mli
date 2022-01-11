open Koka_zero_util

(** typecheck a program, converting to a form with necessary types/effects made
    explicit. And add a toplevel entry point to call main(). Returns a
    [Static_error] upon type errors *)
val infer_program
  :  Minimal_syntax.Program.t
  -> Explicit_syntax.Program.t Or_static_error.t

(** infer the type and effect of an expression, and convert it a explicit form,
    in the context containing the given declarations. Exists for testing
    purposes *)
val infer_expr_toplevel
  :  Minimal_syntax.Expr.t
  -> declarations:Minimal_syntax.Decl.t list
  -> (Type.Mono.t * Effect.t * Explicit_syntax.Expr.t) Or_static_error.t

(** type check and conver a program just as in [infer_program], but don't add an
    entry point (so allow programs without main) *)
val infer_program_without_main
  :  Minimal_syntax.Program.t
  -> Explicit_syntax.Program.t Or_static_error.t
