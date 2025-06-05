open! Core
open! Import

(** typecheck a program, converting to a form with necessary types/effects made
    explicit, and adding an entry_point function to call the user's `main()`.
    Returns a [Static_error] upon type errors *)
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

  (** typecheck a program, converting to a form with necessary types/effects
      made explicit, but don't add an entry point (allowing the program to not
      define `main()`). Returns a [Static_error] upon type errors. Exists for
      testing purposes. *)
  val infer_program_without_main
    :  Minimal_syntax.Program.t
    -> Explicit_syntax.Program.t Or_static_error.t
end
