open! Core
open! Import

type t

val create : unit -> t

val infer_expr_toplevel
  :  t
  -> Minimal_syntax.Expr.t
  -> declarations:Minimal_syntax.Decl.t list
  -> (Explicit_syntax.Expr.t * Import.Type.Mono.t * Import.Effect.t) Or_error.t

val infer_program
  :  t
  -> Minimal_syntax.Program.t
  -> Explicit_syntax.Program.t Or_error.t

val infer_program_without_main
  :  t
  -> Minimal_syntax.Program.t
  -> Explicit_syntax.Program.t Or_error.t
