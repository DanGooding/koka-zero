open! Core
open Import
open Evidence_passing_syntax

val rewrite
  :  Expr.t
  -> toplevel:Variable.Set.t
  -> (Expr.t * Program.Fun_decl.t list) Generation.t

val rewrite_program : Program.t -> Program.t Generation.t
