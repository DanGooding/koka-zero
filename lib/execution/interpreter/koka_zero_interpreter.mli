open! Core
open! Import
module Runtime_error = Runtime_error
module Value = Value

(** interpret a program *)
val interpret_program
  :  Evidence_passing_syntax.Program.t
  -> Value.t Or_runtime_error.t
