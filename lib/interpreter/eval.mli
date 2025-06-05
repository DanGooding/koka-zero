open! Core
open! Import

val eval_program : Evidence_passing_syntax.Program.t -> Value.t Interpreter.t
