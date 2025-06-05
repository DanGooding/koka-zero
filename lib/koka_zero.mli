open! Core
module Static_error = Koka_zero_util.Static_error
module Minimal_syntax = Koka_zero_ir_minimal_syntax.Minimal_syntax
module Explicit_syntax = Koka_zero_ir_explicit_syntax.Explicit_syntax

module Evidence_passing_syntax =
  Koka_zero_ir_evidence_passing_syntax.Evidence_passing_syntax

module Value = Koka_zero_interpreter.Value
module Runtime_error = Koka_zero_interpreter.Runtime_error
module Codegen_error = Koka_zero_code_generation.Codegen_error
module Util = Koka_zero_util

val parse_channel
  :  ?filename:string
  -> In_channel.t
  -> Minimal_syntax.Program.t Static_error.Or_static_error.t

val parse_string
  :  ?filename:string
  -> string
  -> Minimal_syntax.Program.t Static_error.Or_static_error.t

val infer_program
  :  Minimal_syntax.Program.t
  -> Explicit_syntax.Program.t Static_error.Or_static_error.t

val translate
  :  Explicit_syntax.Program.t
  -> Evidence_passing_syntax.Program.t Static_error.Or_static_error.t

val rewrite_program
  :  Evidence_passing_syntax.Program.t
  -> Evidence_passing_syntax.Program.t Static_error.Or_static_error.t

val interpret_program
  :  Evidence_passing_syntax.Program.t
  -> Value.t Runtime_error.Or_runtime_error.t

val compile_program
  :  ?module_name:string
  -> filename:string
  -> Evidence_passing_syntax.Program.t
  -> unit Codegen_error.Or_codegen_error.t
