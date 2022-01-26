open Core
open Koka_zero_util
module Static_error = Static_error

(* TODO: expose syntax abstractly? *)
module Minimal_syntax = Koka_zero_inference.Minimal_syntax
module Explicit_syntax = Koka_zero_inference.Explicit_syntax

module Evidence_passing_syntax =
  Koka_zero_evidence_translation.Evidence_passing_syntax

module Value = Koka_zero_interpreter.Value
module Runtime_error = Koka_zero_interpreter.Runtime_error
open Runtime_error
module Codegen_error = Koka_zero_code_generation.Codegen_error
open Codegen_error

val parse_channel
  :  ?filename:string
  -> In_channel.t
  -> Minimal_syntax.Program.t Or_static_error.t

val parse_string
  :  ?filename:string
  -> string
  -> Minimal_syntax.Program.t Or_static_error.t

val infer_program
  :  Minimal_syntax.Program.t
  -> Explicit_syntax.Program.t Or_static_error.t

val translate : Explicit_syntax.Program.t -> Evidence_passing_syntax.Program.t

val interpret_program
  :  Evidence_passing_syntax.Program.t
  -> Value.t Or_runtime_error.t

val compile_program
  :  ?module_name:string
  -> filename:string
  -> Evidence_passing_syntax.Program.t
  -> unit Or_codegen_error.t
