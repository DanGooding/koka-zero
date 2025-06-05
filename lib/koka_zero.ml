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

let parse_channel = Koka_zero_parsing.parse_channel
let parse_string = Koka_zero_parsing.parse_string
let infer_program = Koka_zero_inference.infer_program
let translate = Koka_zero_evidence_translation.translate
let rewrite_program = Koka_zero_evidence_rewriting.rewrite_program
let interpret_program = Koka_zero_interpreter.interpret_program
let compile_program = Koka_zero_code_generation.compile_then_write_program
