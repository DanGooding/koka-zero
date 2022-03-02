module Static_error = Koka_zero_util.Static_error
module Minimal_syntax = Koka_zero_inference.Minimal_syntax
module Explicit_syntax = Koka_zero_inference.Explicit_syntax

module Evidence_passing_syntax =
  Koka_zero_evidence_translation.Evidence_passing_syntax

module Value = Koka_zero_interpreter.Value
module Runtime_error = Koka_zero_interpreter.Runtime_error
module Codegen_error = Koka_zero_code_generation.Codegen_error
module Util = Koka_zero_util

let[@landmark] parse_channel = Koka_zero_parsing.parse_channel
let[@landmark] parse_string = Koka_zero_parsing.parse_string
let[@landmark] infer_program = Koka_zero_inference.infer_program
let[@landmark] translate = Koka_zero_evidence_translation.translate
let[@landmark] interpret_program = Koka_zero_interpreter.interpret_program

let[@landmark] compile_program =
  Koka_zero_code_generation.compile_then_write_program
;;
