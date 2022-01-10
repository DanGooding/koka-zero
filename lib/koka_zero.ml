module Static_error = Koka_zero_util.Static_error
module Minimal_syntax = Koka_zero_inference.Minimal_syntax
module Explicit_syntax = Koka_zero_inference.Explicit_syntax

module Evidence_passing_syntax =
  Koka_zero_evidence_translation.Evidence_passing_syntax

module Value = Koka_zero_interpreter.Value
module Runtime_error = Koka_zero_interpreter.Runtime_error

let parse_channel = Koka_zero_parsing.parse_channel
let parse_string = Koka_zero_parsing.parse_string
let infer_program = Koka_zero_inference.infer_program
let translate = Koka_zero_evidence_translation.translate
let interpret_program = Koka_zero_interpreter.interpret_program
