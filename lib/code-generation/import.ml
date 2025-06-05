include Koka_zero_util
module Or_codegen_error = Codegen_error.Or_codegen_error

include struct
  open Koka_zero_ir_common
  module Variable = Variable
  module Operator = Operator
  module Literal = Literal
  module Parameter = Parameter
  module Effect_label = Effect.Label
end

module Evidence_passing_syntax =
  Koka_zero_ir_evidence_passing_syntax.Evidence_passing_syntax
