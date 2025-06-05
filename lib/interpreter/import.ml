module Or_runtime_error = Runtime_error.Or_runtime_error

include struct
  open Koka_zero_ir_common
  module Effect_label = Effect.Label
  module Literal = Literal
  module Operator = Operator
  module Parameter = Parameter
  module Variable = Variable
end

module Evidence_passing_syntax =
  Koka_zero_ir_evidence_passing_syntax.Evidence_passing_syntax
