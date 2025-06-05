include Koka_zero_util

include struct
  open Koka_zero_ir_common
  module Effect = Effect
  module Effect_decl = Effect_decl
  module Literal = Literal
  module Operation_shape = Operation_shape
  module Variable = Variable
  module Keyword = Keyword
  module Operator = Operator
  module Parameter = Parameter
end

include struct
  open Koka_zero_ir_evidence_passing_syntax
  module Evidence_passing_syntax = Evidence_passing_syntax
  module Primitive_names = Primitive_names
  module Free_variables = Free_variables
end
