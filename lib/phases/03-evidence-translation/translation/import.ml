include Koka_zero_util
include Koka_zero_evidence_passing_common

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
  module Polar_type = Polar_type
end

module Explicit_syntax = Koka_zero_ir_explicit_syntax.Explicit_syntax

include struct
  open Koka_zero_ir_evidence_passing_syntax
  module Evidence_passing_syntax = Evidence_passing_syntax
  module Primitive_names = Primitive_names
  module Free_variables = Free_variables
end
