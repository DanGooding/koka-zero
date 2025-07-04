include Koka_zero_util

include struct
  open Koka_zero_ir_common
  module Effect = Effect
  module Keyword = Keyword
  module Literal = Literal
  module Operator = Operator
  module Parameter = Parameter
  module Type = Type
  module Variable = Variable
end

module Minimal_syntax = Koka_zero_ir_minimal_syntax.Minimal_syntax
module Explicit_syntax = Koka_zero_ir_explicit_syntax.Explicit_syntax
