include Koka_zero_util

include struct
  open Koka_zero_ir_common
  module Constructor = Constructor
  module Effect = Effect
  module Effect_decl = Effect_decl
  module Keyword = Keyword
  module Literal = Literal
  module Name_source = Name_source
  module Operation_shape = Operation_shape
  module Operator = Operator
  module Parameter = Parameter
  module Pattern = Pattern
  module Polar_type = Polar_type
  module Type = Type
  module Variable = Variable
end

module Minimal_syntax = Koka_zero_ir_minimal_syntax.Minimal_syntax
module Explicit_syntax = Koka_zero_ir_explicit_syntax.Explicit_syntax
