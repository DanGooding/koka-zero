include struct
  open Koka_zero_util
  module Or_static_error = Or_static_error
end

include struct
  open Koka_zero_ir_common
  module Variable = Variable
end

include struct
  open Koka_zero_ir_evidence_passing_syntax
  module EPS = Evidence_passing_syntax
  module Primitive_names = Primitive_names
end
