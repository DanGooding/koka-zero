include struct
  open Koka_zero_evidence_translation
  module EPS = Evidence_passing_syntax
  module Primitives = Primitives
end

include struct
  open Koka_zero_inference
  module Variable = Variable
end

include struct
  open Koka_zero_util
  module Or_static_error = Or_static_error
end
