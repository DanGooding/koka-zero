module Type = Type
module Effect = Effect
module Minimal_syntax = Minimal_syntax
module Explicit_syntax = Explicit_syntax

let check_program = Infer.check_program

module Private = struct
  let infer_expr_toplevel = Infer.infer_expr_toplevel
end
