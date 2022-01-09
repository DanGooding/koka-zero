module Evidence_passing_syntax = Evidence_passing_syntax

let translate program = Generation.run (Translation.translate program)

module Private = struct
  let translate_expr expr = Generation.run (Translation.translate_expr expr)
end
