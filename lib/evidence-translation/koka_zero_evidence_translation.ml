module Evidence_passing_syntax = Evidence_passing_syntax

let translate program = Generation.run (Translation.translate program)

module Free_variables = Free_variables

module Private = struct
  let translate_expr expr = Generation.run (Translation.translate_expr expr)

  let translate_no_prelude program =
    Generation.run (Translation.translate_no_prelude program)
  ;;
end
