open! Core
open! Import

let translate program = Generation.run (Translation.translate program)

module Private = struct
  let translate_expr expr ~evv =
    Generation.run (Translation.translate_expr expr ~evv)
  ;;

  let translate_lambda expr = Generation.run (Translation.translate_lambda expr)

  let translate_no_prelude program =
    Generation.run (Translation.translate_no_prelude program)
  ;;
end
