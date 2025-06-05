open! Core
open! Import
module Evidence_passing_syntax = Evidence_passing_syntax

let translate program = Generation.run (Translation.translate program)

let rewrite_program program =
  Generation.run (Rewriting.rewrite_program program) ~name_prefix:"opt_"
;;

module Free_variables = Free_variables

module Private = struct
  let translate_expr expr ~evv =
    Generation.run (Translation.translate_expr expr ~evv)
  ;;

  let translate_lambda expr = Generation.run (Translation.translate_lambda expr)

  let translate_no_prelude program =
    Generation.run (Translation.translate_no_prelude program)
  ;;

  module Rewriting = struct
    let apply_bind_inlining expr ~toplevel =
      Generation.run (Bind_inlining.rewrite expr ~toplevel) ~name_prefix:"opt_"
    ;;
  end
end
