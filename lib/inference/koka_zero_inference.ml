open Core
module Type = Type
module Effect = Effect
module Minimal_syntax = Minimal_syntax

let check_program = Infer.check_program

module Private = struct
  let infer_expr_toplevel = Infer.infer_expr_toplevel

  module Context = Context
  module Effect_signature = Effect_signature

  let infer_decl decl =
    let%map.Result (env, effect_env), _substitiution =
      Inference.run
        (Infer.infer_decl
           decl
           ~env:Context.empty
           ~effect_env:Effect_signature.Context.empty)
    in
    env, effect_env
  ;;
end
