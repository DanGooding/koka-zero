open Core
open Koka_zero_inference
open Koka_zero_util

let print_inference_result program =
  let type_and_effect = infer_type program in
  [%sexp (type_and_effect : (Type.Mono.t * Effect.t, Static_error.t) Result.t)]
  |> Sexp.to_string_hum
  |> print_endline
;;

let print_expr_inference_result expr =
  let program =
    { Minimal_syntax.Program.effect_declarations = []; body = expr }
  in
  print_inference_result program
;;
