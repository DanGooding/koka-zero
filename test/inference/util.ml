open Core
open Koka_zero_inference
open Koka_zero_util

let print_inference_result expr =
  let type_ = infer_type expr in
  [%sexp (type_ : (Type.Poly.t, Static_error.t) Result.t)]
  |> Sexp.to_string_hum
  |> print_endline
;;
