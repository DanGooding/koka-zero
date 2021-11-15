open Core
open Koka_zero_parsing
open Koka_zero_util

let print_parse_result code =
  let ast = parse_string code in
  [%sexp (ast : (Syntax.program, Static_error.t) Result.t)]
  |> Sexp.to_string_hum
  |> print_endline
;;
