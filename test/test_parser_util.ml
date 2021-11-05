open Core
open Koka_zero

let print_parse_result code =
  let ast = parse_string code in
  [%sexp (ast : (Syntax.program, string) Result.t)]
  |> Sexp.to_string_hum
  |> print_endline
;;
