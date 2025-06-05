open! Core
open! Import

let print_parse_to_syntax_result code =
  let syntax = Koka_zero_parsing.Private.parse_string_to_syntax code in
  [%sexp (syntax : (Syntax.program, Static_error.t) Result.t)]
  |> Sexp.to_string_hum
  |> print_endline;
  syntax
;;

let print_simplification_result syntax =
  match syntax with
  | Result.Error _ -> ()
  | Result.Ok syntax ->
    let minimal_syntax = Koka_zero_parsing.Private.simplify syntax in
    [%sexp
      (minimal_syntax : (Minimal_syntax.Program.t, Static_error.t) Result.t)]
    |> Sexp.to_string_hum
    |> print_endline
;;
