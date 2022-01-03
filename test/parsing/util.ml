open Core
open Koka_zero_parsing
open Koka_zero_util

let print_parse_to_syntax_result code =
  let syntax = Private.parse_string_to_syntax code in
  [%sexp (syntax : (Private.Syntax.program, Static_error.t) Result.t)]
  |> Sexp.to_string_hum
  |> print_endline;
  syntax
;;

let print_simplification_result syntax =
  match syntax with
  | Result.Error _ -> ()
  | Result.Ok syntax ->
    let minimal_syntax = Private.simplify syntax in
    [%sexp
      (minimal_syntax
        : ( Koka_zero_inference.Minimal_syntax.Program.t
          , Static_error.t )
          Result.t)]
    |> Sexp.to_string_hum
    |> print_endline
;;
