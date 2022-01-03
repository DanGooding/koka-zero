open Core
open Lexing
open Koka_zero_util
module LexerUtil = MenhirLib.LexerUtil

let set_filename ?filename lexbuf =
  match filename with
  | Some filename -> LexerUtil.init filename lexbuf
  | None -> lexbuf
;;

let parse lexbuf =
  try Parser.program Lexer.read lexbuf |> Result.Ok with
  | Lexer.Syntax_error message ->
    let location = Source_location.t_of_lexing_position lexbuf.lex_curr_p in
    let error = Static_error.syntax_error ~at:location message in
    Result.Error error
  | Parser.Error ->
    let location = Source_location.t_of_lexing_position lexbuf.lex_curr_p in
    let error = Static_error.syntax_error ~at:location "parse error" in
    Result.Error error
;;

module Private = struct
  module Syntax = Syntax

  let parse_to_syntax ?filename lexbuf =
    let lexbuf = set_filename ?filename lexbuf in
    parse lexbuf
  ;;

  let parse_channel_to_syntax ?filename ch =
    parse_to_syntax ?filename (Lexing.from_channel ch)
  ;;

  let parse_string_to_syntax ?filename s =
    parse_to_syntax ?filename (Lexing.from_string s)
  ;;

  let simplify = Simplification.simplify_program
end

let parse_channel ?filename ch =
  let%bind.Result program = Private.parse_channel_to_syntax ?filename ch in
  Private.simplify program
;;

let parse_string ?filename s =
  let%bind.Result program = Private.parse_string_to_syntax ?filename s in
  Private.simplify program
;;
