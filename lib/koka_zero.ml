open Core
open Lexing
module LexerUtil = MenhirLib.LexerUtil
module Syntax = Syntax
module Static_error = Static_error

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

let set_filename ?filename lexbuf =
  match filename with
  | Some filename -> LexerUtil.init filename lexbuf
  | None -> lexbuf
;;

let parse_with_filename ?filename lexbuf =
  let lexbuf = set_filename ?filename lexbuf in
  parse lexbuf
;;

let parse_channel ?filename ch =
  parse_with_filename ?filename (Lexing.from_channel ch)
;;

let parse_string ?filename s =
  parse_with_filename ?filename (Lexing.from_string s)
;;
