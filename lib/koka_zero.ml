open Core
open Lexing
module LexerUtil = MenhirLib.LexerUtil
module Syntax = Syntax

let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  let char_num = pos.pos_cnum - pos.pos_bol + 1 in
  sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum char_num
;;

let parse lexbuf =
  try Parser.program Lexer.read lexbuf |> Result.Ok with
  | Lexer.Syntax_error message ->
    let pos = string_of_position lexbuf in
    let message = sprintf "%s: %s" pos message in
    Result.Error message
  | Parser.Error ->
    let pos = string_of_position lexbuf in
    let message = sprintf "%s: syntax error" pos in
    Result.Error message
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
