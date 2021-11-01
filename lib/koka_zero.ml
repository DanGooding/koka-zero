open Core
open Lexing

let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  let char_num = pos.pos_cnum - pos.pos_bol + 1 in
  sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum char_num
;;

let parse lexbuf =
  try Parser.program Lexer.read lexbuf |> Or_error.return with
  | Lexer.Syntax_error message ->
    let pos = string_of_position lexbuf in
    let message = sprintf "%s: %s" pos message in
    Or_error.error_string message
  | Parser.Error ->
    let pos = string_of_position lexbuf in
    let message = sprintf "%s: syntax error" pos in
    Or_error.error_string message
;;

let parse_channel ch = parse (Lexing.from_channel ch)
let parse_string s = parse (Lexing.from_string s)
