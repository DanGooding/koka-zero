(*
  Modified from the koka (v2.3.2) grammar specification
  https://github.com/koka-lang/koka/blob/v2.3.2/doc/spec/grammar/lexer.lex
  the license of which is reproduced below
*)
(* Copyright 2012-2021, Microsoft Research, Daan Leijen
   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0.
*)

{
open Parser

exception SyntaxError of string

(** Update the [lexbuf]'s position information,
    incrementing the line number *)
let next_line : Lexing.lexbuf -> unit =
  fun lexbuf ->
    let pos = lexbuf.lex_curr_p in
    let pos =
      { pos with
        pos_bol = pos.pos_cnum
        pos_lnum = pos.pos_lnum + 1
      }
    in
    lexbuf.lex_curr_p <- pos
;;

let bad_dash = Str.regexp "[^A-Za-z0-9]-[^A-Za-z]"

(** An identifier is well formed if dashes are unambiguously not
    subtraction signs. *)
let well_formed identifier =
  not (Str.string_match bad_dash identifier 0)

(** Return the matched identifier from the lexbuf,
    raising SyntaxError if it is not well formed *)
let lex_identifier lexbuf : string =
  let identifier = Lexing.lexeme lexbuf in
    if not (well_formed identifier)
    then raise SyntaxError (
        "malformed identifier: a dash must be preceded and followed by a letter")
    else identifier
;;

}

(* Character classes *)

let upper = ['A'-'Z']
let lower = ['a'-'z']
let letter = lower | upper
let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let space = [' ' '\t']
let newline = '\r'? '\n'

let sign = '-'?
let angle = ['<' '>']
let angle_bar = ['<' '>' '|']

let symbol_except_angle_bar =
  ['$' '%' '&' '*' '+' '@' '!' '\\' '^' '~' '=' '.' '-' ':' '?']
let symbol = symbol_except_angle_bar | angle_bar

(* In types, we can have sequences like "<<exn>|<div|e>>" where
   "<<", ">|<", and ">>" should not be parsed as operator tokens,
   but rather as single characters *)
let operator =
  '/' |
  "||" |
  symbol_except_angle_bar symbol* |
  angle_bar symbol_except_angle_bar symbol*

let final = '\''
let id_char = letter | digit | ['_' '-']
let id = lower id_char* final*
let con_id = upper id_char* final*

rule read =
  parse
  (* keywords*)

  (* | "infix"                     { INFIX } *)
  (* | "infixl"                    { INFIXL } *)
  (* | "infixr"                    { INFIXR } *)

  (* | "type"                      { TYPE } *)
  (* | "alias"                     { ALIAS } *)
  (* | "struct"                    { STRUCT } *)

  | "forall"                    { FORALL }

  | "fun"                       { FUN }
  | "fn"                        { FN }
  | "val"                       { VAL }
  | "var"                       { VAR }
  (* | "con"                       { CON } *)

  | "if"                        { IF }
  | "then"                      { THEN }
  | "else"                      { ELSE }
  | "elif"                      { ELIF }
  | "with"                      { WITH }
  | "in"                        { IN }
  (* | "match"                     { MATCH } *)
  | "return"                    { RETURN }

  (* | "as"                        { AS } *)

  | "control"                   { CONTROL }
  | "ctl"                       { CONTROL }
  (* | "rcontrol"                  { RCONTROL } *)
  (* | "rctl"                      { RCONTROL } *)
  | "except"                    { EXCEPT }
  | "handle"                    { HANDLE }
  | "handler"                   { HANDLER }
  | "effect"                    { EFFECT }

  | "mask"                      { MASK }
  (* | "override"                  { OVERRIDE } *)
  (* | "named"                     { NAMED } *)

  | "initially"                 { ID_INITIALLY }
  | "finally"                   { ID_FINALLY }

  (* kinds, not reserved, can be used as
     constructor names too *)
  | "E"                         { KIND_E }
  | "X"                         { KIND_X }
  | "V"                         { KIND_V }

  (* literals *)
  | "True"                      { BOOL true }
  | "False"                     { BOOl false }

  (* reserved operators *)
  | ':'                         { COLON }
  | '=' { EQUALS }
  | '.' { DOT }
  | "->" { RARROW }
  | "<-" { LARROW }

  (* non reserved, but have special meanning in certain contexts *)
  | ":=" { ASSIGN }
  | "::" { DCOLON }
  | '|' { PIPE }
  | '<' { LESS_THAN }
  | '>' { GREATER_THAN }
  | '!' { EXCLAMATION_MARK }
  | '~' { TILDE }

  (* special (non operator) symbols *)
  | '(' { OPEN_ROUND }
  | ')' { CLOSE_ROUND }
  | '{' { OPEN_CURLY }
  | '}' { CLOSE_CURLY }
  | '[' { OPEN_SQUARE }
  | ']' { CLOSE_SQUARE }
  | ';' { SEMI }
  | ',' { COMMA }
  | '<' { LESS_THAN }
  | '>' { GREATER_THAN }
  | '|' { PIPE }

  (* operators (fixed for now) *)
  | "||" { OP_OR }
  | "&&" { OP_AND }
  | "!=" { OP_NOT_EQUAL }
  | "==" { OP_EQUAL_EQUAL }
  | "<=" { OP_GREATER_EQUAL }
  | ">=" { OP_LESS_THAN }
  | "+"  { OP_PLUS }
  | "-"  { OP_MINUS }
  | "*"  { OP_TIMES }
  | "/"  { OP_DIVIDE }
  | "%"  { OP_MODULO }

  (* comments *)
  | "//" { read_single_line_comment lexbuf }
  | "/*" { read_multi_line_comment 1 lexbuf }

  (* Numbers *)
  | sign '0' ['x' 'X'] hex+ { int_of_string (Lexing.lexeme lexbuf) }
  | sign digit+             { int_of_string (Lexing.lexeme lexbuf) }

  (* Identifiers and operators *)
  | con_id           { CONID (lex_identifier lexbuf) }
  | id               { ID (lex_identifier lexbuf) }
  (* | '(' operator ')' { IDOP (Lexing.lexeme lexbuf) } *)
  (* | operator         { OP (Lexing.lexeme lexbuf) } *)
  | '_' id_char*     { WILDCARD }

  | space+ { read lexbuf }
  | newline { next_line lexbuf; read_lexbuf }

  | eof { EOF }
  | _ { raise SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf) }

and read_single_line_comment =
  parse
  (* TODO: ensure newlines handled the same in all modes *)
  | newline { next_line lexbuf; read lexbuf }
  | eof { EOF }
  (* TODO: match as much as possible for effciency? *)
  | _ { read_single_line_comment lexbuf }

(** Read a comment delimited by /* */.

    A [depth] of 0 means 'not in a comment' so the first call of
    this function should be [read_multi_line_comment 1 lexbuf] *)
and read_multi_line_comment depth =
  parse
  | "/*" { read_multi_line_comment (depth + 1) lexbuf }
  | "*/" { if depth > 1
             then read_multi_line_comment (depth - 1) lexbuf
             else read lexbuf
         }
  | newline { next_line lexbuf; read_multi_line_comment depth lexbuf }
  (* allow end of file to terminate a comment *)
  | eof { EOF }
  (* TODO: match as much as possible for effciency? *)
  | _ { read_multi_line_comment ~depth lexbuf }






