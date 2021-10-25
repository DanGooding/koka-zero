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

(* TODO: [next_line] function? *)


let bad_dash = Str.regexp "[^A-Za-z0-9]-[^A-Za-z]"

(** An identifier is well formed if dashes are unambiguously not
    subtraction signs. *)
let well_formed identifier =
  not (Str.string_match bad_dash identifier 0)

(* TODO: which exception + add to docstring *)
(** Return the matched identifier from the lexer, rasing an exception
    if it is not well formed *)
let lex_identifier lexbuf =
  let identifier = Lexing.lexeme lexbuf in
    if not (well_formed identifier)
    then raise ?
    else identifier

}

(* Character classes *)

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
  angle_bar symbol_except_angle_bar symbol* |

let con_id = upper id_char* final*
let id = lower id_char* final*
let id_char = letter | digit | ['_' '-']

let hex_esc = 'x' hex hex | 'u' hex hex hex hex | 'U' hex hex hex hex hex hex
let char_esc = ['n' 'r' 't' '\\' '\"' '\'']

(* TODO: what are these for? *)
let line_char = graphic_line | utf8
let block_char = graphic_block | utf8

let letter = lower | upper
let upper = ['A'-'Z']
let lower = ['a'-'z']
let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let space = [' ' '\t']
let newline = '\r'? '\n'
let final = '\''

rule read =
  parse
  (* keywords*)
  | "infix"                     { INFIX }
  | "infixl"                    { INFIXL }
  | "infixr"                    { INFIXR }

  | "type"                      { TYPE }
  | "alias"                     { ALIAS }
  | "struct"                    { STRUCT }

  | "forall"                    { FORALL }
  | "exists"                    { EXISTS }
  | "some"                      { SOME }

  | "abstract"                  { ABSTRACT }
  | "extern"                    { EXTERN }

  | "fun"                       { FUN }
  | "fn"                        { FN }
  | "val"                       { VAL }
  | "var"                       { VAR }
  | "con"                       { CON }

  | "if"                        { IF }
  | "then"                      { THEN }
  | "else"                      { ELSE }
  | "elif"                      { ELIF }
  | "with"                      { WITH }
  | "in"                        { IN }
  | "match"                     { MATCH }
  | "return"                    { RETURN }

  | "module"                    { MODULE }
  | "import"                    { IMPORT }
  | "pub"                       { PUBLIC }
  | "public"                    { PUBLIC }
  | "private"                   { PRIVATE }
  | "as"                        { AS }

  | "control"                   { CONTROL }
  | "ctl"                       { CONTROL }
  | "rcontrol"                  { RCONTROL }
  | "rctl"                      { RCONTROL }
  | "except"                    { EXCEPT }
  | "handle"                    { HANDLE }
  | "handler"                   { HANDLER }
  | "effect"                    { EFFECT }

  | "rec"                       { IDREC }
  | "co"                        { IDCO }

  | "mask"                      { MASK }
  | "override"                  { OVERRIDE }
  | "named"                     { NAMED }

  | "inline"                    { IDINLINE  }
  | "noinline"                  { IDNOINLINE }

  | "open"                      { IDOPEN }
  | "extend"                    { IDEXTEND }
  | "linear"                    { IDLINEAR  }
  | "value"                     { IDVALUE  }
  | "reference"                 { IDREFERENCE  }
  | "scoped"                    { IDSCOPED }
  | "behind"                    { IDBEHIND }

  | "initially"                 { IDINITIALLY }
  | "finally"                   { IDFINALLY }

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

  (* comments *)
  | "//" { read_single_line_comment lexbuf }
  | "/*" { read_multi_line_comment lexbuf }

  (* Numbers *)
  | sign '0' ['x' 'X'] hex+ { int_of_string (Lexing.lexeme lexbuf) }
  | sign digit+             { int_of_string (Lexing.lexeme lexbuf) }

  (* Identifiers and operators *)
  | con_id           { CONID (lex_identifier lexbuf) }
  | id               { ID (lex_identifier lexbuf) }
  | '(' operator ')' { IDOP (Lexing.lexeme lexbuf) }
  | operator         { OP (Lexing.lexeme lexbuf) }
  | '_' id_char*     { WILDCARD (lex_identifier lexbuf) }

  | space+ { read lexbuf }
  | newline { next_line lexbuf; read_lexbuf }

  | eof { EOF }
  | _ { raise SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf) }

and read_single_line_comment =
  parse
  (* TODO: ensure newlines handled the same in all modes *)
  | newline { next_line lexbuf; read lexbuf }
  (* TODO: match as much as possible for effciency? *)
  | _ { read_single_line_comment lexbuf }

and read_multi_line_comment ?(depth = 1) =
  parse
  | "(*" { read_multi_line_comment ~depth:(depth + 1) lexbuf }
  | "*)" { if depth > 1
             then read_multi_line_comment ~depth:(depth - 1) lexbuf
             else read lexbuf
         }
  | newline { next_line lexbuf; read_multi_line_comment ~depth lexbuf }
  (* TODO: compare this and [read_single_line_comment] with spec implementations *)
  (* TODO: match as much as possible for effciency? *)
  | _ { read_multi_line_comment ~depth lexbuf }
  (* TODO: EOF token (don't spin forever) *)







