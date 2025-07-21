open Core

type t =
  { filename : string option
  ; line : int (** 1-indexed line number *)
  ; char : int (** 1-indexed position within line *)
  }
[@@deriving sexp]

let t_of_lexing_position (pos : Lexing.position) =
  let open Lexing in
  let char = pos.pos_cnum - pos.pos_bol + 1 in
  let line = pos.pos_lnum in
  let filename =
    if String.is_empty pos.pos_fname then None else Some pos.pos_fname
  in
  { filename; line; char }
;;

let to_string t =
  let { filename; line; char } = t in
  let filename_part =
    match filename with
    | Some filename -> filename ^ ":"
    | None -> ""
  in
  let line_char_part = sprintf "%d:%d" line char in
  filename_part ^ line_char_part
;;
