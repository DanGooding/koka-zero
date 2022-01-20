open Core
open Import

module Suffix_source = struct
  type t = int [@@deriving sexp]

  let fresh = 0

  let next x =
    let s = string_of_int x in
    s, x + 1
  ;;
end

type t = string [@@deriving sexp]

let to_string t = t
let main = "main"

let of_toplevel = function
  | Variable.User s -> "kku_" ^ s
  | Variable.Language s -> "kkl_" ^ s
  | Variable.Generated s -> "kkg_" ^ s
;;

let of_runtime_exn s =
  if String.is_prefix ~prefix:"kkr_" s
  then s
  else raise_s [%message "symbol in the runtime missing prefix:" (s : string)]
;;

let string_of_variable = function
  | Variable.User s | Variable.Language s | Variable.Generated s -> s
;;

let of_local_aux ~containing (local_name : string) source =
  let suffix, source' = Suffix_source.next source in
  let symbol = sprintf "%s__%s__%s" containing local_name suffix in
  symbol, source'
;;

let of_local ~containing v source =
  let local_name = string_of_variable v in
  of_local_aux ~containing local_name source
;;

let of_local_anonymous ~containing source =
  of_local_aux ~containing "local" source
;;
