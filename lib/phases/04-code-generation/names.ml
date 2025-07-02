open! Core
open! Import

let register_name_of_variable = function
  | Variable.User s -> "u_" ^ s
  | Variable.Language s -> "l_" ^ s
  (* generated names tend to already have numerical suffixes - use underscore to
     separate *)
  | Variable.Generated s -> "g_" ^ s ^ "_"
;;
