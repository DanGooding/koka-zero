open Core
open Koka_zero_util

(* TODO: make sure actual syntax errors aren't reported as unsupported syntax *)

let expr_to_minimal_syntax = function
  | Syntax.Return _ -> Static_error.unsupported_syntax "return" |> Result.Error
  | _ -> failwith "not implemented"
;;

let program_to_minimal_syntax _program = failwith "not implemented"
