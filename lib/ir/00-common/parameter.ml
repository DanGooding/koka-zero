open! Core
open! Import

type t =
  | Variable of Variable.t
  | Wildcard
[@@deriving sexp_of]

let variable_opt = function
  | Variable v -> Some v
  | Wildcard -> None
;;

let bound_variables = function
  | Variable v -> Variable.Set.singleton v
  | Wildcard -> Variable.Set.empty
;;
