open! Core
open! Import

type t =
  | Variable of Variable.t
  | Wildcard
  | Tuple of t list
[@@deriving sexp_of]

let rec bound_variables = function
  | Variable v -> Variable.Set.singleton v
  | Wildcard -> Variable.Set.empty
  | Tuple ts -> List.map ts ~f:bound_variables |> Variable.Set.union_list
;;
