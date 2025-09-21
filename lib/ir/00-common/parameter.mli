open! Core
open! Import

(** a binding of value to name, or ignoring with a wildcard *)
type t =
  | Variable of Variable.t
  | Wildcard
  | Tuple of t list
[@@deriving sexp_of]

val bound_variables : t -> Variable.Set.t
