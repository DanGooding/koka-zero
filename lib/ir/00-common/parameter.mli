open! Core
open! Import

(** a binding of value to name, or ignoring with a wildcard *)
type t =
  | Variable of Variable.t
  | Wildcard
[@@deriving sexp_of]

val variable_opt : t -> Variable.t option
