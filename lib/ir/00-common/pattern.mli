open! Core
open! Import

module Scrutinee : sig
  type t =
    | Primitive of Type.Primitive.t
    | List
    | Tuple
  [@@deriving equal, sexp_of]
end

type t =
  | Parameter of Parameter.t
  | Literal of Literal.t
  | Construction of Constructor.t * Parameter.t list
[@@deriving sexp_of]

val scrutinee : t -> Scrutinee.t option
val bound_variables : t -> Variable.Set.t
