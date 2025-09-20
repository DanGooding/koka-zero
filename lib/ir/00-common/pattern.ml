open! Core
open! Import

type t =
  | Parameter of Parameter.t
  | Literal of Literal.t
  | Construction of Constructor.t * t list
[@@deriving sexp_of]
