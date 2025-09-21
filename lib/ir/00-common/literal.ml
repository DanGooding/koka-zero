open! Core
open! Import

type t =
  | Int of int
  | Bool of bool
  | Unit
[@@deriving equal, sexp_of]
