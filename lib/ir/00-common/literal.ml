open! Core
open! Import

type t =
  | Int of int
  | Bool of bool
  | Unit
[@@deriving sexp_of]
