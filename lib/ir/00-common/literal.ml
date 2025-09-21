open! Core
open! Import

type t =
  | Int of int
  | Bool of bool
[@@deriving equal, sexp_of]
