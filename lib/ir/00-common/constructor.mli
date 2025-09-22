open! Core
open! Import

type t =
  | List_nil
  | List_cons
[@@deriving sexp_of]
