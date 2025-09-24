open! Core
open! Import

type t =
  | List_nil
  | List_cons
  | Option_none
  | Option_some
[@@deriving sexp_of]
