open! Core
open! Import

type t =
  | List_nil
  | List_cons
  | Tuple
[@@deriving sexp_of]
