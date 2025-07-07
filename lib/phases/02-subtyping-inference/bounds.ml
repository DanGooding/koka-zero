open! Core
open! Import

type 'a t =
  { mutable lower_bounds : 'a list
  ; mutable upper_bounds : 'a list
  }
[@@deriving sexp_of]

let create () = { lower_bounds = []; upper_bounds = [] }
