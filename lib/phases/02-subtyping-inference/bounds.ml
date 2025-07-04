open! Core
open! Import

type 'a t =
  { mutable lowerBounds : 'a list
  ; mutable upperBounds : 'a list
  }
[@@deriving sexp_of]

let create () = { lowerBounds = []; upperBounds = [] }
