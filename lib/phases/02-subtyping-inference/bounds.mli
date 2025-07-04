open! Core
open! Import

(** encodes a set of constraints of type [lowerBound <= _] and [_ <= upperBound] **)
type 'a t =
  { (* TODO: should these be hash_sets? *)
    mutable lowerBounds : 'a list
  ; mutable upperBounds : 'a list
  }
[@@deriving sexp_of]

val create : unit -> 'a t
