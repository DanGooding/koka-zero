open! Core
open! Import

(** encodes a set of constraints of type [lower_bound <= _] and [_ <= upper_bound] **)
type 'a t =
  { (* TODO: should these be hash_sets? *)
    mutable lower_bounds : 'a list
  ; mutable upper_bounds : 'a list
  }
[@@deriving sexp_of]

val create : unit -> 'a t
