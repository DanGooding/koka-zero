open! Core
open! Import

(** encodes a set of constraints of type [lower_bound <= _] and [_ <= upper_bound] **)
type 'a t = private
  { lower_bounds : 'a Hash_set.t
  ; upper_bounds : 'a Hash_set.t
  ; key : 'a Hashtbl_intf.Hashtbl.Key.t
  }
[@@deriving sexp_of]

val create : 'a Hashtbl_intf.Hashtbl.Key.t -> 'a t

val freshen
  :  'a t
  -> lower:[ `Empty | `Map of 'a -> 'a ]
  -> upper:[ `Empty | `Map of 'a -> 'a ]
  -> 'a t
