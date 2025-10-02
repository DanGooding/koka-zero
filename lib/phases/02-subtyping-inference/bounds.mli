open! Core
open! Import

(** encodes a set of constraints of type [lower_bound <= _] and [_ <= upper_bound] **)
type 'a t [@@deriving sexp_of]

val add : 'a t -> [ `lower | `upper ] -> 'a -> unit
val iter : 'a t -> [ `lower | `upper ] -> f:('a -> unit) -> unit
val fold : 'a t -> [ `lower | `upper ] -> init:'b -> f:('b -> 'a -> 'b) -> 'b

val result_iter
  :  'a t
  -> [ `lower | `upper ]
  -> f:('a -> (unit, 'e) Result.t)
  -> (unit, 'e) Result.t

val to_list : 'a t -> [ `lower | `upper ] -> 'a list
val create : 'a Hashtbl_intf.Hashtbl.Key.t -> 'a t

val freshen
  :  'a t
  -> lower:[ `Empty | `Map of 'a -> 'a ]
  -> upper:[ `Empty | `Map of 'a -> 'a ]
  -> 'a t
