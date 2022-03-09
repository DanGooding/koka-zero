open Core
open Koka_zero_util

(** a value of type ['a] plus a flag indicating whether it has been changed *)
type 'a t

include Monad.S with type 'a t := 'a t
include Monad_utils.S with type 'a t := 'a t

(** inject a value marked as not-modified *)
val original : 'a -> 'a t

(** inject a value marked as modified*)
val modified : 'a -> 'a t

(* TODO: or return value in both branches? *)
val inspect : 'a t -> [ `Modified of 'a | `Unchanged ]
