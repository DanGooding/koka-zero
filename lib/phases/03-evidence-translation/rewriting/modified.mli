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
val value : 'a t -> 'a

(** repeatedly apply [f] until it returns an unchanged value. (the result will
    marked as modified when [f] was run more than once) *)
val apply_while_changes : 'a -> f:('a -> 'a t) -> 'a t

(** convert a function, replacing a [None] with the original argument, and
    [Some x] with x (marked as modified) *)
val original_for_none : ('a -> 'a option) -> 'a -> 'a t
