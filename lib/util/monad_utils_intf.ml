open Core

module type S = sig
  type 'a t

  (** a version of [List.fold] that runs in this monad, sequencing calls to [f]
      in the usual front-to-back order. *)
  val list_fold
    :  'a list
    -> init:'accum
    -> f:('accum -> 'a -> 'accum t)
    -> 'accum t

  (** a version of [List.fold_right] which runs in this monad *)
  val list_fold_right
    :  'a list
    -> init:'accum
    -> f:('a -> 'accum -> 'accum t)
    -> 'accum t

  (** a version of [List.concat_map] which runs in this monad *)
  val list_concat_map : 'a list -> f:('a -> 'b list t) -> 'b list t

  val list_iter : 'a list -> f:('a -> unit t) -> unit t
  val list_map : 'a list -> f:('a -> 'b t) -> 'b list t

  val list_iter2
    :  'a list
    -> 'b list
    -> f:('a -> 'b -> unit t)
    -> unit List.Or_unequal_lengths.t t

  val list_find_map : 'a list -> f:('a -> 'b option t) -> 'b option t

  val map_mapi
    :  ('k, 'a, 'cmp) Map.t
    -> f:(key:'k -> data:'a -> 'b t)
    -> ('k, 'b, 'cmp) Map.t t

  (** a version of [Map.fold] which runs in this monad *)
  val map_fold
    :  ('k, 'v, 'cmp) Map.t
    -> init:'accum
    -> f:(key:'k -> data:'v -> 'accum -> 'accum t)
    -> 'accum t

  (** a version of [Map.fold_right] which runs in this monad *)
  val map_fold_right
    :  ('k, 'v, 'cmp) Map.t
    -> init:'accum
    -> f:(key:'k -> data:'v -> 'accum -> 'accum t)
    -> 'accum t

  (** [all_map] is the equivalent of [all] which operates on map data rather
      than list items. It will sequence computation(according to the order of
      [Map.fold])s given as a map's data into one computation, which returns a
      map of their pure results *)
  val all_map : ('a, 'b t, 'cmp) Map.t -> ('a, 'b, 'cmp) Map.t t

  (** [all_map_unit] sequences computations as [all_map] but them throws away
      all the unit results and just returns a single unit *)
  val all_map_unit : ('a, unit t, 'cmp) Map.t -> unit t

  (** [all_option] is the equivalent of [all] which operations on option values
      rather than list items. It interchang the [option] and [t] type
      constructors, preserving the 'structure' and values of the monadic value
      if it is present. *)
  val all_option : 'a t option -> 'a option t

  (** [all_non_empty] is the equivalent of [all] but over non-empty lists*)
  val all_non_empty : 'a t Non_empty_list.t -> 'a Non_empty_list.t t
end
