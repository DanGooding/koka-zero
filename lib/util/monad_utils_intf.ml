open Core

module type S = sig
  type 'a t

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
