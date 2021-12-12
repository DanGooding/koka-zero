open Core
open Koka_zero_util

module Variable : sig
  (** a variable standing for an effect, either free, or quantified in a
      [Type.Poly.t] *)
  type t

  include Identifiable.S with type t := t
  include Name_source.S with type t := t
end

module Metavariable : sig
  (** a placeholder variable introduced during unification, an effect will be
      substituted for this *)
  type t

  include Identifiable.S with type t := t
  include Name_source.S with type t := t
end

module Label : sig
  (** the name of an individual effect *)
  type t

  include Identifiable.S with type t := t

  module Multiset : sig
    include Multiset.S with type Element.t := t
  end
end

module Row : sig
  module Tail : sig
    type t =
      | Variable of Variable.t
      | Metavariable of Metavariable.t
    [@@deriving sexp]

    val metavariables : t -> Metavariable.Set.t
    val instantiate_as : t -> var_to_meta:Metavariable.t Variable.Map.t -> t
  end

  (** An effect row - a multiset of labels, possibly with a variable in the tail *)
  type t =
    { labels : Label.Multiset.t
    ; tail : Tail.t option
    }
  [@@deriving sexp]

  val total : t
  val closed_singleton : Label.t -> t
  val extend : t -> Label.t -> t
  val is_open : t -> bool

  (* TODO: start without open/close *)
  (* val open : t -> varaible_source -> t *)

  val metavariables : t -> Metavariable.Set.t
  val instantiate_as : t -> var_to_meta:Metavariable.t Variable.Map.t -> t
  val is_total : t -> bool option
end

type t =
  | Metavariable of Metavariable.t
  | Variable of Variable.t
  | Row of Row.t
[@@deriving sexp]

val metavariables : t -> Metavariable.Set.t
val instantiate_as : t -> var_to_meta:Metavariable.t Variable.Map.t -> t

(** a total effect row: [Row Row.total] *)
val total : t

(** returns whether an effect is definitely total [Some true], definitely not
    [Some false], or could be unified into either [None]. "Total" means having
    exactly zero effects - i.e. the empty closed row. Note that this assumes any
    metavariables are unknown. *)
val is_total : t -> bool option
