open! Core
open! Import

module Variable : sig
  (** a variable standing for an effect, either free, or quantified in a
      [Type.Poly.t] *)
  type t [@@deriving sexp_of]

  include Comparable.S_plain with type t := t

  module Name_source : sig
    include Name_source.S with type Name.t := t
  end
end

module Metavariable : sig
  (** a placeholder variable introduced during unification, an effect will be
      substituted for this *)
  type t [@@deriving sexp_of]

  include Comparable.S_plain with type t := t

  module Name_source : sig
    include Name_source.S with type Name.t := t
  end
end

module Label : sig
  (** the name of an individual effect *)
  type t

  include Identifiable.S with type t := t
  include Has_multiset.S with type t := t
end

module Row : sig
  module Tail : sig
    type t =
      | Variable of Variable.t
      | Metavariable of Metavariable.t
    [@@deriving sexp_of]

    val metavariables : t -> Metavariable.Set.t
    val instantiate_as : t -> var_to_meta:Metavariable.t Variable.Map.t -> t
  end

  (** An effect row - a multiset of labels, possibly with a variable in the tail
  *)
  type t =
    | Open of Label.Multiset.Non_empty.t * Tail.t
    | Closed of Label.Multiset.t
  [@@deriving sexp_of]

  val total : t
  val closed_singleton : Label.t -> t
  val extend : t -> Label.t -> t
  val is_open : t -> bool
  val metavariables : t -> Metavariable.Set.t
  val instantiate_as : t -> var_to_meta:Metavariable.t Variable.Map.t -> t

  (** an effect row now must _either_ be total, or have label(s) *)
  val is_total : t -> bool

  val labels : t -> Label.Multiset.t
end

type t =
  | Metavariable of Metavariable.t
  | Variable of Variable.t
  | Row of Row.t
[@@deriving sexp_of]

val metavariables : t -> Metavariable.Set.t
val instantiate_as : t -> var_to_meta:Metavariable.t Variable.Map.t -> t

(** a total effect row: [Row Row.total] *)
val total : t

(** returns whether an effect is definitely total [Some true], definitely not
    [Some false], or could be unified into either [None]. "Total" means having
    exactly zero effects - i.e. the empty closed row. Note that this assumes any
    metavariables are unknown. *)
val is_total : t -> bool option

(** build a row by adding [labels] to either an existing effect, or just a tail
*)
val cons_row : labels:Label.Multiset.Non_empty.t -> effect_:t -> Row.t

val of_row_tail : Row.Tail.t -> t

(** [row_subtract row ls] remove all labels from [row] which are present in
    [ls]. Labels in [ls] but not [row] are ignored (we have no 'lacks'
    constraints). This may remove all labels from an open row, so the result can
    be any effect, not necessarily a row *)
val row_subtract : Row.t -> Label.Multiset.t -> t
