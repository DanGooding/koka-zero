open Core

module Variable : sig
  (** a variable standing for an effect, either free, or quantified in a
      [Type.Poly.t]*)
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
    type label := t
    type t [@@deriving sexp]

    val of_list : label list -> t
    val empty : t
    val add : t -> label -> t
    val union : t -> t -> t

    (** intersection: if an element appears `m` times in one multiset and `n`
        times in the other, then it appears `min m n` in their intersection *)
    val inter : t -> t -> t

    (** difference: if an element appears `m` times in the first argument, and
        `n` times in the second, then it appears `max (m - n) 0` times in the
        result *)
    val diff : t -> t -> t

    val is_empty : t -> bool
  end
end

module Row : sig
  module Tail : sig
    type t =
      | Variable of Variable.t
      | Metavariable of Metavariable.t
    [@@deriving sexp]
  end

  (** An effect row - a multiset of labels, possibly with a variable in the tail *)
  type t =
    { labels : Label.Multiset.t
    ; tail : Tail.t option
    }
  [@@deriving sexp]

  val empty : t
  val extend : t -> Label.t -> t
  val is_open : t -> bool

  (* TODO: start without open/close *)
  (* val open : t -> varaible_source -> t *)

  val metavariables : t -> Metavariable.Set.t
  val instantiate_as : t -> var_to_meta:Metavariable.t Variable.Map.t -> t
end

type t =
  | Metavariable of Metavariable.t
  | Variable of Variable.t
  | Row of Row.t
[@@deriving sexp]

val metavariables : t -> Metavariable.Set.t
val instantiate_as : t -> var_to_meta:Metavariable.t Variable.Map.t -> t
