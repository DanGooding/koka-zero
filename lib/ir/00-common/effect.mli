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
  include Hashable.S_plain with type t := t

  module Name_source : sig
    include Name_source.S with type Name.t := t
  end
end

module Label : sig
  (** the name of an individual effect *)
  type t [@@deriving sexp_of, hash, compare]

  include Stringable.S with type t := t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Unknown : sig
  type t =
    | Metavariable of Metavariable.t
    | Variable of Variable.t
  [@@deriving sexp_of, compare, hash]
end

type t =
  | Unknown of Unknown.t
  | Labels of Label.Set.t
  | Handled of Label.Set.t * Unknown.t
[@@deriving sexp_of, compare, hash]

val metavariables : t -> Metavariable.Set.t
val instantiate_as : t -> var_to_meta:Metavariable.t Variable.Map.t -> t
val max_level : t -> metavariable_level:(Metavariable.t -> int) -> int
