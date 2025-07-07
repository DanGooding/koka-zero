open Core

(* TODO: [Variable] currently have no annotations, but will need to use a
   variant to namespace user names from generated names *)

module Variable : sig
  (** a variable standing for a type, either free, or quantified in a [Poly.t]*)
  type t [@@deriving sexp_of]

  include Comparable.S_plain with type t := t

  module Name_source : sig
    include Name_source.S with type Name.t := t
  end
end

module Metavariable : sig
  (** a placeholder variable introduced during unification, a type will be
      substituted for this *)
  type t [@@deriving sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  module Name_source : sig
    include Name_source.S with type Name.t := t
  end
end

module Primitive : sig
  type t =
    | Int
    | Bool
    | Unit
  [@@deriving equal, compare, sexp_of, hash]

  val metavariables : t -> Metavariable.Set.t * Effect.Metavariable.Set.t
end

(** a monotype contains no forall quantifiers *)
module Mono : sig
  type t =
    | Arrow of t list * Effect.t * t
    | Variable of Variable.t
    | Metavariable of Metavariable.t
    | Primitive of Primitive.t
  [@@deriving sexp_of, compare, hash]

  val metavariables : t -> Metavariable.Set.t * Effect.Metavariable.Set.t

  (** [instantiate_as t var_to_meta] replaces variables with metavariables as
      described by the map [var_to_meta] *)
  val instantiate_as
    :  t
    -> var_to_meta:Metavariable.t Variable.Map.t
    -> effect_var_to_meta:Effect.Metavariable.t Effect.Variable.Map.t
    -> t
end

(** a polytype has a toplevel forall quantifier *)
module Poly : sig
  type t =
    { forall_bound : Metavariable.t -> bool
    ; forall_bound_effects : Effect.Metavariable.t -> bool
    ; monotype : Mono.t
    }
  [@@deriving sexp_of]
end

type t =
  | Mono of Mono.t
  | Poly of Poly.t
[@@deriving sexp_of]

val generalise
  :  Mono.t
  -> should_generalise_type_metavariable:(Metavariable.t -> bool)
  -> should_generalise_effect_metavariable:(Effect.Metavariable.t -> bool)
  -> Poly.t
