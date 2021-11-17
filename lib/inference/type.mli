open Core

module Primitive : sig
  type t =
    | Int
    | Bool
    | Unit
  [@@deriving sexp]
end

(* TODO: currently have no annotations, but will need to use a variant to
   namespace user names from generated names *)

(** a variable standing for a type, either free, or quantified in a [Poly.t]*)
module Variable : sig
  type t [@@deriving sexp]

  include Identifiable.S with type t := t
  include Name_source.S with type t := t
end

(** a placeholder variable introduced during unification, a type will be
    substituted for this *)
module Metavariable : sig
  type t [@@deriving sexp]

  include Identifiable.S with type t := t
  include Name_source.S with type t := t
end
(* TODO: have a version of [Mono.t] without metavariables (for the output of
   inference) *)

(** a monotype contains no forall quantifiers*)
module Mono : sig
  type t =
    | Arrow of t * t
    | Variable of Variable.t
    | Metavariable of Metavariable.t
    | Primitive of Primitive.t
  [@@deriving sexp]
end

(** a polytype has a toplevel forall quantifier *)
module Poly : sig
  type t =
    { forall_bound : Variable.Set.t
    ; monotype : Mono.t
    }
  [@@deriving sexp]
end

type t =
  | Mono of Mono.t
  | Poly of Poly.t
[@@deriving sexp]
