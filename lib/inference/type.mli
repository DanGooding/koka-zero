open Core

module Primitive : sig
  type t =
    | Int
    | Bool
    | Unit
  [@@deriving sexp]
end

module Variable : Identifiable.S

module Mono : sig
  type t =
    | Arrow of t * t
    | Variable of Variable.t
    | Primitive of Primitive.t
  [@@deriving sexp]
end

(* TODO: probably deserves its own module *)
module Poly : sig
  type t =
    { (* TODO: make these private *)
      forall_bound : Variable.Set.t
    ; monotype : Mono.t
    }
  [@@deriving sexp]

  val generalise : in_:Context.t -> Mono.t -> t
  val instantiate : t -> name_source:(unit -> Variable.t) -> Mono.t
end

type t =
  | Monotype of Mono.t
  | Polytype of Poly.t
[@@deriving sexp]
