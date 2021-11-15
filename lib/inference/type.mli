open Core

module Primitive : sig
  type t =
    | Int
    | Bool
    | Unit
  [@@deriving sexp]
end

module Variable = Identifiable

type t =
  | Arrow of t * t
  | Variable of Variable.t
  | Primitive of Primitive.t
[@@deriving]

(* TODO: probably deserves its own module *)
module Scheme : sig
  type type_ := t

  type t =
    { (* TODO: make these private *)
      forall_bound : Variable.Set.t
    ; monotype : type_
    }

  val generalise : type_ -> t
  val instantiate : t -> name_source:(unit -> Variable.t) -> type_
end
