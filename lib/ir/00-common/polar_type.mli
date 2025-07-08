open! Core
open! Import

module Effect : sig
  type t =
    | Variable of Effect.Variable.t
    | Labels of Effect.Label.Set.t
    | Handled of Effect.Label.Set.t * t
    | Union of t list
    | Intersection of t list
    | Recursive of Effect.Variable.t * t
  [@@deriving sexp_of]

  val variables : t -> Effect.Variable.Set.t
  val simplify : t -> t
  val is_total : t -> bool
end

type t =
  | Variable of Type.Variable.t
  | Primitive of Type.Primitive.t
  | Arrow of t list * Effect.t * t
  | Union of t list
  | Intersection of t list
  | Recursive of Type.Variable.t * t
[@@deriving sexp_of]

val variables : t -> Type.Variable.Set.t
val simplify : t -> t
