open! Core
open! Import

module Effect = struct
  type t =
    | Variable of Effect.Variable.t
    | Labels of Effect.Label.Set.t
    | Handled of Effect.Label.Set.t * t
    | Union of t list
    | Intersection of t list
    | Recursive of Effect.Variable.t * t
  [@@deriving sexp_of]

  let rec variables t =
    match t with
    | Variable v -> Effect.Variable.Set.singleton v
    | Labels _labels -> Effect.Variable.Set.empty
    | Handled (_lables, effect_) -> variables effect_
    | Union effects | Intersection effects ->
      Effect.Variable.Set.union_list (List.map effects ~f:variables)
    | Recursive (v, effect_) -> Set.remove (variables effect_) v
  ;;
end

type t =
  | Variable of Type.Variable.t
  | Primitive of Type.Primitive.t
  | Arrow of t list * Effect.t * t
  | Union of t list
  | Intersection of t list
  | Recursive of Type.Variable.t * t
[@@deriving sexp_of]

let rec variables t =
  match t with
  | Variable v -> Type.Variable.Set.singleton v
  | Primitive _ -> Type.Variable.Set.empty
  | Arrow (args, _effect, result) ->
    Type.Variable.Set.union_list (List.map (result :: args) ~f:variables)
  | Union types | Intersection types ->
    Type.Variable.Set.union_list (List.map types ~f:variables)
  | Recursive (v, type_) -> Set.remove (variables type_) v
;;
