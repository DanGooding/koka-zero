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

  let rec simplify t =
    match t with
    | Intersection [ t ] | Union [ t ] -> simplify t
    | Intersection ts -> Intersection (List.map ts ~f:simplify)
    | Union ts -> Union (List.map ts ~f:simplify)
    | Variable v -> Variable v
    | Labels labels -> Labels labels
    | Handled (handled_labels, t) ->
      (match (simplify t : t) with
       | Labels labels ->
         let labels = Set.diff labels handled_labels in
         Labels labels
       | Handled (more_handled_labels, t) ->
         let handled_labels = Set.union handled_labels more_handled_labels in
         Handled (handled_labels, t)
       | Intersection _ | Union _ | Variable _ | Recursive _ ->
         Handled (handled_labels, t))
    | Recursive (v, t) -> Recursive (v, simplify t)
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

(* TODO: this could be much more sophisticated,
   notably there are many cases where we can remove variables *)
let rec simplify t =
  match t with
  | Intersection [ t ] | Union [ t ] -> simplify t
  | Intersection ts -> Intersection (List.map ts ~f:simplify)
  | Union ts -> Union (List.map ts ~f:simplify)
  | Variable _ | Primitive _ -> t
  | Arrow (args, effect_, result) ->
    Arrow (List.map args ~f:simplify, Effect.simplify effect_, simplify result)
  | Recursive (v, t) -> Recursive (v, simplify t)
;;
