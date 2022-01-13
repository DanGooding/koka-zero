open Core

module T = struct
  type t =
    { type_subst : Type.Mono.t Type.Metavariable.Map.t
    ; effect_subst : Effect.t Effect.Metavariable.Map.t
    }
  [@@deriving sexp]
end (* disable "fragile-match" for generated code *) [@warning "-4"]

include T

let identity =
  let type_subst = Type.Metavariable.Map.empty in
  let effect_subst = Effect.Metavariable.Map.empty in
  { type_subst; effect_subst }
;;

let map_type_subst t ~f =
  let { type_subst; _ } = t in
  let type_subst = f type_subst in
  { t with type_subst }
;;

let map_effect_subst t ~f =
  let { effect_subst; _ } = t in
  let effect_subst = f effect_subst in
  { t with effect_subst }
;;

let map_type_subst_or_duplicate t ~f =
  let { type_subst; _ } = t in
  match f type_subst with
  | `Duplicate -> `Duplicate
  | `Ok type_subst -> `Ok { t with type_subst }
;;

let map_effect_subst_or_duplicate t ~f =
  let { effect_subst; _ } = t in
  match f effect_subst with
  | `Duplicate -> `Duplicate
  | `Ok effect_subst -> `Ok { t with effect_subst }
;;

let extend t ~var ~type_ =
  map_type_subst_or_duplicate t ~f:(Map.add ~key:var ~data:type_)
;;

let extend_effect t ~var ~effect =
  map_effect_subst_or_duplicate t ~f:(Map.add ~key:var ~data:effect)
;;

let extend_exn t ~var ~type_ =
  map_type_subst t ~f:(Map.add_exn ~key:var ~data:type_)
;;

let extend_effect_exn t ~var ~effect =
  map_effect_subst t ~f:(Map.add_exn ~key:var ~data:effect)
;;

(** merge two maps, as long as there are no common keys. Returns [ `Duplicate ]
    if there are *)
let merge_subst
    :  ('a, 'cmp, 'b) Map.t -> ('a, 'cmp, 'b) Map.t
    -> ('a, 'cmp, 'b) Map.t Map_intf.Or_duplicate.t
  =
 fun a b ->
  let duplicate_keys = Set.inter (Map.key_set a) (Map.key_set b) in
  if not (Set.is_empty duplicate_keys)
  then `Duplicate
  else
    `Ok
      (Map.merge a b ~f:(fun ~key:_ data ->
           match data with
           | `Left t | `Right t -> Some t
           (* this approach is slower than just catching the duplicate here, but
              it is easier than getting this failure value to outside the map *)
           | `Both (_, _) -> assert false))
;;

let merge_subst_exn subst vars =
  match merge_subst subst vars with
  | `Ok t -> t
  | `Duplicate ->
    raise_s
      [%message "Substitution.merge_subst_exn: found duplicate metavariable"]
;;

let extend_many t meta_to_type =
  map_type_subst_or_duplicate t ~f:(fun type_subst ->
      merge_subst type_subst meta_to_type)
;;

let extend_many_effect t meta_to_effect =
  map_effect_subst_or_duplicate t ~f:(fun effect_subst ->
      merge_subst effect_subst meta_to_effect)
;;

let extend_many_exn t meta_to_type =
  map_type_subst t ~f:(fun type_subst ->
      merge_subst_exn type_subst meta_to_type)
;;

let extend_many_effect_exn t meta_to_effect =
  map_effect_subst t ~f:(fun effect_subst ->
      merge_subst_exn effect_subst meta_to_effect)
;;

let rec lookup_effect t (var : Effect.Metavariable.t) =
  let { effect_subst; _ } = t in
  Map.find effect_subst var |> Option.map ~f:(apply_to_effect t)

and apply_to_effect t = function
  | Effect.Variable v -> Effect.Variable v
  | Effect.Metavariable v ->
    lookup_effect t v |> Option.value ~default:(Effect.Metavariable v)
  | Effect.Row r -> Effect.Row (apply_to_effect_row t r)

and apply_to_effect_row t row =
  match row with
  | Effect.Row.Closed _labels -> row
  | Effect.Row.Open (_labels, Effect.Row.Tail.Variable _v) -> row
  | Effect.Row.Open (labels, Effect.Row.Tail.Metavariable v) ->
    (match lookup_effect t v with
    | None -> row
    | Some tail_effect -> Effect.cons_row ~labels ~effect:tail_effect)
;;

let rec lookup t (var : Type.Metavariable.t) =
  (* here we repeatedly substitute, since [subst] is not normalised *)
  let { type_subst; _ } = t in
  Map.find type_subst var |> Option.map ~f:(apply_to_mono t)

and apply t = function
  | Type.Mono ty -> Type.Mono (apply_to_mono t ty)
  | Type.Poly p ->
    let { Type.Poly.forall_bound; forall_bound_effects; monotype } = p in
    let monotype = apply_to_mono t monotype in
    Type.Poly { Type.Poly.forall_bound; forall_bound_effects; monotype }

and apply_to_mono t = function
  | Type.Mono.Metavariable v ->
    lookup t v |> Option.value ~default:(Type.Mono.Metavariable v)
  | Type.Mono.Primitive p -> Type.Mono.Primitive (apply_to_primitive t p)
  | Type.Mono.Arrow (t_args, e, t_result) ->
    Type.Mono.Arrow
      ( List.map ~f:(apply_to_mono t) t_args
      , apply_to_effect t e
      , apply_to_mono t t_result )
  | Type.Mono.Variable v -> Type.Mono.Variable v

and apply_to_primitive _t = function
  | Type.Primitive.(Int | Bool | Unit) as ty -> ty
;;
