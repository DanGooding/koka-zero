open Core

type t = Type.Mono.t Type.Metavariable.Map.t [@@deriving sexp]

let identity = Type.Metavariable.Map.empty
let extend t ~var ~type_ = Map.add t ~key:var ~data:type_
let extend_exn t ~var ~type_ = Map.add_exn t ~key:var ~data:type_

let extend_many t vars =
  let duplicate_keys = Set.inter (Map.key_set t) (Map.key_set vars) in
  if not (Set.is_empty duplicate_keys)
  then `Duplicate
  else
    `Ok
      (Map.merge t vars ~f:(fun ~key:_ data ->
           match data with
           | `Left t | `Right t -> Some t
           (* this approach is slower than just catching the duplicate here, but
              it is easier than getting this failure value to outside the map *)
           | `Both (_, _) -> assert false))
;;

let extend_many_exn t vars =
  match extend_many t vars with
  | `Ok t -> t
  | `Duplicate ->
    raise_s
      [%message "Substitution.extend_many_exn: found duplicate metavariable"]
;;

let lookup_effect subst (var : Effect.Metavariable.t) =
  failwith "not implemented"
;;

let apply_to_effect subst effect = failwith "not implemented"
let apply_to_effect_row subst row = failwith "not implemented"

let rec lookup subst (var : Type.Metavariable.t) =
  (* here we repeatedly substitute, since [subst] is not normalised *)
  Map.find subst var |> Option.map ~f:(apply_to_mono subst)

and apply subst = function
  | Type.Mono t -> Type.Mono (apply_to_mono subst t)
  | Type.Poly p ->
    let { Type.Poly.forall_bound; forall_bound_effects; monotype } = p in
    let monotype = apply_to_mono subst monotype in
    Type.Poly { Type.Poly.forall_bound; forall_bound_effects; monotype }

and apply_to_mono subst = function
  | Type.Mono.Metavariable v ->
    (match lookup subst v with
    | Some t' -> t'
    | None -> Type.Mono.Metavariable v)
  | Type.Mono.Primitive p -> Type.Mono.Primitive (apply_to_primitive subst p)
  | Type.Mono.Arrow (t_arg, e, t_result) ->
    Type.Mono.Arrow
      ( apply_to_mono subst t_arg
      , apply_to_effect subst e
      , apply_to_mono subst t_result )
  | Type.Mono.Variable v -> Type.Mono.Variable v

and apply_to_primitive _subst = function
  | Type.Primitive.(Int | Bool | Unit) as t -> t
;;
