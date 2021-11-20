open Core
open Koka_zero_util

module State = struct
  type t =
    { substitution : Substitution.t
          (** 'values' of metavariables, these are replaced 'on-demand' in
              [unify] rather than ever propagating over a whole type *)
    ; variable_source : Type.Variable.Name_source.t
    ; metavariable_source : Type.Metavariable.Name_source.t
    }
  [@@deriving sexp]

  let initial : t =
    { substitution = Substitution.identity
    ; variable_source = Type.Variable.Name_source.fresh ~prefix:"$a" ()
    ; metavariable_source = Type.Metavariable.Name_source.fresh ~prefix:"$m" ()
    }
  ;;
end

module T = struct
  (** a state monad to store the global substitution and the name source, plus
      an exception monad for type errors *)
  type 'a t = State.t -> ('a * State.t, Static_error.t) Result.t

  let bind m ~f s =
    let%bind.Result x, s' = m s in
    f x s'
  ;;

  let return x s = Result.Ok (x, s)

  let map =
    let map m ~f s =
      let%map.Result x, s' = m s in
      f x, s'
    in
    `Custom map
  ;;
end

include T
include Monad.Make (T)

let fresh_variable s =
  let { State.variable_source; _ } = s in
  let name, variable_source =
    Type.Variable.Name_source.next_name variable_source
  in
  let s = State.{ s with variable_source } in
  Result.Ok (name, s)
;;

let fresh_metavariable s =
  let { State.metavariable_source; _ } = s in
  let name, metavariable_source =
    Type.Metavariable.Name_source.next_name metavariable_source
  in
  let s = State.{ s with metavariable_source } in
  Result.Ok (name, s)
;;

let lookup_meta a s =
  let { State.substitution; _ } = s in
  let t = Substitution.find substitution a in
  (t, s) |> Result.Ok
;;

(** gives read access to the global substitution *)
let get_substitution s =
  let { State.substitution; _ } = s in
  Result.Ok (substitution, s)
;;

(** apply a pure function to the global substitution *)
let map_substitution (f : Substitution.t -> Substitution.t) s =
  let { State.substitution; _ } = s in
  let substitution = f substitution in
  let s = { s with State.substitution } in
  Result.Ok ((), s)
;;

let substitute_meta_exn ~var ~type_ =
  map_substitution (fun substitution ->
      match Substitution.extend substitution ~var ~type_ with
      | `Duplicate -> raise_s [%message "metavariable already has a type"]
      | `Ok substitution -> substitution)
;;

let type_error message _s = Static_error.type_error message |> Result.Error

let unification_error_of to_sexp t1 t2 =
  let s1 = to_sexp t1 |> Sexp.to_string_hum in
  let s2 = to_sexp t2 |> Sexp.to_string_hum in
  let message = sprintf "cannot unify\n%s\nwith\n%s\n" s1 s2 in
  type_error message
;;

(* TODO: use pretty printing instead *)
let unification_error_primitive = unification_error_of Type.Primitive.sexp_of_t
let unification_error_mono = unification_error_of Type.Mono.sexp_of_t
let unification_error = unification_error_of Type.sexp_of_t

let rec occurs (v : Type.Metavariable.t) ~in_:(t : Type.Mono.t) =
  match t with
  | Type.Mono.Arrow (t_arg, t_result) ->
    occurs v ~in_:t_arg || occurs v ~in_:t_result
  | Type.Mono.Variable _ -> false
  | Type.Mono.Metavariable v' -> Type.Metavariable.(v = v')
  | Type.Mono.Primitive p ->
    (match p with
    | Type.Primitive.(Int | Bool | Unit) -> false)
;;

let unify_primitives p1 p2 =
  let open Type.Primitive in
  match p1, p2 with
  | Int, Int | Bool, Bool | Unit, Unit -> return ()
  | (Int | Bool | Unit), _ -> unification_error_primitive p1 p2
;;

let rec unify t1 t2 =
  let open Let_syntax in
  match t1, t2 with
  | Type.Mono.Metavariable a, _ -> unify_with_meta a t2
  | _, Type.Mono.Metavariable b -> unify_with_meta b t1
  | Type.Mono.Arrow (t1_arg, t1_result), Type.Mono.Arrow (t2_arg, t2_result) ->
    let%bind () = unify t1_arg t2_arg in
    unify t1_result t2_result
  | Type.Mono.Primitive p1, Type.Mono.Primitive p2 -> unify_primitives p1 p2
  | Type.Mono.Variable a, Type.Mono.Variable b ->
    if Type.Variable.(a = b) then return () else unification_error_mono t1 t2
  | (Type.Mono.Arrow (_, _) | Type.Mono.Variable _ | Type.Mono.Primitive _), _
    -> unification_error_mono t1 t2

and unify_with_meta (a : Type.Metavariable.t) t2 : unit t =
  let open Let_syntax in
  match%bind lookup_meta a with
  | Some ta -> unify ta t2
  | None ->
    (* [a] has not been substitued for yet... *)
    (match t2 with
    | Type.Mono.Metavariable b ->
      if Type.Metavariable.(a = b)
      then return ()
      else (
        match%bind lookup_meta b with
        (* [b] has been substituted for, unify with that *)
        | Some tb -> unify_with_meta a tb
        | None -> substitute_meta_exn ~var:a ~type_:(Type.Mono.Metavariable b))
    | Type.Mono.(Arrow (_, _) | Variable _ | Primitive _) ->
      if occurs a ~in_:t2
      then unification_error_mono (Type.Mono.Metavariable a) t2
      else substitute_meta_exn ~var:a ~type_:t2)
;;

let instantiate (poly : Type.Poly.t) : Type.Mono.t t =
  let open Let_syntax in
  let { Type.Poly.forall_bound; monotype } = poly in
  let%map (var_to_meta : Type.Metavariable.t Type.Variable.Map.t) =
    (* generate a fresh metavariale for each variable. Messy since the monadic
       calls must be sequenced *)
    Set.fold
      forall_bound
      ~init:(return Type.Variable.Map.empty)
      ~f:(fun acc v ->
        let%bind acc = acc in
        let%map m = fresh_metavariable in
        Map.add_exn acc ~key:v ~data:m)
  in
  Type.Mono.instantiate_as monotype var_to_meta
;;

(* TODO: should this be in infer instead (doesn't need internal access)? *)
let generalise (t : Type.Mono.t) ~in_:(env : Context.t) : Type.Poly.t t =
  let open Let_syntax in
  let%bind substitution = get_substitution in
  let t = Substitution.apply_to_mono substitution t in
  let t_meta = Type.Mono.metavariables t in
  let env = Context.apply_substitution env substitution in
  let env_meta = Context.metavariables env in
  let meta = Set.diff t_meta env_meta in
  (* replace each of these metavariables with a fresh variable, and universally
     quanitfy over all those *)
  let%bind (meta_to_var : Type.Variable.t Type.Metavariable.Map.t) =
    Set.fold meta ~init:(return Type.Metavariable.Map.empty) ~f:(fun acc m ->
        let%bind acc = acc in
        let%map v = fresh_variable in
        Map.add_exn acc ~key:m ~data:v)
  in
  let meta_to_mono = Map.map meta_to_var ~f:(fun v -> Type.Mono.Variable v) in
  let%bind () =
    (* TODO: is applying this globally the right thing to do?

       it is at least inefficient, we are about to replace the only references
       to these metavariables. However, when backsubstituting, need to be
       careful *)
    map_substitution (fun substitution ->
        Substitution.extend_many_exn substitution meta_to_mono)
  in
  let%map substitution = get_substitution in
  let t = Substitution.apply_to_mono substitution t in
  let forall_bound = Map.data meta_to_var |> Type.Variable.Set.of_list in
  { Type.Poly.forall_bound; monotype = t }
;;

let run (f : 'a t) =
  let%map.Result x, s = f State.initial in
  let State.{ substitution; _ } = s in
  x, substitution
;;
