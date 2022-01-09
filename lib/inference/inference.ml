open Core
open Koka_zero_util

module State = struct
  type t =
    { substitution : Substitution.t
          (** 'values' of metavariables, these are replaced 'on-demand' in
              [unify] rather than ever propagating over a whole type *)
    ; variable_source : Type.Variable.Name_source.t
    ; metavariable_source : Type.Metavariable.Name_source.t
    ; effect_variable_source : Effect.Variable.Name_source.t
    ; effect_metavariable_source : Effect.Metavariable.Name_source.t
    }
  [@@deriving sexp]

  let initial : t =
    { substitution = Substitution.identity
    ; variable_source = Type.Variable.Name_source.fresh ~prefix:"$a" ()
    ; metavariable_source = Type.Metavariable.Name_source.fresh ~prefix:"a" ()
    ; effect_variable_source = Effect.Variable.Name_source.fresh ~prefix:"$e" ()
    ; effect_metavariable_source =
        Effect.Metavariable.Name_source.fresh ~prefix:"e" ()
    }
  ;;
end

module T = struct
  (** a state monad to store the global substitution and the name source, plus
      an exception monad for type errors *)
  type 'a t = State.t -> ('a * State.t) Or_static_error.t

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

module T' = struct
  include T
  include Monad.Make (T)
end

include T'
include Monad_utils.Make (T')

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

let fresh_effect_variable s =
  let { State.effect_variable_source; _ } = s in
  let name, effect_variable_source =
    Effect.Variable.Name_source.next_name effect_variable_source
  in
  let s = State.{ s with effect_variable_source } in
  Result.Ok (name, s)
;;

let fresh_effect_metavariable s =
  let { State.effect_metavariable_source; _ } = s in
  let name, effect_metavariable_source =
    Effect.Metavariable.Name_source.next_name effect_metavariable_source
  in
  let s = State.{ s with effect_metavariable_source } in
  Result.Ok (name, s)
;;

let lookup_meta a s =
  let { State.substitution; _ } = s in
  let t = Substitution.lookup substitution a in
  Result.Ok (t, s)
;;

let lookup_effect_meta a s =
  let { State.substitution; _ } = s in
  let e = Substitution.lookup_effect substitution a in
  Result.Ok (e, s)
;;

(** pass the current global substitution as an argument to a pure function *)
let use_substitution : (Substitution.t -> 'a) -> 'a t =
 fun f s ->
  let { State.substitution; _ } = s in
  Result.Ok (f substitution, s)
;;

(** modify the global substitution using a pure function *)
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

let substitute_effect_meta_exn ~var ~effect =
  map_substitution (fun substitution ->
      match Substitution.extend_effect substitution ~var ~effect with
      | `Duplicate ->
        raise_s [%message "effect metavariable already has a type"]
      | `Ok substitution -> substitution)
;;

let with_any_effect t =
  let open Let_syntax in
  let%map v = fresh_effect_metavariable in
  t, Effect.Metavariable v
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
let unification_error_effect = unification_error_of Effect.sexp_of_t
let unification_error_effect_row = unification_error_of Effect.Row.sexp_of_t

let raise_for_unexpected_variable a =
  (* currently, variables are only bound by foralls in polytypes, but
     unification of those is not currently supported. (Indeed the easiest way to
     do that correctly involves instantiating them)

     so variables should never occur in types to be unified *)
  raise_s [%message "variable encountered in unification" (a : Type.Variable.t)]
;;

let raise_for_unexpected_effect_variable a =
  raise_s
    [%message
      "effect variable encountered in unification" (a : Effect.Variable.t)]
;;

let occurs_effect (a : Effect.Metavariable.t) ~in_:(e : Effect.t) =
  Set.mem (Effect.metavariables e) a
;;

let rec unify_effects e1 e2 =
  match e1, e2 with
  | Effect.Variable a, _ | _, Effect.Variable a ->
    raise_for_unexpected_effect_variable a
  | Effect.Metavariable a, _ -> unify_effect_with_meta a e2
  | _, Effect.Metavariable b -> unify_effect_with_meta b e1
  | Effect.Row r1, Effect.Row r2 -> unify_effect_rows r1 r2

and unify_effect_rows r1 r2 =
  let open Let_syntax in
  let%bind r1 =
    use_substitution (fun s -> Substitution.apply_to_effect_row s r1)
  in
  let%bind r2 =
    use_substitution (fun s -> Substitution.apply_to_effect_row s r2)
  in
  let { Effect.Row.labels = labels1; tail = tail1 } = r1 in
  let { Effect.Row.labels = labels2; tail = tail2 } = r2 in
  let common_labels = Effect.Label.Multiset.inter labels1 labels2 in
  let labels1 = Effect.Label.Multiset.diff labels1 common_labels in
  let labels2 = Effect.Label.Multiset.diff labels2 common_labels in
  (* construct the effects for error output and further unification, but pattern
     match as tuples for readability *)
  let r1 = { Effect.Row.labels = labels1; tail = tail1 } in
  let r2 = { Effect.Row.labels = labels2; tail = tail2 } in
  (* all univerally quantified variables should be instantiated before
     unification *)
  let get_tail_metavariable tail =
    Option.map tail ~f:(function
        | Effect.Row.Tail.Metavariable a -> a
        | Effect.Row.Tail.Variable a -> raise_for_unexpected_effect_variable a)
  in
  let tail_meta1 = get_tail_metavariable tail1 in
  let tail_meta2 = get_tail_metavariable tail2 in
  match
    ( (Effect.Label.Multiset.is_empty labels1, tail_meta1)
    , (Effect.Label.Multiset.is_empty labels2, tail_meta2) )
  with
  (* meta ~ _ *)
  (* substitution was previously applied, so [a] is guaranteed to be unknown,
     therefore directly substituting here is safe However we use
     [unify_with_meta] since we do still need the occurs check *)
  | (true, Some a), _ -> unify_effect_with_meta a (Effect.Row r2)
  | _, (true, Some b) -> unify_effect_with_meta b (Effect.Row r1)
  (* <> ~ <> *)
  | (true, None), (true, None) -> return ()
  (* <ls> !~ _ (as [labels1] & [labels2] are already disjoint) *)
  | (_, None), _ | _, (_, None) -> unification_error_effect_row r1 r2
  (* <ls|meta> ~ <ls'|meta'> *)
  | (false, Some a1), (false, Some a2) ->
    let%bind b = fresh_effect_metavariable in
    let tail = Some (Effect.Row.Tail.Metavariable b) in
    let r1' = { Effect.Row.labels = labels2; tail } in
    let r2' = { Effect.Row.labels = labels1; tail } in
    (* their unification is <labels1+labels2|b> *)
    let%bind () = unify_effect_with_meta a1 (Effect.Row r1') in
    unify_effect_with_meta a2 (Effect.Row r2')

and unify_effect_with_meta (a : Effect.Metavariable.t) (e2 : Effect.t) : unit t =
  let open Let_syntax in
  match%bind lookup_effect_meta a with
  | Some ea -> unify_effects ea e2
  | None ->
    (* [a] has not been substitued for yet... *)
    (match e2 with
    | Effect.Metavariable b ->
      if Effect.Metavariable.(a = b)
      then return ()
      else (
        match%bind lookup_effect_meta b with
        (* [b] has been substituted for, unify with that *)
        | Some tb -> unify_effect_with_meta a tb
        | None ->
          substitute_effect_meta_exn ~var:a ~effect:(Effect.Metavariable b))
    | Effect.Row _ | Effect.Variable _ ->
      if occurs_effect a ~in_:e2
      then unification_error_effect (Effect.Metavariable a) e2
      else substitute_effect_meta_exn ~var:a ~effect:e2)
;;

let occurs (v : Type.Metavariable.t) ~in_:(t : Type.Mono.t) =
  let meta, _effect_meta = Type.Mono.metavariables t in
  Set.mem meta v
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
  | Type.Mono.Variable a, _ | _, Type.Mono.Variable a ->
    raise_for_unexpected_variable a
  | Type.Mono.Metavariable a, _ -> unify_with_meta a t2
  | _, Type.Mono.Metavariable b -> unify_with_meta b t1
  | ( Type.Mono.Arrow (t1_args, eff1, t1_result)
    , Type.Mono.Arrow (t2_args, eff2, t2_result) ) ->
    (match List.zip t1_args t2_args with
    | List.Or_unequal_lengths.Unequal_lengths ->
      unification_error_of [%sexp_of: Type.Mono.t list] t1_args t2_args
    | List.Or_unequal_lengths.Ok zipped_args ->
      let%bind () =
        List.map zipped_args ~f:(fun (t1_arg, t2_arg) -> unify t1_arg t2_arg)
        |> all_unit
      in
      let%bind () = unify_effects eff1 eff2 in
      unify t1_result t2_result)
  | Type.Mono.Primitive p1, Type.Mono.Primitive p2 -> unify_primitives p1 p2
  | Type.Mono.(Arrow (_, _, _) | Primitive _), _ -> unification_error_mono t1 t2

and unify_with_meta (a : Type.Metavariable.t) (t2 : Type.Mono.t) : unit t =
  let open Let_syntax in
  match%bind lookup_meta a with
  | Some ta -> unify ta t2
  | None ->
    (* [a] has not been substitued for yet... *)
    (match t2 with
    | Type.Mono.Variable a -> raise_for_unexpected_variable a
    | Type.Mono.Metavariable b ->
      if Type.Metavariable.(a = b)
      then return ()
      else (
        match%bind lookup_meta b with
        (* [b] has been substituted for, unify with that *)
        | Some tb -> unify_with_meta a tb
        | None -> substitute_meta_exn ~var:a ~type_:(Type.Mono.Metavariable b))
    | Type.Mono.(Arrow (_, _, _) | Primitive _) ->
      if occurs a ~in_:t2
      then unification_error_mono (Type.Mono.Metavariable a) t2
      else substitute_meta_exn ~var:a ~type_:t2)
;;

let instantiate (poly : Type.Poly.t) : Type.Mono.t t =
  let open Let_syntax in
  let { Type.Poly.forall_bound; forall_bound_effects; monotype } = poly in
  let%bind (var_to_meta : Type.Metavariable.t Type.Variable.Map.t) =
    Set.to_map forall_bound ~f:(fun _v -> fresh_metavariable) |> all_map
  in
  let%map (effect_var_to_meta : Effect.Metavariable.t Effect.Variable.Map.t) =
    Set.to_map forall_bound_effects ~f:(fun _v -> fresh_effect_metavariable)
    |> all_map
  in
  Type.Mono.instantiate_as monotype ~var_to_meta ~effect_var_to_meta
;;

let instantiate_type : Type.t -> Type.Mono.t t =
  let open Let_syntax in
  function
  | Type.Mono t -> return t
  | Type.Poly t -> instantiate t
;;

let generalise_total : Type.Mono.t -> env:Context.t -> Type.Poly.t t =
 fun t ~env ->
  let open Let_syntax in
  let%bind t = use_substitution (fun s -> Substitution.apply_to_mono s t) in
  let%bind env = use_substitution (Context.apply_substitution env) in
  (* [e] was unified with total - has no metavariables *)
  let t_meta, t_effect_meta = Type.Mono.metavariables t in
  let env_meta, env_effect_meta = Context.metavariables env in
  (* entirely free metavariables *)
  let meta = Set.diff t_meta env_meta in
  let effect_meta = Set.diff t_effect_meta env_effect_meta in
  (* replace each of these metavariables with a fresh variable, and universally
     quanitfy over all those *)
  (* TODO: factor out the similarity here? *)
  let%bind (meta_to_var : Type.Variable.t Type.Metavariable.Map.t) =
    Set.to_map meta ~f:(fun _m -> fresh_variable) |> all_map
  in
  let meta_to_mono = Map.map meta_to_var ~f:(fun v -> Type.Mono.Variable v) in
  let%bind (effect_meta_to_var : Effect.Variable.t Effect.Metavariable.Map.t) =
    Set.to_map effect_meta ~f:(fun _m -> fresh_effect_variable) |> all_map
  in
  let effect_meta_to_effect =
    Map.map effect_meta_to_var ~f:(fun v -> Effect.Variable v)
  in
  let%bind () =
    map_substitution (fun substitution ->
        (* TODO: labelled parameters would make this much nicer *)
        let substitution =
          Substitution.extend_many_exn substitution meta_to_mono
        in
        Substitution.extend_many_effect_exn substitution effect_meta_to_effect)
  in
  (* actually replace the free metavariables with variables *)
  let%map t = use_substitution (fun s -> Substitution.apply_to_mono s t) in
  let forall_bound = Map.data meta_to_var |> Type.Variable.Set.of_list in
  let forall_bound_effects =
    Map.data effect_meta_to_var |> Effect.Variable.Set.of_list
  in
  let p = { Type.Poly.forall_bound; forall_bound_effects; monotype = t } in
  p
;;

let generalise : Type.Mono.t -> Effect.t -> env:Context.t -> Type.Poly.t t =
 fun t e ~env ->
  let open Let_syntax in
  let%bind () = unify_effects e Effect.total in
  generalise_total t ~env
;;

let run (f : 'a t) =
  let%map.Result x, s = f State.initial in
  let State.{ substitution; _ } = s in
  x, substitution
;;
