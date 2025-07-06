open Core

module Variable = struct
  module T = struct
    include String

    let of_generated_name s = s
  end

  include T
  module Name_source = Name_source.Make (T)
end

module Metavariable = struct
  module T = struct
    include String

    let of_generated_name s = s
  end

  include T
  module Name_source = Name_source.Make (T)
end

module Primitive = struct
  module T = struct
    type t =
      | Int
      | Bool
      | Unit
    [@@deriving equal, compare, sexp_of, hash]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T

  let metavariables = function
    | Int | Bool | Unit -> Metavariable.Set.empty, Effect.Metavariable.Set.empty
  ;;
end

(* TODO: currently have no annotations, but will need to use a variant to
   namespace user names from generated names *)

module Mono = struct
  module T = struct
    type t =
      | Arrow of t list * Effect.t * t
      | Variable of Variable.t
      | Metavariable of Metavariable.t
      | Primitive of Primitive.t
    [@@deriving sexp_of, compare, hash]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T

  let rec metavariables = function
    | Metavariable v ->
      Metavariable.Set.singleton v, Effect.Metavariable.Set.empty
    | Variable _ -> Metavariable.Set.empty, Effect.Metavariable.Set.empty
    | Primitive p -> Primitive.metavariables p
    | Arrow (t_args, effect_, t_result) ->
      let arg_metas, arg_effect_metas =
        List.map t_args ~f:metavariables |> List.unzip
      in
      let result_meta, result_effect_meta = metavariables t_result in
      let effect_meta = Effect.metavariables effect_ in
      ( Metavariable.Set.union_list (result_meta :: arg_metas)
      , Effect.Metavariable.Set.union_list
          (effect_meta :: result_effect_meta :: arg_effect_metas) )
  ;;

  let rec instantiate_as t ~var_to_meta ~effect_var_to_meta =
    match t with
    | Variable v ->
      (match Map.find var_to_meta v with
       | Some m -> Metavariable m
       | None -> Variable v)
    | Metavariable m -> Metavariable m
    | Arrow (t_args, eff, t_result) ->
      Arrow
        ( List.map t_args ~f:(instantiate_as ~var_to_meta ~effect_var_to_meta)
        , Effect.instantiate_as eff ~var_to_meta:effect_var_to_meta
        , instantiate_as t_result ~var_to_meta ~effect_var_to_meta )
    | Primitive p -> Primitive p
  ;;
end

module Poly = struct
  type t =
    { forall_bound : Variable.Set.t
    ; forall_bound_effects : Effect.Variable.Set.t
    ; monotype : Mono.t
    }
  [@@deriving sexp_of]

  let metavariables t =
    let { monotype; _ } = t in
    Mono.metavariables monotype
  ;;

  let wrap_monotype monotype =
    let forall_bound = Variable.Set.empty in
    let forall_bound_effects = Effect.Variable.Set.empty in
    { monotype; forall_bound; forall_bound_effects }
  ;;
end

type t =
  | Mono of Mono.t
  | Poly of Poly.t
[@@deriving sexp_of]

let metavariables = function
  | Poly p -> Poly.metavariables p
  | Mono t -> Mono.metavariables t
;;

let generalise
      (mono : Mono.t)
      ~should_generalise_type_metavariable
      ~should_generalise_effect_metavariable
      ~fresh_type_variable
      ~fresh_effect_variable
  : Poly.t
  =
  let type_meta_to_var = Metavariable.Table.create () in
  let effect_meta_to_var = Effect.Metavariable.Table.create () in
  let rec generalise_aux (mono : Mono.t) : Mono.t =
    match mono with
    | Variable v -> Variable v
    | Metavariable m when should_generalise_type_metavariable m ->
      let v =
        Hashtbl.find_or_add type_meta_to_var m ~default:fresh_type_variable
      in
      Variable v
    | Metavariable m -> Metavariable m
    | Arrow (t_args, eff, t_result) ->
      Arrow
        ( List.map t_args ~f:generalise_aux
        , generalise_effect_aux eff
        , generalise_aux t_result )
    | Primitive p -> Primitive p
  and generalise_effect_aux (effect_ : Effect.t) : Effect.t =
    match effect_ with
    | Unknown unknown -> Unknown (generalise_unknown_effect_aux unknown)
    | Labels labels -> Labels labels
    | Handled (labels, unknown) ->
      Handled (labels, generalise_unknown_effect_aux unknown)
  and generalise_unknown_effect_aux (effect_ : Effect.Unknown.t)
    : Effect.Unknown.t
    =
    match effect_ with
    | Variable v -> Variable v
    | Metavariable m when should_generalise_effect_metavariable m ->
      let v =
        Hashtbl.find_or_add effect_meta_to_var m ~default:fresh_effect_variable
      in
      Variable v
    | Metavariable m -> Metavariable m
  in
  let monotype = generalise_aux mono in
  let forall_bound = Hashtbl.data type_meta_to_var |> Variable.Set.of_list in
  let forall_bound_effects =
    Hashtbl.data effect_meta_to_var |> Effect.Variable.Set.of_list
  in
  { monotype; forall_bound; forall_bound_effects }
;;

let instantiate
      (poly : Poly.t)
      ~fresh_type_metavariable
      ~fresh_effect_metavariable
  =
  let var_to_meta =
    Set.to_map poly.forall_bound ~f:(fun _var -> fresh_type_metavariable ())
  in
  let effect_var_to_meta =
    Set.to_map poly.forall_bound_effects ~f:(fun _var ->
      fresh_effect_metavariable ())
  in
  Mono.instantiate_as poly.monotype ~var_to_meta ~effect_var_to_meta
;;
