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

  let rec max_level t ~type_metavariable_level ~effect_metavariable_level =
    match t with
    | Variable _ | Primitive _ -> 0
    | Metavariable meta -> type_metavariable_level meta
    | Arrow (args, effect_, result) ->
      let type_levels =
        List.map
          (result :: args)
          ~f:(max_level ~type_metavariable_level ~effect_metavariable_level)
      in
      let effect_level =
        Effect.max_level effect_ ~metavariable_level:effect_metavariable_level
      in
      List.max_elt (effect_level :: type_levels) ~compare:[%compare: int]
      |> Option.value ~default:0
  ;;
end

module Poly = struct
  type t =
    { forall_bound : Metavariable.t -> bool
    ; forall_bound_effects : Effect.Metavariable.t -> bool
    ; monotype : Mono.t
    }
  [@@deriving sexp_of]
end

type t =
  | Mono of Mono.t
  | Poly of Poly.t
[@@deriving sexp_of]

let generalise
      (monotype : Mono.t)
      ~should_generalise_type_metavariable
      ~should_generalise_effect_metavariable
  : Poly.t
  =
  { monotype
  ; forall_bound = should_generalise_type_metavariable
  ; forall_bound_effects = should_generalise_effect_metavariable
  }
;;
