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
    [@@deriving sexp_of]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T

  let metavariables = function
    | Int | Bool | Unit -> Metavariable.Set.empty, Effect.Metavariable.Set.empty
  ;;

  let instantiate_as t ~var_to_meta:_ ~effect_var_to_meta:_ =
    match t with
    | (Int | Bool | Unit) as t -> t
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
    [@@deriving sexp_of]
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
    | Primitive p ->
      Primitive (Primitive.instantiate_as p ~var_to_meta ~effect_var_to_meta)
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
end

type t =
  | Mono of Mono.t
  | Poly of Poly.t
[@@deriving sexp_of]

let metavariables = function
  | Poly p -> Poly.metavariables p
  | Mono t -> Mono.metavariables t
;;
