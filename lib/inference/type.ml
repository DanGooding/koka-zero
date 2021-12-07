open Core

module Variable = struct
  module T = String
  include T
  include Name_source.Make (T)
end

module Metavariable = struct
  module T = String
  include T
  include Name_source.Make (T)
end

module Primitive = struct
  module T = struct
    type t =
      | Int
      | Bool
      | Unit
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T

  let metavariables = function
    | Int | Bool | Unit -> Metavariable.Set.empty
  ;;

  let instantiate_as t _var_to_meta =
    match t with
    | (Int | Bool | Unit) as t -> t
  ;;
end

(* TODO: currently have no annotations, but will need to use a variant to
   namespace user names from generated names *)

module Mono = struct
  module T = struct
    type t =
      | Arrow of t * Effect.t * t
      | Variable of Variable.t
      | Metavariable of Metavariable.t
      | Primitive of Primitive.t
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T

  let rec metavariables = function
    | Metavariable v -> Metavariable.Set.singleton v
    | Variable _ -> Metavariable.Set.empty
    | Primitive p -> Primitive.metavariables p
    | Arrow (t_arg, t_result) ->
      Set.union (metavariables t_arg) (metavariables t_result)
  ;;

  let rec instantiate_as t var_to_meta =
    match t with
    | Variable v ->
      (match Map.find var_to_meta v with
      | Some m -> Metavariable m
      | None -> Variable v)
    | Metavariable m -> Metavariable m
    | Arrow (t_arg, t_result) ->
      Arrow
        (instantiate_as t_arg var_to_meta, instantiate_as t_result var_to_meta)
    | Primitive p -> Primitive (Primitive.instantiate_as p var_to_meta)
  ;;
end

(* TODO: probably deserves its own module *)
module Poly = struct
  type t =
    { (* TODO: make these private *)
      forall_bound : Variable.Set.t
    ; monotype : Mono.t
    }
  [@@deriving sexp]

  let metavariables t =
    let { monotype; _ } = t in
    Mono.metavariables monotype
  ;;
end

module T = struct
  type t =
    | Mono of Mono.t
    | Poly of Poly.t
  [@@deriving sexp]
end (* disable "fragile-match" for generated code *) [@warning "-4"]

include T

let metavariables = function
  | Poly p -> Poly.metavariables p
  | Mono t -> Mono.metavariables t
;;
