open! Core
open! Import

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

module Label = struct
  module T = String
  include T

  module Set = struct
    include Set
    include Set.Provide_hash (T)
  end
end

module Unknown = struct
  type t =
    | Metavariable of Metavariable.t
    | Variable of Variable.t
  [@@deriving sexp_of, compare, hash]

  let metavariables = function
    | Variable _ -> Metavariable.Set.empty
    | Metavariable v -> Metavariable.Set.singleton v
  ;;

  let instantiate_as t ~var_to_meta =
    match t with
    | Variable v ->
      (match Map.find var_to_meta v with
       | Some m -> Metavariable m
       | None -> Variable v)
    | Metavariable a -> Metavariable a
  ;;
end

type t =
  | Unknown of Unknown.t
  | Labels of Label.Set.t
  | Handled of Set.M(Label).t * Unknown.t
[@@deriving sexp_of, compare, hash]

let metavariables = function
  | Unknown unknown -> Unknown.metavariables unknown
  | Labels _ -> Metavariable.Set.empty
  | Handled (_, unknown) -> Unknown.metavariables unknown
;;

let instantiate_as t ~var_to_meta =
  match t with
  | Unknown unknown -> Unknown (Unknown.instantiate_as unknown ~var_to_meta)
  | Labels labels -> Labels labels
  | Handled (labels, unknown) ->
    Handled (labels, Unknown.instantiate_as unknown ~var_to_meta)
;;

let max_level t ~metavariable_level : int =
  match t with
  | Unknown (Metavariable meta) | Handled (_, Metavariable meta) ->
    metavariable_level meta
  | Unknown (Variable _) | Handled (_, Variable _) | Labels _ -> 0
;;
