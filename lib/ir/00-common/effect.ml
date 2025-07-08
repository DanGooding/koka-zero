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

type t =
  | Metavariable of Metavariable.t
  | Labels of Label.Set.t
  | Handled of Set.M(Label).t * Metavariable.t
[@@deriving sexp_of, compare, hash]

let metavariables = function
  | Metavariable meta -> Metavariable.Set.singleton meta
  | Labels _ -> Metavariable.Set.empty
  | Handled (_, meta) -> Metavariable.Set.singleton meta
;;

let max_level t ~metavariable_level : int =
  match t with
  | Metavariable meta | Handled (_, meta) -> metavariable_level meta
  | Labels _ -> 0
;;
