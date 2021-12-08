open Core

type t = Type.t Minimal_syntax.Variable.Map.t [@@deriving sexp]

let extend t ~var ~type_ = Map.set t ~key:var ~data:type_
let find t var = Map.find t var
let empty = Minimal_syntax.Variable.Map.empty
let apply_substitution t subst = Map.map t ~f:(Substitution.apply subst)

let metavariables t =
  let metavariable_sets, effect_metavariable_sets =
    Map.map t ~f:Type.metavariables |> Map.data |> List.unzip
  in
  ( Type.Metavariable.Set.union_list metavariable_sets
  , Effect.Metavariable.Set.union_list effect_metavariable_sets )
;;
