open Core

type t = Type.t Minimal_syntax.Variable.Map.t [@@deriving sexp]

let extend t ~var ~type_ = Map.set t ~key:var ~data:type_
let find t var = Map.find t var
let empty = Minimal_syntax.Variable.Map.empty
let apply_substitution t subst = Map.map t ~f:(Substitution.apply subst)

let metavariables t =
  Map.map t ~f:Type.metavariables
  |> Map.data
  |> Type.Metavariable.Set.union_list
;;
