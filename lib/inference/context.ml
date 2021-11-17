open Core

type t = Type.t Minimal_syntax.Variable.Map.t [@@deriving sexp]

let extend t ~var ~type_ = Map.set t ~key:var ~data:type_
let find t var = Map.find t var
let empty = Minimal_syntax.Variable.Map.empty
