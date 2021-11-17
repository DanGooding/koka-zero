open Core

type t = Type.Mono.t Type.Metavariable.Map.t [@@deriving sexp]

let identity = Type.Metavariable.Map.empty
let extend t ~var ~type_ = Map.add t ~key:var ~data:type_
let extend_exn t ~var ~type_ = Map.add_exn t ~key:var ~data:type_
let find t var = Map.find t var
