open! Core
open! Import

type t = Type.Mono.t Variable.Map.t [@@deriving sexp_of]

let empty = Variable.Map.empty
let extend t ~name ~type_ = Map.set t ~key:name ~data:type_
let get_exn t name = Map.find_exn t name
