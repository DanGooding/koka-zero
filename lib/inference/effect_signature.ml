open Core
open Minimal_syntax

module T = struct
  type t = Variable.Set.t [@@deriving compare, sexp]
end

include T
include Comparable.Make (T)

let of_handler { Expr.operations; _ } = Variable.Map.key_set operations

module Context = struct
  (** maps signatures to their effect labels *)
  type t = Effect.Label.t T.Map.t [@@deriving sexp]

  let empty = T.Map.empty
  let extend t ~label ~signature = T.Map.add t ~key:signature ~data:label
  let find t s = T.Map.find t s
end
