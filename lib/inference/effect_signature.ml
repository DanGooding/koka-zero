open Core

module Operation = struct
  type t =
    { argument_type : Type.Mono.t
    ; answer_type : Type.Mono.t
    }
  [@@deriving sexp]
  (* TODO: include number of arguments when this becomes varaiable *)
end

module Signature = struct
  module T = struct
    type t = Variable.Set.t [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

include Signature

let t_of_map m = Variable.Map.key_set m
let t_of_handler { Minimal_syntax.Expr.operations; _ } = t_of_map operations

module Context = struct
  let signature_of_map = t_of_map

  type t = Effect.Label.t Signature.Map.t [@@deriving sexp]

  let empty = Signature.Map.empty
  let extend t ~label ~signature = Map.add t ~key:signature ~data:label

  let extend_decl t { Minimal_syntax.Decl.Effect.name; operations } =
    let signature = signature_of_map operations in
    extend t ~label:name ~signature
  ;;

  let find t s = Map.find t s
end
