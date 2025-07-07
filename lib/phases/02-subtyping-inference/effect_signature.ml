open! Core
open! Import

module Signature = struct
  module T = struct
    type t = Variable.Set.t [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

include Signature

let t_of_map m = Core.Map.key_set m
let t_of_handler { Minimal_syntax.Expr.operations; _ } = t_of_map operations

module Context = struct
  type t = (Effect.Label.t * Operation_shape.t Variable.Map.t) Signature.Map.t
  [@@deriving sexp_of]

  let empty = Signature.Map.empty

  let extend t ~label ~operation_shapes =
    let signature = Core.Map.key_set operation_shapes in
    Core.Map.add t ~key:signature ~data:(label, operation_shapes)
  ;;

  let extend_decl t { Effect_decl.name; operations } =
    let operation_shapes =
      Variable.Map.map operations ~f:(fun { Effect_decl.Operation.shape; _ } ->
        shape)
    in
    extend t ~label:name ~operation_shapes
  ;;

  let find t s = Core.Map.find t s
end
