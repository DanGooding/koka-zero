open Core

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
  module T = struct
    type t = (Effect.Label.t * Operation_shape.t Variable.Map.t) Signature.Map.t
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T

  let empty = Signature.Map.empty

  let extend t ~label ~operation_shapes =
    let signature = Variable.Map.key_set operation_shapes in
    Map.add t ~key:signature ~data:(label, operation_shapes)
  ;;

  let extend_decl t { Minimal_syntax.Decl.Effect.name; operations } =
    let operation_shapes =
      Variable.Map.map
        operations
        ~f:(fun { Minimal_syntax.Decl.Effect.Operation.shape; _ } -> shape)
    in
    extend t ~label:name ~operation_shapes
  ;;

  let find t s = Map.find t s
end
