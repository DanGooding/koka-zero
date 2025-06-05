open! Core
open! Import

module Operation = struct
  type t =
    { shape : Operation_shape.t
    ; argument : Type.Mono.t
    ; answer : Type.Mono.t
    }
  [@@deriving sexp_of]
end

type t =
  { name : Effect.Label.t
  ; operations : Operation.t Variable.Map.t
  }
[@@deriving sexp_of]

let console =
  let name = Effect.Label.of_string "console" in
  let operations =
    [ ( Variable.of_user "println"
      , { Operation.shape = Operation_shape.Fun
        ; argument = Type.Mono.Primitive Type.Primitive.Unit
        ; answer = Type.Mono.Primitive Type.Primitive.Unit
        } )
    ; ( Variable.of_user "println-int"
      , { Operation.shape = Operation_shape.Fun
        ; argument = Type.Mono.Primitive Type.Primitive.Int
        ; answer = Type.Mono.Primitive Type.Primitive.Unit
        } )
    ; ( Variable.of_user "print-int"
      , { Operation.shape = Operation_shape.Fun
        ; argument = Type.Mono.Primitive Type.Primitive.Int
        ; answer = Type.Mono.Primitive Type.Primitive.Unit
        } )
    ; ( Variable.of_user "read-int"
      , { Operation.shape = Operation_shape.Fun
        ; argument = Type.Mono.Primitive Type.Primitive.Unit
        ; answer = Type.Mono.Primitive Type.Primitive.Int
        } )
    ]
    |> Variable.Map.of_alist_exn
  in
  { name; operations }
;;
