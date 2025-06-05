open! Core
open! Import

module Operation : sig
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

(** effect of interacting with stdin/stdout. A single `console` handler is
    wrapped around the toplevel call to `main()` *)
val console : t
