open! Core
open! Import

module T = struct
  type t =
    | Type_at_most of
        { type_lo : Type.Mono.t
        ; type_hi : Type.Mono.t
        }
    | Effect_at_most of
        { effect_lo : Effect.t
        ; effect_hi : Effect.t
        }
  [@@deriving sexp_of, compare, hash]
end

include T
include Hashable.Make_plain (T)
