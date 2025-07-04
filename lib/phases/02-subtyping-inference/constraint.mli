open! Core
open! Import

(** a single inequality between a pair of types or effects *)
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

include Hashable.S_plain with type t := t
