open! Core
open! Import

(** the stateful collection of a partially solved set of constraints on metavariables.
*)
type t [@@deriving sexp_of]

val create : metavariables:Metavariables.t -> t

(** [constrain_type_at_most t ty1 ty2] adds [ty1 <= ty2] **)
val constrain_type_at_most : t -> Type.Mono.t -> Type.Mono.t -> unit Or_error.t

val constrain_effect_at_most : t -> Effect.t -> Effect.t -> unit Or_error.t
val get_type_bounds : t -> Type.Metavariable.t -> Type.Mono.t Bounds.t option
val get_effect_bounds : t -> Effect.Metavariable.t -> Effect.t Bounds.t option

val add_fresh_type_exn
  :  t
  -> Type.Metavariable.t
  -> Type.Mono.t Bounds.t
  -> unit

val add_fresh_effect_exn
  :  t
  -> Effect.Metavariable.t
  -> Effect.t Bounds.t
  -> unit
