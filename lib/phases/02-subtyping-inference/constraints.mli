open! Core
open! Import

(** the stateful collection of a partially solved set of constraints on metavariables.
*)
type t [@@deriving sexp_of]

val create : unit -> t

(** [constrain_type_at_most_exn t ty1 ty2] adds [ty1 <= ty2] **)
val constrain_type_at_most_exn : t -> Type.Mono.t -> Type.Mono.t -> unit

val constrain_effect_at_most_exn : t -> Effect.t -> Effect.t -> unit
