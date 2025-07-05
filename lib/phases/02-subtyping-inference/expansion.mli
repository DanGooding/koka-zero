open! Core
open! Import

(** statefully tracks fully-expanded metavariables *)
type t

val create : constraints:Constraints.t -> t
val expand_type : t -> Type.Mono.t -> Polar_type.t
val expand_effect : t -> Effect.t -> Polar_type.Effect.t
