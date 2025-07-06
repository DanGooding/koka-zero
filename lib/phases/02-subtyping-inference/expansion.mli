open! Core
open! Import

(** statefully tracks fully-expanded metavariables *)
type t

val create
  :  constraints:Constraints.t
  -> type_variable_source:Type.Variable.Name_source.t
  -> effect_variable_source:Effect.Variable.Name_source.t
  -> t

val expand_type : t -> Type.Mono.t -> Polar_type.t
val expand_effect : t -> Effect.t -> Polar_type.Effect.t
