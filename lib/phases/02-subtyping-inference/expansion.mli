open! Core
open! Import

(** statefully tracks fully-expanded metavariables *)
type t [@@deriving sexp_of]

val create
  :  constraints:Constraints.t
  -> type_variable_source:Type.Variable.Name_source.t
  -> effect_variable_source:Effect.Variable.Name_source.t
  -> t

val expand_type
  :  ?context_type:Polar_type.t
  -> ?simplify:bool
  -> t
  -> Type.Mono.t
  -> Polar_type.t

val expand_effect
  :  ?context_type:Polar_type.t
  -> ?simplify:bool
  -> t
  -> Effect.t
  -> Polar_type.Effect.t
