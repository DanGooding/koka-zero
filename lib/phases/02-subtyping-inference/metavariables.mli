open! Core
open! Import

(** manages the metadata of metavariables *)
type t [@@deriving sexp_of]

val create : unit -> t
val fresh_type_metavariable : t -> level:int -> Type.Metavariable.t
val fresh_effect_metavariable : t -> level:int -> Effect.Metavariable.t
val fresh_type : t -> level:int -> Type.Mono.t
val fresh_effect : t -> level:int -> Effect.t
val type_level_exn : t -> Type.Metavariable.t -> int
val effect_level_exn : t -> Effect.Metavariable.t -> int
