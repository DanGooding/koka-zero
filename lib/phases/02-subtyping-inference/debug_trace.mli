open! Core
open! Import

type t [@@deriving sexp_of]

val create_enabled_by_environment_variable : string -> t
val enter : t -> (unit -> Sexp.t) -> t
val log : t -> (unit -> Sexp.t) -> unit
val print_stack : t -> unit
