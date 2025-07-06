open! Core
open! Import

type t [@@deriving sexp_of]

val empty : t
val extend : t -> name:Variable.t -> type_:Type.t -> t
val get_exn : t -> Variable.t -> Type.t
