open Core

(** augments an [Identifiable] adding a submodule [Name_source] which allows the
    generation of unique values of type [t] in a pure way (wihout mutable state) *)

module type S = sig
  type t

  module Name_source : sig
    type name = t
    type t [@@deriving sexp]

    val fresh : ?prefix:string -> unit -> t
    val next_name : t -> name * t
  end
end

module Make (N : Identifiable.S) : S with type t := N.t
