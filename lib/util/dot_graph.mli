open! Core

module Node_id : sig
  type t [@@deriving sexp_of]

  val of_string : string -> t
end

module Edge_disambiguator : sig
  type t [@@deriving sexp_of]

  val of_string : string -> t
end

module Attrs : sig
  type t = (string * string) list [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val create : unit -> t
val add_node : ?attrs:Attrs.t -> t -> Node_id.t -> unit

(* will add the nodes if they don't exist *)
val add_edge
  :  ?disambiguator:Edge_disambiguator.t
  -> ?attrs:Attrs.t
  -> t
  -> from:Node_id.t
  -> to_:Node_id.t
  -> unit

val write : t -> Out_channel.t -> unit
