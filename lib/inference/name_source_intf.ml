module type Name_S = sig
  type t

  val of_generated_name : string -> t
end

module type S = sig
  module Name : Name_S

  type t [@@deriving sexp]

  val fresh : ?prefix:string -> unit -> t
  val next_name : t -> Name.t * t
end
