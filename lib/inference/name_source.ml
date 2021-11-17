open Core

module type S = sig
  type t

  module Name_source : sig
    type name = t
    type t [@@deriving sexp]

    val fresh : ?prefix:string -> unit -> t
    val next_name : t -> name * t
  end
end

module Make (N : Identifiable.S) = struct
  module Name_source = struct
    type name = N.t

    type t =
      { next : int
      ; prefix : string
      }
    [@@deriving sexp]

    let fresh ?prefix () =
      { next = 0; prefix = Option.value prefix ~default:"" }
    ;;

    let next_name t =
      let { next; prefix } = t in
      let name = sprintf "%s%d" prefix next in
      let name = N.of_string name in
      let next = next + 1 in
      let t = { t with next = next + 1 } in
      name, t
    ;;
  end
end
