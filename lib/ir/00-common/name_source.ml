open Core
include Name_source_intf

module Make (Name : Name_S) = struct
  module Name = Name

  type t =
    { mutable next : int
    ; prefix : string
    }
  [@@deriving sexp]

  let fresh ?prefix () = { next = 0; prefix = Option.value prefix ~default:"" }

  let next_name t =
    let name = sprintf "%s%d" t.prefix t.next in
    let name = Name.of_generated_name name in
    t.next <- t.next + 1;
    name
  ;;
end
