open Core
include Name_source_intf

module Make (Name : Name_S) = struct
  module Name = Name

  type t =
    { next : int
    ; prefix : string
    }
  [@@deriving sexp]

  let fresh ?prefix () = { next = 0; prefix = Option.value prefix ~default:"" }

  let next_name t =
    let { next; prefix } = t in
    let name = sprintf "%s%d" prefix next in
    let name = Name.of_generated_name name in
    let next = next + 1 in
    let t = { t with next } in
    name, t
  ;;
end
