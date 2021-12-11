open Core
include Multiset_intf

module Make (Element : sig
  type t [@@deriving sexp]

  include Comparable.S with type t := t
end) =
struct
  module Multiset = struct
    module Element = Element

    (* invariant: if a label is present, then it has count >= 1 *)
    type t = int Element.Map.t [@@deriving sexp]

    (** restore the invariant that only items 'in' the multiset are keys in the
        map *)
    let remove_zeros xs = Element.Map.filter xs ~f:(fun n -> n > 0)

    let of_list xs =
      List.map xs ~f:(fun x -> x, 1) |> Element.Map.of_alist_reduce ~f:Int.( + )
    ;;

    let empty = Element.Map.empty

    let add xs x =
      Element.Map.update xs x ~f:(fun n -> Option.value ~default:0 n + 1)
    ;;

    let union xs ys =
      Map.merge xs ys ~f:(fun ~key:_ data ->
          match data with
          | `Left n | `Right n -> Some n
          | `Both (m, n) -> Some (m + n))
    ;;

    let inter xs ys =
      Map.merge xs ys ~f:(fun ~key:_ data ->
          match data with
          | `Left _ | `Right _ -> None
          | `Both (m, n) -> Some (min m n))
    ;;

    let diff xs ys =
      Map.mapi xs ~f:(fun ~key:label ~data:nx ->
          let ny = Element.Map.find ys label |> Option.value ~default:0 in
          max (nx - ny) 0)
      |> remove_zeros
    ;;

    let is_empty xs = Element.Map.is_empty xs
  end
end
