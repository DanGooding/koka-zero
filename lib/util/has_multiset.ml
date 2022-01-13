open Core
include Has_multiset_intf

module Make (Element : sig
  type t [@@deriving sexp]

  include Comparable.S with type t := t
end) =
struct
  module Multiset = struct
    module Multiset = struct
      module Element = Element

      (* invariant: all entries have value (count) >= 1 *)
      type t = int Element.Map.t [@@deriving sexp]

      (** restore the invariant that only items 'in' the multiset are keys in
          the map *)
      let remove_zeros xs = Element.Map.filter xs ~f:(fun n -> n > 0)

      let of_list xs =
        List.map xs ~f:(fun x -> x, 1)
        |> Element.Map.of_alist_reduce ~f:Int.( + )
      ;;

      let to_list xs =
        Element.Map.fold xs ~init:[] ~f:(fun ~key:element ~data:n acc ->
            List.init n ~f:(fun _i -> element) @ acc)
      ;;

      let empty = Element.Map.empty

      let add xs x =
        Element.Map.update xs x ~f:(fun n -> Option.value n ~default:0 + 1)
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
        Map.mapi xs ~f:(fun ~key:element ~data:nx ->
            let ny = Element.Map.find ys element |> Option.value ~default:0 in
            max (nx - ny) 0)
        |> remove_zeros
      ;;

      let is_empty xs = Element.Map.is_empty xs
    end

    module Non_empty = struct
      module T = struct
        type t = Non_empty of Multiset.t [@@deriving sexp]
      end (* disable "fragile-match" for generated code *) [@warning "-4"]

      include T

      let of_multiset m =
        if Multiset.is_empty m then None else Some (Non_empty m)
      ;;

      let of_multiset_verbose m =
        match of_multiset m with
        | None -> `Empty
        | Some m -> `Non_empty m
      ;;

      let of_multiset_exn m =
        match of_multiset m with
        | Some t -> t
        | None -> raise_s [%message "got empty multiset"]
      ;;

      let of_list xs = Multiset.of_list xs |> of_multiset
      let of_list_exn xs = Multiset.of_list xs |> of_multiset_exn
      let of_non_empty_list xs = Non_empty_list.to_list xs |> of_list_exn
      let to_list (Non_empty m) = Multiset.to_list m
      let to_non_empty_list t = to_list t |> Non_empty_list.of_list_exn
      let to_multiset (Non_empty m) = m
      let add (Non_empty m) x = Multiset.add m x |> Non_empty

      let union (Non_empty m1) (Non_empty m2) =
        Multiset.union m1 m2 |> Non_empty
      ;;

      let union_mixed (Non_empty m1) m2 = Multiset.union m1 m2 |> Non_empty
      let inter_to_multiset (Non_empty m1) (Non_empty m2) = Multiset.inter m1 m2
      let diff_to_multiset (Non_empty m1) (Non_empty m2) = Multiset.diff m1 m2
      let inter t1 t2 = inter_to_multiset t1 t2 |> of_multiset_verbose
      let diff t1 t2 = diff_to_multiset t1 t2 |> of_multiset_verbose
      let inter_opt t1 t2 = inter_to_multiset t1 t2 |> of_multiset
      let diff_opt t1 t2 = diff_to_multiset t1 t2 |> of_multiset
    end

    include Multiset
  end
end
