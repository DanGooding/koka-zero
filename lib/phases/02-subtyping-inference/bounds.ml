open! Core
open! Import

type 'a t =
  { lower_bounds : 'a Hash_set.t
  ; upper_bounds : 'a Hash_set.t
  ; key : ('a Hashtbl_intf.Hashtbl.Key.t[@sexp.opaque])
  }
[@@deriving sexp_of]

let create key =
  let lower_bounds = Hash_set.create key in
  let upper_bounds = Hash_set.create key in
  { lower_bounds; upper_bounds; key }
;;

let freshen t ~lower ~upper =
  let fresh = create t.key in
  (match lower with
   | `Empty -> ()
   | `Map f ->
     Hash_set.iter t.lower_bounds ~f:(fun lower_bound ->
       Hash_set.add fresh.lower_bounds (f lower_bound)));
  (match upper with
   | `Empty -> ()
   | `Map f ->
     Hash_set.iter t.upper_bounds ~f:(fun upper_bound ->
       Hash_set.add fresh.upper_bounds (f upper_bound)));
  fresh
;;

let bound_set t which =
  match which with
  | `lower -> t.lower_bounds
  | `upper -> t.upper_bounds
;;

let add t which ub =
  (* avoid hitting the mutation-while-iterating check in the specific case that
     we're adding a duplicate. This comes up when propagating constraints when
     a pair of metavariables are equal (<= and >=) *)
  let set = bound_set t which in
  if not (Hash_set.mem set ub) then Hash_set.add set ub
;;

let to_list t which = bound_set t which |> Hash_set.to_list

(* copy to allow modification while iterating *)
let iter t which ~f = to_list t which |> List.iter ~f
let fold t which ~init ~f = to_list t which |> List.fold ~init ~f

let result_iter t which ~f =
  fold t which ~init:(Ok ()) ~f:(fun maybe_unit x ->
    let%bind.Result () = maybe_unit in
    f x)
;;
