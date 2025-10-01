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
