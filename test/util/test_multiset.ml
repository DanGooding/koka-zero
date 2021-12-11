open Core
open Koka_zero_util

module Name : sig
  type t

  val t_of_string : string -> t

  include Multiset.S with type Multiset.Element.t := t
end = struct
  include String

  let t_of_string x = x

  include Multiset.Make (String)
end

let of_strings xs = List.map xs ~f:Name.t_of_string |> Name.Multiset.of_list

let print_multiset names =
  Name.Multiset.sexp_of_t names |> Sexp.to_string_hum |> print_endline
;;

let%expect_test "can build from list" =
  let names = of_strings [ "a"; "b"; "c"; "c"; "a"; "e"; "f"; "f"; "f" ] in
  print_multiset names;
  [%expect {| ((a 2) (b 1) (c 2) (e 1) (f 3)) |}]
;;

let%expect_test "set operations work correctly" =
  let x = of_strings [ "a"; "b"; "b"; "c" ] in
  let y = of_strings [ "a"; "a"; "a"; "b"; "c"; "d"; "e"; "f" ] in
  let z = of_strings [ "c"; "d"; "f"; "f" ] in
  Name.Multiset.union x y |> print_multiset;
  [%expect {| ((a 4) (b 3) (c 2) (d 1) (e 1) (f 1)) |}];
  Name.Multiset.union y z |> print_multiset;
  [%expect {| ((a 3) (b 1) (c 2) (d 2) (e 1) (f 3)) |}];
  Name.Multiset.diff x y |> print_multiset;
  [%expect {| ((b 1)) |}];
  Name.Multiset.diff y x |> print_multiset;
  [%expect {| ((a 2) (d 1) (e 1) (f 1)) |}];
  Name.Multiset.diff y z |> print_multiset;
  [%expect {| ((a 3) (b 1) (e 1)) |}];
  Name.Multiset.inter x y |> print_multiset;
  [%expect {| ((a 1) (b 1) (c 1)) |}]
;;
