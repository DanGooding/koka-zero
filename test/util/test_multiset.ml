open Core
open Koka_zero_util

module Name : sig
  type t [@@deriving sexp_of]

  val t_of_string : string -> t

  include Has_multiset.S with type t := t
end = struct
  include String

  let t_of_string x = x

  include Has_multiset.Make (String)
end

let of_strings xs = List.map xs ~f:Name.t_of_string |> Name.Multiset.of_list

let non_empty_of_strings_exn xs =
  List.map xs ~f:Name.t_of_string |> Name.Multiset.Non_empty.of_list_exn
;;

let print_multiset names =
  Name.Multiset.sexp_of_t names |> Sexp.to_string_hum |> print_endline
;;

let%expect_test "can convert to and from list" =
  let names = of_strings [ "a"; "b"; "c"; "c"; "a"; "e"; "f"; "f"; "f" ] in
  print_multiset names;
  [%expect {| ((a 2) (b 1) (c 2) (e 1) (f 3)) |}];
  let names = Name.Multiset.to_list names in
  [%sexp (names : Name.t list)] |> Sexp.to_string_hum |> print_endline;
  [%expect {| (f f f e c c b a a) |}]
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

let%expect_test "cannot create empty non_empty_multiset" =
  let x = of_strings [] |> Name.Multiset.Non_empty.of_multiset in
  [%sexp (x : Name.Multiset.Non_empty.t option)] |> print_s;
  [%expect {| () |}];
  let y = non_empty_of_strings_exn [ "a"; "a"; "b" ] in
  let z = non_empty_of_strings_exn [ "c"; "d"; "e" ] in
  Name.Multiset.Non_empty.inter_opt y z
  |> [%sexp_of: Name.Multiset.Non_empty.t option]
  |> print_s;
  [%expect {| () |}];
  let w = non_empty_of_strings_exn [ "a"; "a"; "a"; "b"; "b"; "c" ] in
  Name.Multiset.Non_empty.diff_opt y w
  |> [%sexp_of: Name.Multiset.Non_empty.t option]
  |> print_s;
  [%expect {| ()  |}]
;;
