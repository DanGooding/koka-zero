open Core
open Koka_zero_util

module Writer : sig
  type 'a t

  include Monad.S with type 'a t := 'a t
  include Monad_utils.S with type 'a t := 'a t

  val write : int -> unit t
  val collect : 'a t -> 'a * int list
end = struct
  module T = struct
    (* list of ints build up in reverse order *)
    type 'a t = 'a * int list

    let return x = x, []

    let bind (x, ys) ~f =
      let x', ys' = f x in
      x', ys' @ ys
    ;;

    let map = `Define_using_bind
  end

  module T' = struct
    include T
    include Monad.Make (T)
  end

  include T'
  include Monad_utils.Make (T')

  let write x = (), [ x ]
  let collect (x, ys) = x, List.rev ys
end

let print_written m =
  let _x, ys = Writer.collect m in
  [%sexp (ys : int list)] |> Sexp.to_string_hum |> print_endline
;;

let%expect_test "all_map and all_map_unit sequence in the same order" =
  let values = [ 6; 1; 3; 2; 4; 5 ] in
  let write_values =
    List.map values ~f:(fun i -> i, Writer.write i) |> Int.Map.of_alist_exn
  in
  print_written (Writer.all_map write_values);
  [%expect {| (1 2 3 4 5 6) |}];
  print_written (Writer.all_map_unit write_values);
  [%expect {| (1 2 3 4 5 6) |}]
;;

let%expect_test "list_fold gives the same order as List.fold" =
  let xs = [ 1; 2; 3; 4; 5 ] in
  let f () x = printf "%d\n" x in
  List.fold xs ~init:() ~f;
  [%expect
    {|
    1
    2
    3
    4
    5 |}];
  let f' () x =
    let open Writer.Let_syntax in
    let%bind () = Writer.write x in
    return (f () x)
  in
  let action = Writer.list_fold xs ~init:() ~f:f' in
  let (), written = Writer.collect action in
  [%expect
    {|
    1
    2
    3
    4
    5 |}];
  [%sexp (written : int list)] |> print_s;
  [%expect {| (1 2 3 4 5) |}]
;;

let%expect_test "list_fold gives reverse order to list_fold_right" =
  let xs = [ 1; 2; 3; 4; 5 ] in
  let print_xs = Writer.list_fold xs ~init:() ~f:(fun () -> Writer.write) in
  let print_xs_right =
    Writer.list_fold_right xs ~init:() ~f:(fun x () -> Writer.write x)
  in
  print_written print_xs;
  [%expect {| (1 2 3 4 5) |}];
  print_written print_xs_right;
  [%expect {| (5 4 3 2 1) |}]
;;
