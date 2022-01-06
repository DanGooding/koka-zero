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
