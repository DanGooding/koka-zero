open Core

module T = struct
  type 'a t = Cons of 'a * 'a list [@@deriving sexp]
end (* disable "fragile-match" for generated code *) [@warning "-4"]

include T

let of_list = function
  | [] -> None
  | x :: xs -> Cons (x, xs) |> Some
;;

let of_list_exn xs =
  match of_list xs with
  | Some xs -> xs
  | None -> raise_s [%message "expected list to have at least one element"]
;;

let to_list (Cons (x, xs)) = x :: xs

let map (Cons (x, xs)) ~f =
  let y = f x in
  let ys = List.map xs ~f in
  Cons (y, ys)
;;
