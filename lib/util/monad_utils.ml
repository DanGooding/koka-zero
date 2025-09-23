open Core
include Monad_utils_intf

module Make (M : Monad.S) = struct
  type 'a t = 'a M.t

  let list_fold xs ~init ~f =
    let open M.Let_syntax in
    List.fold xs ~init:(return init) ~f:(fun acc x ->
      let%bind acc = acc in
      f acc x)
  ;;

  let list_fold_right xs ~init ~f =
    let open M.Let_syntax in
    List.fold_right xs ~init:(return init) ~f:(fun x acc ->
      let%bind acc = acc in
      f x acc)
  ;;

  let list_concat_map xs ~f =
    let open M.Let_syntax in
    let%map lists = List.map xs ~f |> M.all in
    List.concat lists
  ;;

  let list_iter xs ~f = List.map xs ~f |> M.all_unit

  let list_iter2 xs ys ~f =
    let open M.Let_syntax in
    match (List.map2 xs ys ~f : _ List.Or_unequal_lengths.t) with
    | Unequal_lengths -> return List.Or_unequal_lengths.Unequal_lengths
    | Ok results ->
      let%map () = M.all_unit results in
      List.Or_unequal_lengths.Ok ()
  ;;

  let list_map xs ~f = List.map xs ~f |> M.all

  let rec list_find_map xs ~f =
    let open M.Let_syntax in
    match xs with
    | [] -> return None
    | x :: xs ->
      (match%bind f x with
       | None -> list_find_map xs ~f
       | Some y -> return (Some y))
  ;;

  let map_mapi m ~f =
    let open M.Let_syntax in
    Map.fold
      m
      ~init:(return (Map.empty (Map.comparator_s m)))
      ~f:(fun ~key ~data acc ->
        let%bind acc = acc in
        let%map data = f ~key ~data in
        Map.add_exn acc ~key ~data)
  ;;

  let map_fold xs ~init ~f =
    let open M.Let_syntax in
    Map.fold xs ~init:(return init) ~f:(fun ~key ~data acc ->
      let%bind acc = acc in
      f ~key ~data acc)
  ;;

  let map_fold_right xs ~init ~f =
    let open M.Let_syntax in
    Map.fold xs ~init:(return init) ~f:(fun ~key ~data acc ->
      let%bind acc = acc in
      f ~key ~data acc)
  ;;

  let all_map ts =
    let open M.Let_syntax in
    let cmp = Map.comparator_s ts in
    map_fold ts ~init:(Map.empty cmp) ~f:(fun ~key ~data m ->
      let%map data = data in
      Map.add_exn m ~key ~data)
  ;;

  let all_map_unit ts = map_fold ts ~init:() ~f:(fun ~key:_ ~data () -> data)

  let all_option =
    let open M.Let_syntax in
    function
    | None -> return None
    | Some m ->
      let%map x = m in
      Some x
  ;;

  let all_non_empty (Non_empty_list.Cons (t, ts)) =
    let open M.Let_syntax in
    let%bind x = t in
    let%map xs = M.all ts in
    Non_empty_list.Cons (x, xs)
  ;;
end
