open Core
include Monad_utils_intf

module Make (M : Monad.S) = struct
  type 'a t = 'a M.t

  let all_map ts =
    let open M.Let_syntax in
    let cmp = Map.comparator_s ts in
    Map.fold
      ts
      ~init:(return (Map.empty cmp))
      ~f:(fun ~key ~data acc ->
        let%bind m = acc in
        let%map data = data in
        Map.add_exn m ~key ~data)
  ;;

  let all_map_unit ts =
    let open M.Let_syntax in
    Map.fold ts ~init:(return ()) ~f:(fun ~key:_ ~data acc ->
        let%bind () = acc in
        data)
  ;;

  let all_option =
    let open M.Let_syntax in
    function
    | None -> return None
    | Some m ->
      let%map x = m in
      Some x
  ;;
end
