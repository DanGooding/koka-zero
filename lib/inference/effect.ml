open Core

module Variable = struct
  module T = struct
    include String

    let module_name = "Variable"
  end

  include T
  include Name_source.Make (T)
end

module Metavariable = struct
  module T = struct
    include String

    let module_name = "Metavariable"
  end

  include T
  include Name_source.Make (T)
end

module Label = struct
  module T = String
  include T

  module Multiset = struct
    (* TODO: is this the right way to make a Label.Map? *)
    type t = int T.Map.t [@@deriving sexp]

    let of_list xs = T.Map.of_alist_fold ~init:0 ~f:Int.( + ) xs
    let empty = T.Map.empty

    let add xs x =
      Map.update xs x ~f:(function
          | Some n -> n + 1
          | None -> 0)
    ;;

    let union xs ys = Map.merge_skewed xs ys ~combine:Int.( + )

    let diff xs ys =
      Map.mapi xs ~f:(fun ~key:label ~data:nx ->
          let ny = Map.find ys label |> Option.value ~default:0 in
          max (nx - ny) 0)
    ;;

    let is_empty xs = Map.is_empty xs
  end
end

module Row = struct
  module Tail = struct
    module T = struct
      type t =
        | Variable of Variable.t
        | Metavariable of Metavariable.t
      [@@deriving sexp]
    end (* disable "fragile-match" for generated code *) [@warning "-4"]

    include T
  end

  (** An effect row - a multiset of labels, possibly with a variable in the tail *)
  type t =
    { labels : Label.Multiset.t
    ; tail : Tail.t option
    }
  [@@deriving sexp]

  let total =
    let labels = Label.Multiset.empty in
    let tail = None in
    { labels; tail }
  ;;

  let extend t l =
    let { labels; tail } = t in
    let labels = Label.Multiset.add labels l in
    { labels; tail }
  ;;

  let is_open { tail; _ } = Option.is_some tail

  let metavariables { tail; _ } =
    match tail with
    | Some (Tail.Metavariable v) -> Metavariable.Set.singleton v
    | Some (Tail.Variable _) | None -> Metavariable.Set.empty
  ;;

  let instantiate_as { tail; _ } ~var_to_meta =
    let open Option.Let_syntax in
    match%bind tail with
    | Tail.Variable v ->
      let%map m = Map.find var_to_meta v in
      Tail.Metavariable m
    | Tail.Metavariable v -> Some (Tail.Metavariable v)
  ;;

  let is_total { labels; tail } =
    if Label.Multiset.is_empty labels
    then (
      match tail with
      | Some Tail.(Metavariable _ | Variable _) -> None
      | None -> Some true)
    else Some false
  ;;
end

module T = struct
  type t =
    | Metavariable of Metavariable.t
    | Variable of Variable.t
    | Row of Row.t
  [@@deriving sexp]
end (* disable "fragile-match" for generated code *) [@warning "-4"]

include T

let metavariables = function
  | Variable _ -> Metavariable.Set.empty
  | Metavariable v -> Metavariable.Set.singleton v
  | Row r -> Row.metavariables r
;;

let instantiate_as ~var_to_meta = function
  | Variable v ->
    (match Map.find var_to_meta v with
    | Some m -> Metavariable m
    | None -> Variable v)
  | Metavariable a -> Metavariable a
  | Row r -> Row.instantiate_as r ~var_to_meta
;;

let total = Row Row.total

let is_total = function
  | Variable _ | Metavariable _ -> None
  | Row r -> Row.is_total r
;;
