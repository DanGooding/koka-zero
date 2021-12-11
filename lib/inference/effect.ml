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
  module Label = String

  module Multiset = struct
    (* invariant: if a label is present, then it has count >= 1 *)
    type t = int Label.Map.t [@@deriving sexp]

    (* TODO: [Label.Map.whatever] shouldn't be necessary, but here [Map] seems
       to refer to the parent module's map, giving confusing type errors *)

    (** restore the invariant that only items 'in' the multiset are keys in the
        map *)
    let remove_zeros xs = Label.Map.filter xs ~f:(fun n -> n > 0)

    let of_list xs =
      List.map xs ~f:(fun x -> x, 1) |> Label.Map.of_alist_reduce ~f:Int.( + )
    ;;

    let empty = Label.Map.empty

    let add xs x =
      Label.Map.update xs x ~f:(fun n -> Option.value ~default:0 n + 1)
    ;;

    let union xs ys =
      Map.merge xs ys ~f:(fun ~key:_ data ->
          match data with
          | `Left n | `Right n -> Some n
          | `Both (m, n) -> Some (m + n))
    ;;

    let diff xs ys =
      Map.mapi xs ~f:(fun ~key:label ~data:nx ->
          let ny = Label.Map.find ys label |> Option.value ~default:0 in
          max (nx - ny) 0)
      |> remove_zeros
    ;;

    let is_empty xs = Label.Map.is_empty xs
  end

  (* TODO: putting this before [Multiset] gives weird type errors when using map
     methods *)
  include Label
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
