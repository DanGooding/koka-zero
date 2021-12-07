open Core

module Variable = struct
  module T = struct
    include String

    let module_name = "Variable"
  end

  include Identifiable.Make_plain (T)
  include Name_source.Make (T)
end

module Metavariable = struct
  module T = struct
    include String

    let module_name = "Metavariable"
  end

  include Identifiable.Make_plain (T)
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

    let add x xs =
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

  let extend t l =
    let { labels; tail } = t in
    let labels = Label.Multiset.add labels l in
    { labels; tail }
  ;;

  let is_open { labels; _ } = Option.is_some labels

  let metavariables { tail; _ } =
    match tail with
    | Variable _ -> Metavariable.Set.empty
    | Metavariable v -> Metavariable.Set.singleton v
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
