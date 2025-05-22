open Core
open Koka_zero_util

module Variable = struct
  module T = struct
    include String

    let of_generated_name s = s
  end

  include T
  module Name_source = Name_source.Make (T)
end

module Metavariable = struct
  module T = struct
    include String

    let of_generated_name s = s
  end

  include T
  module Name_source = Name_source.Make (T)
end

module Label = struct
  module T : Identifiable.S = String
  include T
  include Has_multiset.Make (T)
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

    let metavariables = function
      | Metavariable v -> Metavariable.Set.singleton v
      | Variable _ -> Metavariable.Set.empty
    ;;

    let instantiate_as t ~var_to_meta =
      match t with
      | Metavariable v -> Metavariable v
      | Variable v ->
        (match Map.find var_to_meta v with
        | Some m -> Metavariable m
        | None -> Variable v)
    ;;
  end

  module T = struct
    (** An effect row - a multiset of labels, possibly with a variable in the
        tail *)
    type t =
      | Open of Label.Multiset.Non_empty.t * Tail.t
      | Closed of Label.Multiset.t
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T

  let total =
    let labels = Label.Multiset.empty in
    Closed labels
  ;;

  let extend t l =
    match t with
    | Open (labels, tail) ->
      let labels = Label.Multiset.Non_empty.add labels l in
      Open (labels, tail)
    | Closed labels -> Label.Multiset.add labels l |> Closed
  ;;

  let closed_singleton l = Closed (Label.Multiset.of_list [ l ])

  let is_open = function
    | Open _ -> true
    | Closed _ -> false
  ;;

  let metavariables = function
    | Open (_, tail) -> Tail.metavariables tail
    | Closed _ -> Metavariable.Set.empty
  ;;

  let instantiate_as row ~var_to_meta =
    match row with
    | Open (labels, tail) ->
      let tail = Tail.instantiate_as ~var_to_meta tail in
      Open (labels, tail)
    | Closed labels -> Closed labels
  ;;

  let is_total = function
    (* open rows must have at least one label *)
    | Open (_, _) -> false
    | Closed labels -> Label.Multiset.is_empty labels
  ;;

  let labels = function
    | Open (labels, _) -> Label.Multiset.Non_empty.to_multiset labels
    | Closed labels -> labels
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

let instantiate_as t ~var_to_meta =
  match t with
  | Variable v ->
    (match Map.find var_to_meta v with
    | Some m -> Metavariable m
    | None -> Variable v)
  | Metavariable a -> Metavariable a
  | Row r -> Row (Row.instantiate_as r ~var_to_meta)
;;

let total = Row Row.total

let is_total = function
  | Variable _ | Metavariable _ -> None
  | Row r -> Row.is_total r |> Some
;;

let cons_row ~labels ~effect_ =
  match effect_ with
  | Metavariable v ->
    let tail = Row.Tail.Metavariable v in
    Row.Open (labels, tail)
  | Variable v ->
    let tail = Row.Tail.Variable v in
    Row.Open (labels, tail)
  (* if the tail itself is a row, flatten into a single row *)
  | Row r ->
    (match r with
    | Row.Closed labels' ->
      let labels'' =
        Label.Multiset.union
          (Label.Multiset.Non_empty.to_multiset labels)
          labels'
      in
      Row.Closed labels''
    | Row.Open (labels', tail) ->
      let labels'' = Label.Multiset.Non_empty.union labels labels' in
      Row.Open (labels'', tail))
;;

let of_row_tail = function
  | Row.Tail.Metavariable v -> Metavariable v
  | Row.Tail.Variable v -> Variable v
;;

let row_subtract row labels =
  match row with
  | Row.Open (row_labels, tail) ->
    let row_labels = Label.Multiset.Non_empty.to_multiset row_labels in
    let diff = Label.Multiset.diff row_labels labels in
    (match Label.Multiset.Non_empty.of_multiset_verbose diff with
    | `Empty -> of_row_tail tail
    | `Non_empty row_labels' -> Row.Open (row_labels', tail) |> Row)
  | Row.Closed row_labels ->
    let row_labels' = Label.Multiset.diff row_labels labels in
    Row.Closed row_labels' |> Row
;;
