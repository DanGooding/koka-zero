open Core
open Koka_zero_util

module Variable = struct
  module T = String
  include T
  include Name_source.Make (T)
end

module Metavariable = struct
  module T = String
  include T
  include Name_source.Make (T)
end

module Label = struct
  module T : Identifiable.S = String
  include T
  module Multiset = Multiset.Make (T)
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
    let { labels; _ } = t in
    let labels = Label.Multiset.add labels l in
    { t with labels }
  ;;

  let closed_singleton l = extend total l
  let is_open { tail; _ } = Option.is_some tail

  let metavariables { tail; _ } =
    match tail with
    | Some tail -> Tail.metavariables tail
    | None -> Metavariable.Set.empty
  ;;

  let instantiate_as row ~var_to_meta =
    let { tail; _ } = row in
    let tail = Option.map tail ~f:(Tail.instantiate_as ~var_to_meta) in
    { row with tail }
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
  | Row r -> Row.is_total r
;;

let cons_row ~labels ~effect =
  match effect with
  | Metavariable v ->
    let tail = Some (Row.Tail.Metavariable v) in
    { Row.labels; tail }
  | Variable v ->
    let tail = Some (Row.Tail.Variable v) in
    { Row.labels; tail }
  (* if the tail itself is a row, flatten into a single row *)
  | Row { Row.labels = labels'; tail } ->
    let labels = Label.Multiset.union labels labels' in
    { Row.labels; tail }
;;
