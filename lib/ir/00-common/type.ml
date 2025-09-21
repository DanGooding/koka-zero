open Core
open! Import

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

module Primitive = struct
  type t =
    | Int
    | Bool
  [@@deriving equal, compare, sexp_of, hash]

  let node_label t =
    match t with
    | Int -> "int"
    | Bool -> "bool"
  ;;
end

module Mono = struct
  type t =
    | Arrow of t list * Effect.t * t
    | Metavariable of Metavariable.t
    | Primitive of Primitive.t
    | List of t
    | Tuple of t list
  [@@deriving sexp_of, compare, hash]

  let rec max_level t ~type_metavariable_level ~effect_metavariable_level =
    match t with
    | Primitive _ -> 0
    | Metavariable meta -> type_metavariable_level meta
    | List t -> max_level t ~type_metavariable_level ~effect_metavariable_level
    | Tuple ts ->
      List.map
        ts
        ~f:(max_level ~type_metavariable_level ~effect_metavariable_level)
      |> List.max_elt ~compare:[%compare: int]
      |> Option.value ~default:0
    | Arrow (args, effect_, result) ->
      let type_levels =
        List.map
          (result :: args)
          ~f:(max_level ~type_metavariable_level ~effect_metavariable_level)
      in
      let effect_level =
        Effect.max_level effect_ ~metavariable_level:effect_metavariable_level
      in
      List.max_elt (effect_level :: type_levels) ~compare:[%compare: int]
      |> Option.value ~default:0
  ;;

  let node_id t =
    [%sexp (t : t)] |> Sexp.to_string_mach |> Dot_graph.Node_id.of_string
  ;;

  let node_label t =
    match t with
    | Arrow _ -> "Arrow"
    | List _ -> "List"
    | Tuple _ -> "Tuple"
    | Metavariable meta -> Metavariable.to_string meta
    | Primitive p -> Primitive.node_label p
  ;;

  let node_children t : ([ `Type of t | `Effect of Effect.t ] * string) list =
    match t with
    | Primitive _ | Metavariable _ -> []
    | List element -> [ `Type element, "element" ]
    | Tuple element_types ->
      List.mapi element_types ~f:(fun i element ->
        `Type element, [%string "[%{i#Int}]"])
    | Arrow (args, effect_, result) ->
      List.mapi args ~f:(fun i arg ->
        let label = [%string "arg%{i#Int}"] in
        `Type arg, label)
      @ [ `Effect effect_, "effect" ]
      @ [ `Type result, "result" ]
  ;;

  let rec add_tree_to_graph t graph =
    let parent_id = node_id t in
    let shape =
      match t with
      | Metavariable _ -> []
      | Primitive _ | Arrow _ | List _ | Tuple _ -> [ "shape", "box" ]
    in
    Dot_graph.add_node graph parent_id ~attrs:([ "label", node_label t ] @ shape);
    let children = node_children t in
    List.iter children ~f:(fun (child, label) ->
      (match child with
       | `Type child -> add_tree_to_graph child graph
       | `Effect child -> Effect.add_tree_to_graph child graph);
      let child_id =
        match child with
        | `Type child -> node_id child
        | `Effect child -> Effect.node_id child
      in
      Dot_graph.add_edge
        graph
        ~from:parent_id
        ~to_:child_id
        ~disambiguator:(Dot_graph.Edge_disambiguator.of_string label)
        ~attrs:[ "label", label; "style", "dashed" ])
  ;;
end

module Poly = struct
  type t =
    { forall_bound : Metavariable.t -> bool
    ; forall_bound_effects : Effect.Metavariable.t -> bool
    ; monotype : Mono.t
    }
  [@@deriving sexp_of]
end

type t =
  | Mono of Mono.t
  | Poly of Poly.t
[@@deriving sexp_of]

let generalise
      (monotype : Mono.t)
      ~should_generalise_type_metavariable
      ~should_generalise_effect_metavariable
  : Poly.t
  =
  { monotype
  ; forall_bound = should_generalise_type_metavariable
  ; forall_bound_effects = should_generalise_effect_metavariable
  }
;;
