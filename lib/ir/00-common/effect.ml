open! Core
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

module Label = struct
  module T = String
  include T

  module Set = struct
    include Set
    include Set.Provide_hash (T)
  end
end

type t =
  | Metavariable of Metavariable.t
  | Labels of Label.Set.t
  | Handled of Set.M(Label).t * Metavariable.t
[@@deriving sexp_of, compare, hash]

let max_level t ~metavariable_level : int =
  match t with
  | Metavariable meta | Handled (_, meta) -> metavariable_level meta
  | Labels _ -> 0
;;

let node_id t =
  [%sexp (t : t)] |> Sexp.to_string_mach |> Dot_graph.Node_id.of_string
;;

let node_label t =
  let labels_to_string labels =
    Set.to_list labels |> List.map ~f:Label.to_string |> String.concat ~sep:","
  in
  match t with
  | Metavariable meta -> Metavariable.to_string meta
  | Labels labels -> [%string "<%{labels_to_string labels}>"]
  | Handled (labels, _) -> [%string "handled <%{labels_to_string labels}>"]
;;

let rec add_tree_to_graph t graph =
  let parent_id = node_id t in
  let shape =
    match t with
    | Metavariable _ -> []
    | Labels _ | Handled _ -> [ "shape", "box" ]
  in
  Dot_graph.add_node graph parent_id ~attrs:([ "label", node_label t ] @ shape);
  (* add edges to child nodes *)
  match t with
  | Labels _ | Metavariable _ -> ()
  | Handled (_labels, meta) ->
    let child = Metavariable meta in
    add_tree_to_graph child graph;
    Dot_graph.add_edge
      graph
      ~from:parent_id
      ~to_:(node_id child)
      ~disambiguator:(Dot_graph.Edge_disambiguator.of_string "original")
      ~attrs:[ "label", node_label child; "style", "dashed" ]
;;
