open! Core
open! Import

module Attrs = struct
  type t = (string * string) list [@@deriving sexp_of]

  let empty = []

  let to_attrs (t : t) : Odot.attr list =
    List.map t ~f:(fun (key, value) ->
      Odot.Double_quoted_id key, Some (Odot.Double_quoted_id value))
  ;;
end

module Node_id = struct
  include String

  let to_node_id t : Odot.node_id =
    let port = None in
    Double_quoted_id t, port
  ;;

  let node_stmt t ~(attrs : Attrs.t) : Odot.stmt =
    let id = to_node_id t in
    let attrs = Attrs.to_attrs attrs in
    Stmt_node (id, attrs)
  ;;
end

module Edge_disambiguator = String

module Edge_id = struct
  module T = struct
    type t =
      { from : Node_id.t
      ; to_ : Node_id.t
      ; disambiguator : Edge_disambiguator.t option
      }
    [@@deriving sexp_of, compare, hash]
  end

  include T
  include Hashable.Make_plain (T)

  let create ?disambiguator () ~from ~to_ = { from; to_; disambiguator }

  let edge_stmt (id : t) ~attrs : Odot.stmt =
    let from_id = Node_id.to_node_id id.from in
    let to_id = Node_id.to_node_id id.to_ in
    let attrs = Attrs.to_attrs attrs in
    Stmt_edge (Edge_node_id from_id, [ Edge_node_id to_id ], attrs)
  ;;
end

type t =
  { nodes : Attrs.t Node_id.Table.t
  ; edges : Attrs.t Edge_id.Table.t
  }
[@@deriving sexp_of]

let create () =
  let nodes = Node_id.Table.create () in
  let edges = Edge_id.Table.create () in
  { nodes; edges }
;;

let add_node ?(attrs = Attrs.empty) t node_id =
  Hashtbl.set t.nodes ~key:node_id ~data:attrs
;;

let add_edge ?disambiguator ?(attrs = Attrs.empty) t ~from ~to_ =
  let edge_id = Edge_id.create ?disambiguator () ~from ~to_ in
  Hashtbl.set t.edges ~key:edge_id ~data:attrs
;;

let to_graph t : Odot.graph =
  let nodes =
    Hashtbl.to_alist t.nodes
    |> List.map ~f:(fun (node_id, attrs) -> Node_id.node_stmt node_id ~attrs)
  in
  let edges =
    Hashtbl.to_alist t.edges
    |> List.map ~f:(fun (edge_id, attrs) -> Edge_id.edge_stmt edge_id ~attrs)
  in
  { strict = false; kind = Digraph; id = None; stmt_list = nodes @ edges }
;;

let write t out_channel =
  let graph = to_graph t in
  Odot.print out_channel graph;
  Out_channel.newline out_channel
;;
