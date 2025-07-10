open! Core
open! Import

module Node_id = struct
  include String

  let to_node_id t : Odot.node_id =
    let port = None in
    Double_quoted_id t, port
  ;;

  let to_stmt t : Odot.stmt =
    let id = to_node_id t in
    let attrs = [] in
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

  let to_stmt t : Odot.stmt =
    let from_id = Node_id.to_node_id t.from in
    let to_id = Node_id.to_node_id t.to_ in
    let attrs = [] in
    Stmt_edge (Edge_node_id from_id, [ Edge_node_id to_id ], attrs)
  ;;
end

type t =
  { nodes : Node_id.Hash_set.t
  ; edges : Edge_id.Hash_set.t
  }
[@@deriving sexp_of]

let create () =
  let nodes = Node_id.Hash_set.create () in
  let edges = Edge_id.Hash_set.create () in
  { nodes; edges }
;;

let add_node t node_id = Hash_set.add t.nodes node_id

let add_edge ?disambiguator t ~from ~to_ =
  let edge_id = Edge_id.create ?disambiguator () ~from ~to_ in
  Hash_set.add t.edges edge_id
;;

let to_graph t : Odot.graph =
  let nodes = Hash_set.to_list t.nodes |> List.map ~f:Node_id.to_stmt in
  let edges = Hash_set.to_list t.edges |> List.map ~f:Edge_id.to_stmt in
  { strict = false; kind = Digraph; id = None; stmt_list = nodes @ edges }
;;

let write t out_channel =
  let graph = to_graph t in
  Odot.print out_channel graph;
  Out_channel.newline out_channel
;;
