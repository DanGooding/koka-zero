open! Core
open! Import
open Evidence_passing_syntax

(* For any instance `Construct_pure expr` push the constructor inside any control flow/let-bindings etc. *)
val sink_pure : Expr.t -> Expr.t Modified.t
